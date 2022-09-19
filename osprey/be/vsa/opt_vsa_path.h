//-*-c++-*-

/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

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

// ====================================================================
// ====================================================================
//
// Module: opt_vsa_path.h
//
// Path information to track the VSA issue
//
// ====================================================================
//


#ifndef opt_vsa_path_INCLUDED
#define opt_vsa_path_INCLUDED        "opt_vsa_path.h"

#ifdef _KEEP_RCS_ID
static char *opt_vsa_pathrcs_id =         opt_vsa_path_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "opt_defs.h"
#include "srcpos.h"
#include "cxx_memory.h"
#include "whirl_file_mgr.h"
#include "opt_vsa_report.h"
#include "opt_vsa_util.h"
#include "config_vsa.h"
#include <ext/hash_set>

using __gnu_cxx::hash_set;

class BB_NODE;
class COMP_UNIT;
class DNA_NODE;
class IPSA;
class RNA_NODE;
class STMTREP;
class VALUE_GRAPH;
class VSA;
class VSYM_OBJ_REP;

#define VSA_VAR_NAME_MAX_LEN 128

enum PATH_POSSIBLE_RES
{
  PATH_NOT_REACHABLE,    // path is not reachable
  PATH_REACHABLE,        // path is reachable
  PATH_MAY_REACHABLE,    // path may reachable
};

enum PATH_INFO {
  // path_info_msg[] in file need to update according to this enum
  PATHINFO_NONE            = 0,
  PATHINFO_COPY            = 1,
  PATHINFO_DNA_CALLSITE    = 2,
  PATHINFO_VUL_SPOT        = 3,
  PATHINFO_ST_DECLARE      = 4,
  PATHINFO_CALL_CHI        = 5,
  PATHINFO_CHI             = 6,
  PATHINFO_PHI             = 7,
  PATHINFO_ISTORE          = 8,
  PATHINFO_ALLOC           = 9,
  PATHINFO_FREE            = 10,
  PATHINFO_CD_BB           = 11,
  PATHINFO_FUN_EXIT        = 12,
  PATHINFO_INL_BEGIN       = 13,
  PATHINFO_INL_END         = 14,
  PATHINFO_PARM            = 15,
  PATHINFO_RBC             = 16,
  PATHINFO_BRANCH          = 17,
  PATHINFO_DNA_CALLRETURN  = 18,
  PATHINFO_EH_THROW        = 19,
  PATHINFO_EH_CATCH        = 20,
  PATHINFO_TRANSIT         = 21,
  PATHINFO_LDA             = 22,
  PATHINFO_COND_TRUE       = 23,
  PATHINFO_COND_FALSE      = 24,
  PATHINFO_THEN_TAKEN      = 25,
  PATHINFO_ELSE_TAKEN      = 26,
  PATHINFO_COPY_PROP       = 27,
  PATHINFO_DUMMY           = 28,
  PATHINFO_VUL_SPOT_SO     = 29,
  PATHINFO_MAX             = 30,
};

// =============================================================================
//
// struct SRCPOS_NODE, a lightway struct to save SRCPOS with the inline contex
// BB id can be used to get inline context when composing path string 
// 
// =============================================================================
class SRCPOS_NODE {
private:
  union {
    SRCPOS   _srcpos; // for PATHINFO_VUL_SPOT_SO
    ST      *_st;     // for PATHINFO_ST_DECLARE
    BB_NODE *_bb;     // for PATHINFO_PHI/PATHINFO_BRANCH
    INLCXT  *_inlcxt; // for PATHINFO_COPY_PROP
    STMTREP *_stmt;   // for the rest
  } _u1;
  DNA_NODE  *_dna;
  INT32      _info;

private:
  BOOL Is_srcpos() const
  {
    return _info == PATHINFO_VUL_SPOT_SO;
  }

  BOOL Is_st() const
  {
    return _info == PATHINFO_ST_DECLARE;
  }

  BOOL Is_inlcxt() const
  {
    return _info == PATHINFO_COPY_PROP;
  }

  BOOL Is_bb() const
  {
    return _info == PATHINFO_PHI || _info == PATHINFO_BRANCH ||
           _info == PATHINFO_THEN_TAKEN || _info == PATHINFO_ELSE_TAKEN;
  }

  BOOL Is_bb_first_linenum() const
  {
    return _info == PATHINFO_PHI ||
           _info == PATHINFO_THEN_TAKEN || _info == PATHINFO_ELSE_TAKEN;
  }

  BOOL Is_bb_last_linenum() const
  {
    return _info == PATHINFO_BRANCH;
  }

  BOOL Is_stmt() const
  {
    return !Is_srcpos() && !Is_st() && !Is_bb();
  }

public:
  SRCPOS_NODE(SRCPOS spos, DNA_NODE* dna, INT32 info)
    : _dna(dna), _info(info)
  {
    Is_True(Is_srcpos(), ("invalid info"));
    _u1._srcpos = spos;
  }
  SRCPOS_NODE(ST *st, DNA_NODE* dna, INT32 info)
    : _dna(dna), _info(info)
  {
    Is_True(Is_st(), ("invalid info"));
    _u1._st = st;
  }
  SRCPOS_NODE(BB_NODE* bb, DNA_NODE* dna, INT32 info)
    : _dna(dna), _info(info)
  {
    Is_True(Is_bb(), ("invalid info"));
    _u1._bb = bb;
  }
  SRCPOS_NODE(STMTREP* stmt, DNA_NODE* dna, INT32 info)
    : _dna(dna), _info(info)
  {
    Is_True(Is_stmt(), ("invalid info"));
    _u1._stmt = stmt;
  }
  SRCPOS_NODE(INLCXT* inlcxt, DNA_NODE* dna, INT32 info)
    : _dna(dna), _info(info)
  {
    Is_True(Is_inlcxt(), ("invalid info"));
    _u1._inlcxt = inlcxt;
  }
  SRCPOS_NODE(DNA_NODE* dna)
    : _dna(dna), _info(PATHINFO_NONE)
  {
    _u1._st = NULL;
  }
  SRCPOS_NODE(void)
    : _dna(NULL), _info(PATHINFO_NONE)
  {
    _u1._st = NULL;
  }

  void Set_stmt(STMTREP* stmt, INT32 info = PATHINFO_NONE)
  {
    if (info != PATHINFO_NONE)
      _info = info;
    Is_True(Is_stmt(), ("invalid info"));
    _u1._stmt = stmt;
  }
  void Set_bb(BB_NODE* bb, INT32 info = PATHINFO_NONE)
  {
    if (info != PATHINFO_NONE)
      _info = info;
    Is_True(Is_bb(), ("invalid info"));
    _u1._bb = bb;
  }
  void Set_inlcxt(INLCXT* cxt, INT32 info = PATHINFO_NONE)
  {
    if (info != PATHINFO_NONE)
      _info = info;
    Is_True(Is_inlcxt(), ("invalid info"));
    _u1._inlcxt = cxt;
  }

  BOOL equal(const SRCPOS_NODE& n) const
  {
    return equal(&n);
  }
  BOOL equal(const SRCPOS_NODE* n) const
  {
    return _info == n->_info && _dna == n->_dna &&
           Spos() == n->Spos() && _info != PATHINFO_TRANSIT;
  }

public:
  SRCPOS Spos() const;
  UINT File_idx() const;
  INLCXT *Inlcxt() const;
  const char* Fname() const;
  const char* Vname() const;

  ST*      St() const
  {
    Is_True(Is_st(), ("invalid info"));
    return _u1._st;
  }
  STMTREP* Stmt() const
  {
    Is_True(Is_stmt(), ("invalid info"));
    return _u1._stmt;
  }
  BB_NODE* Bb() const
  {
    Is_True(Is_bb(), ("invalid info"));
    return _u1._bb;
  }

  INT32 Info() const     { return _info; }
  void Set_info(INT32 i) { _info = i; }

  void Set_dna(DNA_NODE *dna){ _dna = dna; }
  DNA_NODE* Dna() const      { return _dna; }

  SRCPOS Transpos() const    { return Transpos(Spos(), File_idx()); }

  void Clean()
  {
    _u1._st = NULL;
    _dna = NULL;
    _info = PATHINFO_NONE;
  }

public:
  // translate filenum in spos from local to global and return new SRCPOS
  static SRCPOS Transpos(SRCPOS spos, UINT file_idx)
  {
    if (spos == 0 || file_idx == 0)
      return spos;
    WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
    Is_True(mgr != NULL, ("file_idx not 0 in single file mode"));
    WHIRL_FILE_INFO& info = mgr->Get_file(file_idx);
    if (FILE_INFO_is_rbc(info.Whirl_file_info()))
      return 0;
    USRCPOS uspos;
    USRCPOS_srcpos(uspos) = spos;
    USRCPOS_filenum(uspos) = info.Get_global_file_num(SRCPOS_filenum(spos));
    return USRCPOS_srcpos(uspos);
  }
};


typedef std::vector<SRCPOS_NODE> SRCPOS_NODES;

// =============================================================================
//
// PATH_SELECTED
//
// =============================================================================
class PATH_SELECTED {
private:
  typedef hash_set<uint32_t> U32_SET;
  typedef hash_set<uint64_t> U64_SET;
  typedef hash_map<uint32_t, U64_SET>  PATH_MAP;

  U32_SET  _rna_selected;
  PATH_MAP _phi_selected;

  PATH_SELECTED(const PATH_SELECTED& path);
  PATH_SELECTED& operator = (const PATH_SELECTED& path);

public:
  void Add_rna(IDTYPE rna_idx) {
    _rna_selected.insert(rna_idx);
  }

  void Add_phi(IDTYPE dna_idx, IDTYPE from, IDTYPE to) {
    PATH_MAP::iterator it = _phi_selected.find(dna_idx);
    if (it == _phi_selected.end()) {
      std::pair<PATH_MAP::iterator, bool> ret;
      ret = _phi_selected.insert(make_pair(dna_idx, U64_SET()));
      Is_True(ret.second == true, ("failed to insert into PATH_MAP"));
      it = ret.first;
    }
    U64_SET& paths = it->second;
    paths.insert((uint64_t)from);
    uint64_t path = (uint64_t)to << 32 | (uint64_t)from;
    paths.insert(path);
  }

public:
  PATH_SELECTED() {}

  PATH_SELECTED(const PATH_SELECTED& path, IDTYPE dna_idx, IDTYPE from, IDTYPE to)
   : _rna_selected(path._rna_selected),
     _phi_selected(path._phi_selected)
  {
    Add_phi(dna_idx, from, to);
  }

  BOOL Selected(IDTYPE dna_idx, IDTYPE from) const
  {
    PATH_MAP::const_iterator it = _phi_selected.find(dna_idx);
    if (it == _phi_selected.end())
      return FALSE;
    const U64_SET& paths = it->second;
    if (paths.find((uint64_t)from) == paths.end())
      return FALSE;
    return TRUE;
  }

  BOOL Selected(IDTYPE dna_idx, IDTYPE from, IDTYPE to) const
  {
    PATH_MAP::const_iterator it = _phi_selected.find(dna_idx);
    if (it == _phi_selected.end())
      return FALSE;
    const U64_SET& paths = it->second;
    uint64_t path = (uint64_t)to << 32 | (uint64_t)from;
    if (paths.find(path) == paths.end())
      return FALSE;
    return TRUE;
  }

  BOOL Selected(IDTYPE rna_idx) const
  {
    if (_rna_selected.find(rna_idx) == _rna_selected.end())
      return FALSE;
    return TRUE;
  }

  void Print(FILE* fp = stdout) const;
};

// =============================================================================
//
// SRCPOS_TREENODE, SRCPOS_HANDLE, SRCPOS_TR_ITER allows error classifier to
// append SRCPOS_NODE while it traverse the program. TREENODE allows the handle to
// fork when multiple path are needed.
//
// =============================================================================
enum SRCPOS_FLAG {
  SRCPOS_FLAG_NONE     = 0x0,    // no flag
  SRCPOS_FLAG_MAYBE    = 0x1,    // this node maybe takes. if not set, means definitely take
  SRCPOS_FLAG_PHI_OPND = 0x2,    // this node is created for phi opnd
  SRCPOS_FLAG_CALLER   = 0x4,    // this node is created for caller
  SRCPOS_FLAG_CALLEE   = 0x8,    // this node is created for callee
};

class SRCPOS_CTRLDEP {
private:
  COMP_UNIT *_cu;
  BB_NODE   *_pred;
  BB_NODE   *_succ;

public:
  SRCPOS_CTRLDEP(COMP_UNIT* cu, BB_NODE* pred, BB_NODE* succ)
   : _cu(cu), _pred(pred), _succ(succ) { }

  COMP_UNIT *Comp_unit() const { return _cu;        }
  DNA_NODE  *Dna() const       { return _cu->Dna(); }
  VSA       *Vsa() const       { return _cu->Vsa(); }
  BB_NODE   *Pred() const      { return _pred;      }
  BB_NODE   *Succ() const      { return _succ;      }
};

class SRCPOS_TREENODE {
  typedef mempool_allocator<SRCPOS_NODE> SRCPOS_ALLOCATOR;
  typedef vector<SRCPOS_NODE, SRCPOS_ALLOCATOR> SRCPOS_NODE_VECTOR;
  typedef mempool_allocator<SRCPOS_TREENODE*> SNODE_ALLOCATOR;
  typedef vector<SRCPOS_TREENODE*, SNODE_ALLOCATOR> SNODE_VECTOR;
  typedef mempool_allocator<SRCPOS_CTRLDEP> CTRLDEP_ALLOCATOR;
  typedef vector<SRCPOS_CTRLDEP, CTRLDEP_ALLOCATOR> CTRLDEP_VECTOR;

private:
  SRCPOS_NODE_VECTOR     _data;
  SRCPOS_TREENODE *_parent;
  SNODE_VECTOR     _children;
  CTRLDEP_VECTOR   _cds;                   // control dependencies
  MEM_POOL        *_mem_pool;
  DNA_NODE        *_dna;
  UINT32           _flags;
  UINT32           _par_count;             // node count from root to parent node

  SRCPOS_TREENODE(void);                   // REQUIRED UNDEFINED UNWANTED methods
  SRCPOS_TREENODE(const SRCPOS_TREENODE&); // REQUIRED UNDEFINED UNWANTED methods
  SRCPOS_TREENODE& operator = (const SRCPOS_TREENODE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  SRCPOS_TREENODE(SRCPOS_TREENODE* parent, DNA_NODE* dna, MEM_POOL *mem_pool) :
    _parent(parent),
    _mem_pool(mem_pool),
    _dna(dna),
    _children(0, (SRCPOS_TREENODE*)NULL, SNODE_VECTOR::allocator_type(mem_pool)),
    _data(0, SRCPOS_NODE(), SRCPOS_NODE_VECTOR::allocator_type(mem_pool)),
    _cds(SRCPOS_NODE_VECTOR::allocator_type(mem_pool)),
    _flags(SRCPOS_FLAG_NONE)
  {
    _par_count = parent ? parent->Get_data_cnt() : 0;
  }
  ~SRCPOS_TREENODE() { }

  DNA_NODE              *Dna() const                           { return _dna;        }
  void                   Set_dna(DNA_NODE* dna)                { _dna = dna;         }
  CTRLDEP_VECTOR        *Control_dependency()                  { return &_cds;       }
  SRCPOS_CTRLDEP         Get_control_dependency(std::size_t idx) const
  {
    return _cds[idx];
  }
  void                   Append_control_dependency(COMP_UNIT* cu, BB_NODE* pred, BB_NODE* succ)
  {
    _cds.push_back(SRCPOS_CTRLDEP(cu, pred, succ));
  }
  BOOL                   Find_control_dependency(COMP_UNIT* cu, BB_NODE* pred, BB_NODE* succ) const
  {
    for (CTRLDEP_VECTOR::const_iterator it = _cds.begin(); it != _cds.end(); ++it) {
      if (it->Comp_unit() == cu && it->Pred() == pred && it->Succ() == succ)
        return TRUE;
    }
    return FALSE;
  }
  SRCPOS_NODE_VECTOR    *Data(void)                            { return &_data;     }
  SRCPOS_NODE            Get_data(const std::size_t idx)const  { return _data[idx]; }
  void                   Append_data(SRCPOS_NODE  data)   { _data.push_back(data);  }
  void                   Append_data(SRCPOS spos, DNA_NODE* dna, INT32 info)
  {
    _data.push_back(SRCPOS_NODE(spos, dna, info));
  }
  void                   Append_data(ST* st, DNA_NODE* dna, INT32 info)
  {
    _data.push_back(SRCPOS_NODE(st, dna, info));
  }
  void                   Append_data(BB_NODE* bb, DNA_NODE* dna, INT32 info)
  {
    _data.push_back(SRCPOS_NODE(bb, dna, info));
  }
  void                   Append_data(STMTREP* stmt, DNA_NODE* dna, INT32 info)
  {
    SRCPOS_NODE tmp(stmt, dna, info);
    // skip same spos
    if(!_data.empty() && _data.back().equal(tmp)) {
      return;
    }
    _data.push_back(tmp);
  }
  void                   Remove_last(void) { _data.pop_back() ; }
  SRCPOS_TREENODE *Add_child(void)
  {
    Is_True(this, ("call SRCPOS_TREENODE::Add_child with NULL pointer"));
    SRCPOS_TREENODE *retv = CXX_NEW(SRCPOS_TREENODE(this, _dna, _mem_pool), _mem_pool);
    retv->_flags = _flags;    // copy flags to child
    _children.push_back(retv);
    return retv;
  }
  void             Add_children(INT cnt)
  {
    Is_True(this, ("call SRCPOS_TREENODE::Add_children with NULL pointer"));
    _children.clear();
    for (; cnt > 0; --cnt) {
      SRCPOS_TREENODE *retv = CXX_NEW(SRCPOS_TREENODE(this, _dna, _mem_pool), _mem_pool);
      retv->_flags = _flags;
      _children.push_back(retv);
    }
  }
  SRCPOS_TREENODE* Parent(void) const      { return _parent;  }
  SRCPOS_TREENODE* Get_child(const std::size_t& idx) const { 
    Is_True(Get_child_cnt() > idx, ("SRCPOS_TREENODE::Get_child for non-exist child"));
    return _children[idx];
  }
  UINT32           Get_data_cnt(void) const{ return _par_count + _data.size(); }
  INT              Get_child_cnt(void)const{ return (INT)_children.size();  }
  UINT32           Water_mark(void) const  { return _data.size(); }
  void             Set_water_mark(UINT32 s){ _data.resize(s);     }

  BOOL             Is_flag_set(SRCPOS_FLAG f) const { return (_flags&f)==f; }
  void             Set_flag(SRCPOS_FLAG f)    { _flags |= f; }
  void             Reset_flag(SRCPOS_FLAG f)  { _flags &= ~f; }

  void             Print(INT indent, FILE *fp=stderr) const;
};

class PHI_NODE;
class COMP_UNIT;

// =============================================================================
//
// SRCPOS_PATH
//
// Save key BB_NODE on the traversal path, include:
//   - Origin BB where the traversal starts
//   - PHI OPND BB where the phi opnd is taken
//   - CALLSITE BB where caller or callee entered
// =============================================================================
class SRCPOS_PATH {
private:
  enum NODE_TYPE {
    NODE_IS_PHI = 0,
    NODE_IS_CU  = 1,
    NODE_IS_BB  = 2,
  };

  typedef pair<BB_NODE*, NODE_TYPE>         PATH_NODE;
  typedef mempool_allocator<PATH_NODE>      PATH_ALLOCATOR;
  typedef vector<PATH_NODE, PATH_ALLOCATOR> PATH_VECTOR;

  COMP_UNIT*  _cu;
  PATH_VECTOR _path;

private:
  const char* Node_name(NODE_TYPE type)
  {
    static const char* name[] = { "phi", "cu", "bb" };
    return name[type];
  }

  void Push(BB_NODE* first, NODE_TYPE type)
  {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "PATH-PUSH: %p %s\n", first, Node_name(type)));
    PATH_NODE node = std::make_pair(first, type);
    _path.push_back(node);
  }

  void Pop(BB_NODE* first, BOOL pop_mark, NODE_TYPE type)
  {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "PATH-POP : %p %s %s\n", first, Node_name(type),
                     pop_mark ? "T" : "F"));
    PATH_NODE node = std::make_pair(first, type);
    while (!_path.empty() && _path.back() != node)
      _path.pop_back();
    if (pop_mark) {
      Is_True_Ret(!_path.empty() && _path.back() == node, ("not find curnode"));
      if (_path.back() == node)
        _path.pop_back();
    }
  }

public:
  SRCPOS_PATH(COMP_UNIT* cu, MEM_POOL* mp)
   : _cu(cu), _path(PATH_ALLOCATOR(mp))
  {
    _path.reserve(16);
  }

  void Push_mark(PHI_NODE* phi)
  {
    Push((BB_NODE*)phi, NODE_IS_PHI);
  }
  void Push_mark(COMP_UNIT* cu)
  {
    Push((BB_NODE*)cu, NODE_IS_CU);
    _cu = cu;
  }
  void Pop_mark(PHI_NODE* phi, BOOL pop_mark)
  {
    Pop((BB_NODE*)phi, pop_mark, NODE_IS_PHI);
  }
  void Pop_mark(COMP_UNIT* cu, BOOL pop_mark)
  {
    Pop((BB_NODE*)cu, pop_mark, NODE_IS_CU);
    _cu = cu;
  }

  void Add_bb(BB_NODE* bb)
  {
    Push(bb, NODE_IS_BB);
  }

  BB_NODE* Origin_bb() const
  {
    INT i;
    for (i = _path.size() - 1; i >= 0; --i) {
      if (_path[i].second == NODE_IS_CU) {
        Is_True(_path[i].first == (BB_NODE*)_cu, ("comp_unit mismatch"));
        break;
      }
    }
    if (i < 0)
      i = 0;
    while (i < _path.size() && _path[i].second != NODE_IS_BB)
      ++i;
    Is_True(i >= 0 && i < _path.size(), ("not find origin bb"));
    return _path[i].first;
  }

  PATH_POSSIBLE_RES Is_path_possible(PHI_NODE* phi, INT opnd) const;

  PATH_POSSIBLE_RES Is_def_reachable(BB_NODE *use_bb, STMTREP *def_sr) const;

  SRCPOS_PATH* Clone(MEM_POOL* mp) const
  {
    SRCPOS_PATH* path = CXX_NEW(SRCPOS_PATH(_cu, mp), mp);
    path->_path.reserve(_path.size());
    path->_path.assign(_path.begin(), _path.end());
    return path;
  }

};

// ============================================================================
// SRCPOSINFO
// ============================================================================
class SRCPOSINFO {
private:
  SRCPOS      _spos;          // srcpos, file id, line and column
  const char* _vname;         // variable name
  const char* _fname;         // function name
  INT32       _msg_id;        // message template id

public:
  // constructor
  SRCPOSINFO(SRCPOS spos, INT32 msg_id, const char* vn, const char* fn)
    : _spos(spos), _vname(vn), _fname(fn), _msg_id(msg_id)
  {
    Is_True(fn != NULL, ("fn is NULL"));
  }

  // get srcpos
  SRCPOS      Spos() const    { return _spos;   }
  // get variable name
  const char* Vname() const   { return _vname; }
  // get function name
  const char* Fname() const   { return _fname; }
  // get message template id
  INT32       Msg_id() const  { return _msg_id; }
};

typedef vector<SRCPOSINFO> SRCPOSINFO_VECTOR;

// =============================================================================
//
// Comments about "key" srcpos in SRCPOS_HANDLE:
//
// Each SRCPOS_HANDLE has a "key" srcpos, which indicates the "key" step causes
// the issue. VSA follow the following convention to set "key" srcpos so that
// different issues with same root cause can be grouped.
// By grouping by "key", the issues with the same "key" can be classified into
// 4 kinds:
//  - Single Source, Single Sink     (SSSS)
//  - Single Source, Multiple Sink   (SSMS)
//  - Multiple Source, Single Sink   (MSSS)
//  - Multiple Source, Multiple Sink (MSMS)
// For each kind:
//  - SSSS: add both source and sink into "key" srcpos.
//  - SSMS: add source into "key" srcpos
//  - MSSS: add sink into "key" srcpos
//  - MSMS: "key" is generated without srcpos info.
// So far only SSSS and SSMS are allowed. MSSS is converted into SSSS. No MSMS.
//  - SSMS: DBZ, NPD, UAF, UIV
//  - SSSS: the rest
// The API SRCPOS_HANDLE::Set_key_srcpos(DNA_NODE* dna, BB_NODE* bb, SRCPOS spos)
// should be called to set the "key" srcpos.
// AOB: add srcpos where the array base is assigned, index is assigned and array
//      element is accessed intoo "key" srcpos.
// DBF: add srcpos where the buffer is freed first time and second time into "key"
//      srcpos.
// UAF: add srcpos where the buffer is freed into "key" srcpos.
// UDR: add srcpos where the pointer is assigned and freed into "key" srcpos
// MSF: add srcpos where the buffer is allocated and end of function where the
//      buffer is inaccessible
// NPD: where the pointer is set to 0 or treated as 0 by value range. Call
//      Set_key_srcpos() on statement where pointer is set to 0. No change for
//      getting 0 from value range analysis. This will be done later.
// DBZ: where the variable is set to 0 or treated as 0 by value range. Call
//      Set_key_srcpos() on statement where divisor is set to 0. No change for
//      getting 0 from value range analysis. This will be done later.
// UIV: where the variable is declared. Call Set_key_srcpos() on line where the st
//      is declared
// Other issue categories are not changed
//
// The new key contains:
//   rule name, variable name, filename and line number for all "key" srcpos
//
// SRCPOS_HANDLE also keep the inlined function name for issue reporting. For
// example, after inline zero into foo, our previous report says NPD is in
// foo() and now NPD is in zero().
// static int zero() {
//   return *(NULL);
// }
// int foo() {
//     return zero();
// }
// =============================================================================
#define INVALID_INLCXT ((INLCXT*)-1)

class SRCPOS_HANDLE {
  typedef std::vector<SRCPOS> SRCPOS_VECTOR;
  struct KEY_SRCPOS {
    DNA_NODE *_dna;
    INLCXT   *_inlcxt;
    SRCPOS    _spos;
    const char *_vname;
    KEY_SRCPOS(DNA_NODE *dna, INLCXT *cxt, SRCPOS spos, const char *name)
      : _dna(dna), _inlcxt(cxt), _spos(spos), _vname(name) { }
  };
  typedef std::vector<KEY_SRCPOS,
                      mempool_allocator<KEY_SRCPOS> > KEY_SRCPOS_VEC;
  
private:
  STMTREP         *_root_stmt;             // stmt that triggers this handler
  CODEREP         *_root_x;                // expression that triggers this handler
  DNA_NODE        *_context;               // the context which init the handle
  SRCPOS_TREENODE *_root;                  // root node of the srcpos repo
  SRCPOS_TREENODE *_cur_node;              // where we are accumulating
  VALUE_GRAPH     *_graph;                 // graph to calculate value ranges and control dependencies
  SRCPOS_PATH     *_path;                  // BB to BB path for this handle
  IDTYPE           _cur_idx;               // index into the children of parent
  const char      *_orig_stname;           // if not NULL, is the original sym name before copy propagation
  const char      *_orig_puname;           // if not NULL, is the orginal pu name that triggers this handler
                                           // if in inline context, the inlined callee pu name is used
  char            *_message;               // if not NULL, emit this message to replace path_info_msg[3]
  char            *_msgid;                 // if not NULL, use the message template id in report
  BB_NODE         *_prev_bb;               // previous BB to calculate CD
  DNA_NODE        *_prev_dna;              // previous DNA
  MEM_POOL        *_mem_pool;
  KEY_SRCPOS_VEC  *_key_spos;              // srcposes where the "key" statement is, used to generate issue key
  UINT32           _children_count;        // total count of children added
  mutable UINT32   _complexity;            // complexity of the path
  BOOL             _forward;               // the spos order is forward
  
  SRCPOS_HANDLE(void);                     // REQUIRED UNDEFINED UNWANTED methods
  SRCPOS_HANDLE(const SRCPOS_HANDLE&);     // REQUIRED UNDEFINED UNWANTED methods
  SRCPOS_HANDLE& operator = (const SRCPOS_HANDLE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  SRCPOS_HANDLE(DNA_NODE* dna, MEM_POOL *pool);
  SRCPOS_HANDLE(CODEREP *x, STMTREP *stmt, DNA_NODE* dna, MEM_POOL *pool, VSA *vsa=NULL, CODEREP *orign = NULL);
  ~SRCPOS_HANDLE(void) { OPT_POOL_Pop(_mem_pool, VSA_DUMP_FLAG); }

  DNA_NODE        *Context(void) const        { return _context; }
  STMTREP         *Root_stmt(void) const      { return _root_stmt; }
  CODEREP         *Root_x(void) const         { return _root_x;   }
  SRCPOS           Root_linenum(void) const   { return (*_root->Data())[0].Transpos(); }
  SRCPOS_TREENODE *Root(void) const           { return _root;     }
  SRCPOS_TREENODE *Cur_node(void) const       { return _cur_node; }
  IDTYPE           Cur_idx(void) const        { return _cur_idx;  }
  SRCPOS_PATH     *Path(void) const           { return _path;     }
  const char      *Orig_stname(void) const    { return _orig_stname; }
  const char      *Orig_puname(void) const    { return _orig_puname; }
  const char      *Message(void) const        { return _msgid ? _msgid : _message; }
  UINT32           Complexity(void) const     { return _complexity;  }
  BOOL             Is_forward(void) const     { return _forward; }
  void             Set_forward(BOOL v)        { _forward = v; }
  const char      *Find_orig_stname(CODEREP *cr, STMTREP *stmt, DNA_NODE *dna, BOOL forced = FALSE, BOOL follow_ud = TRUE) const;
  const char      *Find_cr_stname(CODEREP *cr, STMTREP *stmt, DNA_NODE *dna,
                                  BOOL follow_ud = TRUE, BOOL gen_cr = FALSE, BOOL hex = FALSE) const
  {
    if (cr == NULL)
      return NULL;
    char *buf = (char*)MEM_POOL_Alloc(_mem_pool, VSA_VAR_NAME_MAX_LEN);
    STRING_BUFFER sbuf(buf, VSA_VAR_NAME_MAX_LEN);
    return Find_cr_stname(&sbuf, cr, stmt, dna, follow_ud, gen_cr, hex);
  }
  const char      *Gen_fld_stname(const char *st_name, TY_IDX st_ty, UINT16 field_id) const
  {
    char *buf = (char*)MEM_POOL_Alloc(_mem_pool, VSA_VAR_NAME_MAX_LEN);
    STRING_BUFFER sbuf(buf, VSA_VAR_NAME_MAX_LEN);
    return Gen_fld_stname(&sbuf, st_name, st_ty, field_id);
  }
  void             Set_orig_stname(DNA_NODE* dna, CODEREP* cr);
  void             Set_orig_stname(const char* name) { _orig_stname = name; }
  void             Set_orig_puname(const char* name) { _orig_puname = name; }
  void             Set_key_srcpos(DNA_NODE* dna, STMTREP* stmt, CODEREP *cr);
  void             Set_key_srcpos(DNA_NODE* dna, BB_NODE* bb, SRCPOS spos, const char* name)
  {
    Is_True(dna != NULL && spos != 0, ("invalid func or spos"));
    INLCXT *cxt = bb ? bb->Inlinecxt() : INVALID_INLCXT;
    _key_spos->push_back(KEY_SRCPOS(dna, cxt, spos, name));
  }
  void             Remove_last_key_srcpos()
  {
    if (!_key_spos->empty())
      _key_spos->pop_back();
  }
  SRCPOS_NODE      First_spos() const {
    Is_True(_root != NULL, ("srcpos_h root is NULL"));
    Is_True(_root->Data()->size() > 0, ("srcpos_h root is empty"));
    return _root->Data()->front();
  }
  SRCPOS_NODE      Last_spos() const {
    Is_True(_cur_node != NULL, ("srcpos_h cur node is NULL"));
    Is_True(_cur_node->Data()->size() > 0, ("srcpos_h cur node is empty"));
    return _cur_node->Data()->back();
  }
  void             Set_msgid(const char *msgid);
  void             Add_message(const char* fmt, ...);
  void             Set_to_root()              { _cur_node = _root; }
  void             Set_cur_node(SRCPOS_TREENODE *cn, IDTYPE i, BOOL reset = FALSE)  {
    _cur_node = cn->Get_child((std::size_t) i); _cur_idx = i;
    if (reset)
      _prev_bb = NULL;
  }
  void             Reset_cur_node(SRCPOS_TREENODE *cn, IDTYPE i) {
    _cur_node = cn; _cur_idx = i;
  }
  SRCPOS_TREENODE *Add_children(INT cnt)      { _cur_node->Add_children(cnt); _children_count += cnt; return _cur_node; }
  BOOL             Is_flag_set(SRCPOS_FLAG f) const { return _cur_node->Is_flag_set(f); }
  void             Set_flag(SRCPOS_FLAG f)       { _cur_node->Set_flag(f); }
  void             Reset_flag(SRCPOS_FLAG f)     { _cur_node->Reset_flag(f); }
  MEM_POOL        *Mem_pool(void) const       { return _mem_pool; }
  void             Append_stpath(STMTREP *stmt, CODEREP *cr, DNA_NODE* dna, BOOL force_set_orig_st = FALSE);
  void             Append_data(STMTREP *stmt, CODEREP *cr, DNA_NODE* dna, INT32 info) {
    Append_data(stmt, dna, info);
    Append_stpath(stmt, cr, dna, FALSE);
  }
  void             Append_data(SRCPOS_NODE  data)
  {
    _cur_node->Append_data(data);
  }
  BOOL             Find_control_dependency(COMP_UNIT* cu, BB_NODE* pred, BB_NODE* succ) const
  {
    DNA_NODE* dna = cu->Dna();
    SRCPOS_TREENODE* node = _cur_node;
    while (node != NULL && node->Dna() == dna) {
      if (node->Find_control_dependency(cu, pred, succ))
        return TRUE;
      node = node->Parent();
    }
    return FALSE;
  }
  UINT32           Children_count(void) const { return _children_count; }
  UINT32           Water_mark(void) const     { return _cur_node->Water_mark(); }
  void             Set_water_mark(UINT32 s)   { _cur_node->Set_water_mark(s);   }
  void             Append_data(SRCPOS spos, BB_NODE* bb, DNA_NODE* dna, INT32 info);
  void             Append_data(ST* st, BB_NODE* bb, DNA_NODE* dna, INT32 info);
  void             Append_data(BB_NODE* bb, DNA_NODE* dna, INT32 info);
  void             Append_data(STMTREP* stmt, DNA_NODE* dna, INT32 info, BOOL add_data = TRUE);
  void             Remove_last(void) { _cur_node->Remove_last(); }
  void             Append_control_dependency(BB_NODE* succ, BB_NODE* pred, DNA_NODE* dna, BOOL add_data);
  char            *Compose_path_string(void) const;
  void             Compose_path_vector(SRCPOSINFO_VECTOR*) const;
  void             Compose_path_selected(IPSA*, PATH_SELECTED*) const;
  void             Write_path_json(FILE *fp, SRCPOS master) const;
  const char      *Path_to_key(char* buf, INT len, const char* var,
                               const char* rname, const char* cname, IDTYPE& keyid) const;
  SRCPOS_HANDLE   *Clone(MEM_POOL *pool = NULL);
  static BOOL      Is_temp_var(const char *name);
  static BOOL      Is_temp_var(ST_IDX stidx);
  static
  const char      *Find_cr_stname(STRING_BUFFER *buf, CODEREP *cr, STMTREP *stmt, DNA_NODE *dna,
                                  BOOL follow_ud = TRUE, BOOL gen_cr = FALSE, BOOL hex = FALSE);
  static
  const char      *Gen_fld_stname(STRING_BUFFER *buf, const char *st_name, TY_IDX st_ty, UINT16 field_id);
  static
  const char      *Gen_cr_stname(STRING_BUFFER *buf, CODEREP *cr, STMTREP *stmt, DNA_NODE *dna, BOOL follow_ud, BOOL hex);
  BOOL             Reach_check_limit(void)
  {
    if ((VSA_Checker_Max_Srcpos_Child >=0 &&
         Children_count() >= VSA_Checker_Max_Srcpos_Child) ||
        (VSA_Checker_Max_Srcpos_Data >=0 &&
         Cur_node() && Cur_node()->Get_data_cnt() > VSA_Checker_Max_Srcpos_Data)) {
      return TRUE;
    }
    return FALSE;
  }

  INT              Push_value_graph();
  void             Pop_value_graph(INT level);
  BOOL             Add_assign(VSA* lvsa, CODEREP *lhs, VSA *rvsa, CODEREP *rhs);
  BOOL             Add_assign(VSA* lvsa, VSYM_OBJ_REP *lhs, VSA *rvsa, VSYM_OBJ_REP *rhs);
  BOOL             Add_control_dependency(DNA_NODE *dna, BB_NODE *pred, BB_NODE *succ);
  BOOL             Add_phi_opnd(DNA_NODE *dna, PHI_NODE *phi, INT32 opnd_idx);

  void             Print(FILE *fp=stderr)  const;
};

class SRCPOS_TR_ITER {

private:
  SRCPOS_TREENODE *_iter_node;
  std::size_t      _iter_idx;

  SRCPOS_TR_ITER(void);                    // REQUIRED UNDEFINED UNWANTED methods
  SRCPOS_TR_ITER(const SRCPOS_TR_ITER&);   // REQUIRED UNDEFINED UNWANTED methods
  SRCPOS_TR_ITER& operator = (const SRCPOS_TR_ITER&); // REQUIRED UNDEFINED UNWANTED methods

  SRCPOS_TREENODE *Iter_node(void) const   { return _iter_node; }
  void             Iterate_parent(void)    {
    _iter_node = _iter_node->Parent();
    _iter_idx  = _iter_node->Data()->size();
  }
  
  std::size_t      Iter_idx(void) const    { return _iter_idx; }
  void             Dec_iter_idx(void)      { _iter_idx--; }

public:
  SRCPOS_TR_ITER(const SRCPOS_HANDLE* sp_h){
    _iter_node = sp_h->Cur_node();
    _iter_idx  = _iter_node->Data()->size();
  }
  ~SRCPOS_TR_ITER(void) { }
  BOOL             Is_empty(void) const    {
    return (Iter_idx() == 0 && Iter_node()->Parent() == NULL);
  }

  SRCPOS_NODE           Next(void)          {
    if (Iter_idx() == 0) Iterate_parent();
    Is_True(Iter_idx() != 0, ("empty srcpos node"));
    Dec_iter_idx();
    return Iter_node()->Get_data(_iter_idx);
  }
};

class SRCPOS_CD_ITER {
private:
  SRCPOS_TREENODE* _iter_node;
  std::size_t      _iter_idx;

  SRCPOS_CD_ITER(const SRCPOS_CD_ITER&);   // REQUIRED UNDEFINED UNWANTED methods
  SRCPOS_CD_ITER& operator = (const SRCPOS_CD_ITER&); // REQUIRED UNDEFINED UNWANTED methods

  SRCPOS_TREENODE *Iter_node(void) const   { return _iter_node; }
  void             Iterate_parent(void)    {
    _iter_node = _iter_node->Parent();
    _iter_idx  = _iter_node->Control_dependency()->size();
  }

  std::size_t      Iter_idx(void) const    { return _iter_idx;  }
  void             Dec_iter_idx(void)      { _iter_idx--; }

public:
  SRCPOS_CD_ITER(const SRCPOS_HANDLE* sp_h){
    _iter_node = sp_h->Cur_node();
    _iter_idx  = _iter_node->Control_dependency()->size();
  }
  BOOL             Is_empty(void)          {
    while (_iter_idx == 0 && _iter_node->Parent() != NULL) {
      _iter_node = _iter_node->Parent();
      _iter_idx = _iter_node->Control_dependency()->size();
    }
    return _iter_idx == 0;
  }
  SRCPOS_CTRLDEP   Next(void)              {
    if (Iter_idx() == 0) Iterate_parent();
    Dec_iter_idx();
    return Iter_node()->Get_control_dependency(_iter_idx);
  }
};

#endif  // opt_vsa_path_INCLUDE
