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

// =============================================================================
// =============================================================================
//
// Module: opt_vsa_vcg.cxx
//
// =============================================================================
//
// Description:
//
// Dump vsa & ipsa data structure into VCG
//
// =============================================================================
// =============================================================================

#include "defs.h"
#include "opt_vsa_vcg.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_vsa_rbc.h"
using namespace __gnu_cxx;


// =============================================================================
// IPSA_CG_VCG
// Print IPSA Call Graph to VCG
// =============================================================================
class IPSA_CG_VCG : public IPSA_VCG_BASE {
private:
  // callby edge
  struct CALLBY_EDGE {
  private:
    UINT32 _style;              // edge style
    SRCPOS _spos;               // edge srcpos

  public:
    // constructor
    CALLBY_EDGE(UINT32 style, SRCPOS spos) : _style(style), _spos(spos) {}

    // get edge style
    UINT32 Style() const { return _style; }
    // get edge srcpos
    SRCPOS Spos() const  { return _spos;  }
  };  // CALLBY_EDGE

  // CALLBY_INFO
  typedef uint64_t CALLBY_INFO;
  // pair of <CALLBY_INFO, CALLBY_EDGE>
  typedef std::pair<CALLBY_INFO, CALLBY_EDGE> CALLBY_PAIR;

  // map of CALLBY_INFO for inlcxt to avoid duplicated edges
  typedef hash_map<CALLBY_INFO, CALLBY_EDGE,
                   __gnu_cxx::hash<CALLBY_INFO>,
                   std::equal_to<CALLBY_INFO>,
                   mempool_allocator<CALLBY_PAIR> > CALLBY_MAP;

  // <title, label> for node info
  struct NODE_INFO {
  private:
    const char *_title;         // title of the node
    const char *_label;         // label of the node
    CALLBY_MAP *_clby_map;      // parent nodes
    BOOL        _addr_taken;    // address taken
    IDTYPE_SET *_access_core;   // core ID of conditional access

  public:
    // empty constructor
    NODE_INFO()
      : _title(NULL), _label(NULL), _clby_map(NULL),
        _addr_taken(FALSE), _access_core(NULL) {}

    // for variable
    NODE_INFO(const char *t, const char *l, MEM_POOL *mp)
      : _title(t), _label(l)
    {
      _clby_map = CXX_NEW(CALLBY_MAP(3,
                                     __gnu_cxx::hash<CALLBY_INFO>(),
                                     std::equal_to<CALLBY_INFO>(),
                                     mempool_allocator<CALLBY_PAIR>(mp)),
                          mp);
      _addr_taken = FALSE;
      _access_core = CXX_NEW(IDTYPE_SET(3,
                                        IDTYPE_HASHER(),
                                        IDTYPE_EQUAL(),
                                        IDTYPE_ALLOCATOR(mp)),
                             mp);
      Is_True(_clby_map != NULL, ("ipsa cg vcg out of memory"));
    }

    // intialize node
    void Init(const char *t, const char *l, MEM_POOL *mp)
    {
      _title = t;
      _label = l;
      _clby_map = CXX_NEW(CALLBY_MAP(3,
                                     __gnu_cxx::hash<CALLBY_INFO>(),
                                     std::equal_to<CALLBY_INFO>(),
                                     mempool_allocator<CALLBY_PAIR>(mp)),
                          mp);
      _addr_taken = FALSE;
      _access_core = CXX_NEW(IDTYPE_SET(3,
                                        IDTYPE_HASHER(),
                                        IDTYPE_EQUAL(),
                                        IDTYPE_ALLOCATOR(mp)),
                             mp);
      Is_True(_clby_map != NULL, ("ipsa cg vcg out of memory"));
    }

    // check if node info is valid
    BOOL Is_valid() const { return _title != NULL && _label != NULL; }
    // check if node info is null
    BOOL Is_null() const  { return _title == NULL && _label == NULL; }
    // check if address taken
    BOOL Is_addr_taken() const { return _addr_taken; }

    // get node title
    const char *Title() const      { return _title;    }
    // get node label
    const char *Label() const      { return _label;    }
    // get call-by map
    const CALLBY_MAP *Clby() const { return _clby_map; }
    // update node lable
    void Update_label(const char *l) { _label = l; }
    // get access core ID set
    IDTYPE_SET *Access_core_set(void) { return _access_core; }
    // set access core ID set
    void Set_access_core_set(IDTYPE_SET *ac) { _access_core = ac; }
    // add core id to access core id set
    void Add_access_core(IDTYPE core_id)
    {
      if (_access_core != NULL)
        _access_core->insert(core_id);
    }
    // check if core id is in access core id set
    BOOL Access_core(IDTYPE core_id)
    {
      if (_access_core == NULL || _access_core->empty())
        return TRUE;
      return _access_core->find(core_id) != _access_core->end();
    }

    // add call-by with edge style and srcpos
    BOOL Add_clby(const NODE_INFO *clby, UINT32 style, SRCPOS spos)
    {
      Is_True(_clby_map != NULL, ("_clby_map not initialized"));
      CALLBY_PAIR pair((CALLBY_INFO)clby, CALLBY_EDGE(style, spos));
      return _clby_map->insert(pair).second;
    }
    // set address taken flag
    void Set_addr_taken() { _addr_taken = TRUE; }

    // print node info to fp
    void Print(FILE *fp) const;
  }; // NODE_INFO

  // pair of <file_idx << 32 | st_idx, node_info*>
  typedef std::pair<uint64_t, NODE_INFO *> ST_INFO_PAIR;

  // map of <file_idx << 32 | st_idx, node_info>
  typedef hash_map<uint64_t, NODE_INFO *,
                   __gnu_cxx::hash<uint64_t>,
                   std::equal_to<uint64_t>,
                   mempool_allocator<ST_INFO_PAIR> > ST_INFO_MAP;

  // pair of <extern_symbol_name, node_info>
  typedef std::pair<const char *, NODE_INFO*> EXT_INFO_PAIR;

  // streq to compare strings
  struct streq {
    bool operator()(const char *l, const char *r) const {
      return strcmp(l, r) == 0;
    }
  };

  // map of <extern_symbol_name, node_info>
  typedef hash_map<const char *, NODE_INFO *,
                   __gnu_cxx::hash<const char *>, streq,
                   mempool_allocator<EXT_INFO_PAIR> > EXT_INFO_MAP;


  // set of generic pointer to avoid duplicated visit
  typedef hash_set<uintptr_t,
                   __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   mempool_allocator<uintptr_t> > PTR_SET;

  // prefix for vcg node title
  enum NODE_PREFIX {
    FUNC_NODE     = 'F',    // node is function
    VAR_NODE      = 'V',    // node is variable
    EXT_FUNC_NODE = 'f',    // node is external function
    EXT_VAR_NODE  = 'v',    // node is external variable
  };

  // prefix for vcg edge prefix
  enum EDGE_PREFIX {
    CALL_EDGE     = 'C',    // direct call
    ICALL_EDGE    = 'I',    // indirect call
    ACCESS_EDGE   = 'A',    // function access symbol
  };

  // shapre for vcg node
  enum NODE_SHAPE {
    FUNC_SHAPE    = Box,           // box for function
    VAR_SHAPE     = Ellipse,       // ellipse for variable
  };

  // background color for vcg node
  enum NODE_COLOR {
    DEF_COLOR     = Khaki,         // symbol defined
    UNDEF_COLOR   = LightCyan,     // symbol undefined
  };

  // style for vcg edge
  enum EDGE_STYLE {
    CALL_STYLE    = Continuous,    // solid line for call
    ICALL_STYLE   = Dashed,        // dashed line for indirect call
    ACCESS_STYLE  = Dotted,        // dotted line for function access variable
  };

private:
  IPSA          *_ipsa;            // IPSA
  NODE_INFO     *_dna_info;        // dna title, indexed by dna index
  ST_INFO_MAP   *_st_info_map;     // <file_idx, st_idx> -> info map
  EXT_INFO_MAP  *_ext_info_map;    // ext_name -> info map
  EXT_INFO_MAP  *_name_info_map;   // st_name -> info map
  UINT64_SET    *_info_visited;    // set of INLCXT visited
  mUINT8        *_dna_visited;     // dna visited
  mUINT8         _root_count;      // count of dna that on top is root
  BOOL           _exp_inlcxt;      // expand inline context
  BOOL           _multi_edge;      // multi edge for diff callsite between same caller/callee
  BOOL           _ext_var;         // add external variable
  BOOL           _ext_func;        // add external function
  BOOL           _top_not_root;    // has top level dna that is not root
  BOOL           _top_not_root_at; // has top level dna that is not root but address taken

  IPSA_CG_VCG(const IPSA_CG_VCG &);             // disable copy ctor
  IPSA_CG_VCG &operator=(const IPSA_CG_VCG &);  // disable assign oper

private:
  // check and set if NODE_INFO has been visited
  BOOL Visited(NODE_INFO *info)
  {
    UINT64_SET::iterator it = _info_visited->find((uintptr_t)info);
    if (it != _info_visited->end())
      return TRUE;
    _info_visited->insert((uintptr_t)info);
    return FALSE;
  }

  // check and set if DNA_NODE has been visited before
  BOOL Visited(DNA_NODE *dna)
  {
    if (_dna_visited[dna->Dna_idx()] == TRUE)
      return TRUE;
    _dna_visited[dna->Dna_idx()] = TRUE;
    return FALSE;
  }

  // clear visited flag
  void Reset_visited() {
    _info_visited->clear();

    UINT32 dna_cnt = _ipsa->Dna_count();
    memset(_dna_visited, 0, dna_cnt * sizeof(mUINT8));
  }

private:
  // get file name from given file_id and spos
  const char *Get_file_name(UINT32 file_id, SRCPOS spos);

  // get label from given name and spos
  const char *Get_label(const char *name, UINT32 file_id, SRCPOS spos);
  const char *Get_label(const char *name, const char *file, UINT64 line);

  // get title from inlcxt
  NODE_INFO *Get_inlcxt_info(VCGGraph &vcg, DNA_NODE *dna, INLCXT *inlcxt);

  // get title for symbol
  template<typename _MAP, typename _KEY>
  NODE_INFO *Lookup_info(_MAP *map, _KEY key, NODE_PREFIX pfx, VCGGraph &vcg,
                            const char *name, UINT32 file_idx, SRCPOS spos)
  {
    typename _MAP::iterator it = map->find(key);
    if (it != map->end()) {
      Is_True(it->second && it->second->Is_valid(), ("bad node info"));
      return it->second;
    }
    else {
      char *title = TYPE_MEM_POOL_ALLOC_N(char, Mem_pool(), 16);
      Is_True(title != NULL, ("ipsa cg vcg out of memory"));
      snprintf(title, 16, "%c%d", pfx, (UINT32)map->size());
      const char *label = Get_label(name, file_idx, spos);
      NODE_INFO *info = CXX_NEW(NODE_INFO(title, label, Mem_pool()),
                                Mem_pool());
      map->insert(std::pair<_KEY, NODE_INFO*>(key, info));

      if (Visited(info) == FALSE) {
        NODE_SHAPE shape = (pfx == VAR_NODE || pfx == EXT_VAR_NODE) ? VAR_SHAPE
                                                                    : FUNC_SHAPE;
        NODE_COLOR color = (pfx == FUNC_NODE || pfx == VAR_NODE) ? DEF_COLOR
                                                                 : UNDEF_COLOR;
        VCGNode *node = Build_node(title, label, (NodeShape)shape);
        node->backGroundColor((VCGColors)color);
        vcg.addNode(*node);
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: add node [%s, %s] for %s %s\n",
                         title, label,
                         color == DEF_COLOR ? "defined" : "extern",
                         shape == VAR_SHAPE ? "variable" : "function"));
      }

      return info;
    }
  }

  // add a edge from src to dst at line N
  void        Add_edge(VCGGraph &vcg, const char *src, const char *dst,
                       EDGE_STYLE style, SRCPOS spos)
  {
    Is_True(src != NULL && dst != NULL, ("invalid src or dst"));

    VCGEdge *edge = Build_edge(src, dst, (VCGEdgeLineStyle)style);

    char *label = NULL;
    if (spos != 0) {
      label = TYPE_MEM_POOL_ALLOC_N(char, Mem_pool(), 16);
      Is_True(label != NULL, ("ipsa cg vcg out of memory"));
      snprintf(label, 16, "L%d", Srcpos_To_Line(spos));
      edge->label(label);
    }

    vcg.addEdge(*edge);
    Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
             (TFile, "VCG: add edge [%s] %s [%s] at %s\n",
                     src, dst,
                     style == CALL_STYLE ? "call" :
                       style == ICALL_STYLE ? "icall" : "access",
                     label ? label : "<no srcpos>"));
  }

  // initialize title array and map
  void        Initialize();
  // add global symbols used in cr, attach to function with given title
  BOOL        Add_symbol_expr(VCGGraph &vcg, DNA_NODE *dna, STMTREP *sr,
                              CODEREP *cr, PTR_SET *inlcxt_set, BOOL use);
  // add global symbols accessed in DNA by search stmt, with inlcxt
  BOOL        Add_symbol_stmt(VCGGraph &vcg, DNA_NODE *dna, PTR_SET *inlcxt_set);
  // add inlcxt and it's parent to vcg
  BOOL        Add_cg_inlcxt(VCGGraph &vcg, DNA_NODE *dna, INLCXT *inlcxt, NODE_INFO *ninfo);
  // add dna node and it's caller to vcg
  void        Add_cg_caller(VCGGraph &vcg, DNA_NODE *dna);
  // add the DNA node
  NODE_INFO  *Add_dna(VCGGraph &vcg, DNA_NODE *dna, BOOL required);
  // add the DNA node via RBC annotation
  NODE_INFO  *Add_rbc_dna(VCGGraph &vcg, DNA_NODE *dna);
  // build vcg for while call graph
  void        Build_cg_vcg();
  // find symbol access core ID according to control dependency
  IDTYPE      Find_symbol_access_core(BB_NODE *bb, DNA_NODE *dna, IDTYPE_SET &visited);

  // add call-by node for symbol
  void        Add_clby_vcg(VCGGraph &vcg, NODE_INFO *info, BOOL is_sym);
  // build vcg for each global variable
  void        Build_sym_vcg();

  // set has top dna that is not a root
  void        Set_has_top_not_root() { _top_not_root = TRUE; }
  // if there's a dna that is on top but not a root
  BOOL        Has_top_not_root() { return _top_not_root; }
  // reset the flag
  void        Reset_has_top_not_root() { _top_not_root = FALSE; }
  // set has top dna that is not a root but address taken
  void        Set_has_top_not_root_addr_taken() { _top_not_root_at = TRUE; }
  // if there's a dna that is on top but not a root and address taken
  BOOL        Has_top_not_root_addr_taken() { return _top_not_root_at; }
  // reset the flag
  void        Reset_has_top_not_root_addr_taken() { _top_not_root_at = FALSE; }

  // increase the count of root dna by 1
  void        Increase_root_count() { _root_count++; }
  // return the counts of dna that is root
  mUINT8      Root_count() { return _root_count; }
  // reset the count
  void        Reset_root_count() { _root_count = 0; }

public:
  // constructor
  IPSA_CG_VCG(IPSA *ipsa, const char *fname)
    : IPSA_VCG_BASE(fname),
      _ipsa(ipsa), _dna_info(NULL), //_st_vec_map(NULL),
      _st_info_map(NULL), _ext_info_map(NULL), _name_info_map(NULL),
      _info_visited(NULL), _dna_visited(NULL),
      _exp_inlcxt(TRUE), _multi_edge(FALSE),
      _ext_var(TRUE), _ext_func(FALSE),
      _top_not_root(FALSE), _top_not_root_at(FALSE), _root_count(0) {}

  // build VCG
  void Build_vcg();

  // dump node info
  void Print_node_info(FILE *fp) const;

}; // IPSA_CG_VCG

// =============================================================================
// IPSA_CG_VCG::NODE_INFO::Print()
// =============================================================================
void
IPSA_CG_VCG::NODE_INFO::Print(FILE *fp) const
{
  if (_title == NULL) {
    Is_True(_label == NULL && _clby_map == NULL, ("bad node fields"));
    fprintf(fp, "null\n");
    return;
  }
  Is_True(_label != NULL && _clby_map != NULL, ("bad node fields"));
  char *pos = strchr((char*)_label, '\\');
  UINT len = pos ? (pos - _label) : strlen(_label);
  fprintf(fp, "title=%s label=%.*s\n", _title, len, _label);
  for (CALLBY_MAP::const_iterator it = _clby_map->begin();
       it != _clby_map->end(); ++it) {
    const NODE_INFO *clby = (const NODE_INFO *)it->first;
    const char *style = (it->second.Style() == CALL_STYLE) ? "Called"
                           : (it->second.Style() == ICALL_STYLE) ? "Icalled"
                                : "Accessed";
    const char *label = clby->Label();
    pos = strchr((char*)label, '\\');
    len = pos ? (pos - label) : strlen(label);
    fprintf(fp, "   %s by title=%s, label=%.*s at line %d\n",
                style, clby->Title(), len, clby->Label(),
                Srcpos_To_Line(it->second.Spos()));
  }
  fprintf(fp, "   Accessed in Core {");
  for (IDTYPE_SET::const_iterator it = _access_core->begin();
       it != _access_core->end(); ++it) {
    fprintf(fp, " %d", *it);
  }
  fprintf(fp, " }\n");
}

// =============================================================================
// IPSA_CG_VCG::Get_file_name()
// Get file name from local file id from SRCPOS
// =============================================================================
const char *
IPSA_CG_VCG::Get_file_name(UINT32 file_id, SRCPOS spos)
{
  if (spos == 0)
    return "<UNKNOWN>";

  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  Is_True(file_id == 0 || mgr != NULL, ("invalid mgr"));

  const char *fname = NULL;
  const char *dname = NULL;
  if (file_id == 0) {
    Get_Srcpos_Filename(spos, &fname, &dname);
    return fname;
  }

  INT32 lid = SRCPOS_filenum(spos);
  INT32 gid = mgr->Get_file(file_id).Get_global_file_num(lid);
  fname = mgr->Get_file_name(gid);
  Is_True(fname && fname[0] != '\0', ("bad file name"));
  // strip dirname
  dname = fname;
  char ch;
  while ((ch = *fname) != '\0') {
    if (ch == '/' || ch == '\\')
      dname = fname + 1;
    ++fname;
  }
  return dname;
}

// =============================================================================
// IPSA_CG_VCG::Get_label()
// =============================================================================
const char *
IPSA_CG_VCG::Get_label(const char *name, UINT32 file_id, SRCPOS spos)
{
  const char *filename = Get_file_name(file_id, spos);
  Is_True(filename && filename[0] != '\0', ("bad file name"));
  BOOL need_free = FALSE;
  if (name[0] == '_' && name[1] == 'Z') {
    // need demangle
    name = Vsa_demangle(name);
    Is_True(name && name[0] != '\0', ("bad demangle name"));
    need_free = TRUE;
  }
  UINT32 label_len = strlen(name) + strlen(filename) + 16;
  char *label = TYPE_MEM_POOL_ALLOC_N(char, Mem_pool(), label_len);
  Is_True(label != NULL, ("ipsa cg vcg out of memory"));
  snprintf(label, label_len, "%s\\n%s:%d", name, filename,
                             SRCPOS_linenum(spos));
  if (need_free)
    free((void *)name);

  return label;
}

// =============================================================================
// IPSA_CG_VCG::Get_label()
// =============================================================================
const char *
IPSA_CG_VCG::Get_label(const char *name, const char *filename, UINT64 line)
{
  Is_True(filename && filename[0] != '\0', ("bad file name"));
  BOOL need_free = FALSE;
  if (name[0] == '_' && name[1] == 'Z') {
    // need demangle
    name = Vsa_demangle(name);
    Is_True(name && name[0] != '\0', ("bad demangle name"));
    need_free = TRUE;
  }
  UINT32 label_len = strlen(name) + strlen(filename) + 16;
  char *label = TYPE_MEM_POOL_ALLOC_N(char, Mem_pool(), label_len);
  Is_True(label != NULL, ("ipsa cg vcg out of memory"));
  snprintf(label, label_len, "%s\\n%s:%lld", name, filename, line);
  if (need_free)
    free((void *)name);

  return label;
}

// =============================================================================
// IPSA_CG_VCG::Get_inlcxt_info()
// =============================================================================
IPSA_CG_VCG::NODE_INFO *
IPSA_CG_VCG::Get_inlcxt_info(VCGGraph &vcg, DNA_NODE *dna, INLCXT *inlcxt)
{
  Is_True(inlcxt != NULL &&
          inlcxt->Inlcxt_call_st() != ST_IDX_ZERO, ("invalid inlcxt"));
  UINT32 file_idx = dna->File_idx();
  ST* st = St_ptr(file_idx, inlcxt->Inlcxt_call_st());
  Is_True(st != NULL && ST_class(st) == CLASS_FUNC, ("invalid inlcxt st"));
  DNA_NODE *inl_dna = _ipsa->Get_dna(file_idx, st);
  NODE_INFO *info = NULL;
  if (inl_dna != NULL) {
    info = Add_dna(vcg, inl_dna, TRUE);
  }
  else {
    Is_True(ST_sclass(st) != SCLASS_EXTERN, ("inline extern func"));
    uint64_t key = ((uint64_t)file_idx << 32) | ST_st_idx(st);
    const char *name = ST_name(file_idx, st);
    info = Lookup_info(_st_info_map, key, FUNC_NODE,
                       vcg, name, file_idx, ST_Srcpos(*st));
  }
  Is_True(info && info->Title()[0] == FUNC_NODE, ("invalid node title"));
  return info;
}

// =============================================================================
// IPSA_CG_VCG::Initialize()
// =============================================================================
void
IPSA_CG_VCG::Initialize()
{
  UINT32 dna_cnt = _ipsa->Dna_count();
  // initialize _dna_info array
  Is_True(_dna_info == NULL, ("_dna_info already initialized"));
  _dna_info = TYPE_MEM_POOL_ALLOC_N(NODE_INFO, Mem_pool(), dna_cnt);
  Is_True(_dna_info != NULL, ("ipsa cg vcg out of memory"));
  memset(_dna_info, 0, sizeof(NODE_INFO) * dna_cnt);

  // initialize global ST -> node_info map
  Is_True(_st_info_map == NULL, ("_st_info_map already initialized"));
  _st_info_map = CXX_NEW(ST_INFO_MAP(11,
                                       __gnu_cxx::hash<uint64_t>(),
                                       std::equal_to<uint64_t>(),
                                       mempool_allocator<ST_INFO_PAIR>(Mem_pool())),
                          Mem_pool());
  Is_True(_st_info_map != NULL, ("ipsa cg vcg out of memory"));

  // initialize extern ST -> node_info map
  Is_True(_ext_info_map == NULL, ("_ext_info_map already initialized"));
  _ext_info_map = CXX_NEW(EXT_INFO_MAP(11,
                                         __gnu_cxx::hash<const char *>(), streq(),
                                         mempool_allocator<EXT_INFO_PAIR>(Mem_pool())),
                            Mem_pool());
  Is_True(_ext_info_map != NULL, ("ipsa cg vcg out of memory"));

  // initialize name -> node_info map
  Is_True(_name_info_map == NULL, ("_name_info_map already initialized"));
  _name_info_map = CXX_NEW(EXT_INFO_MAP(11,
                                        __gnu_cxx::hash<const char *>(), streq(),
                                        mempool_allocator<EXT_INFO_PAIR>(Mem_pool())),
                           Mem_pool());
  Is_True(_name_info_map != NULL, ("ipsa cg vcg out of memory"));

  // initialize _info_visited
  Is_True(_info_visited == NULL, ("_info_visited already initialized"));
  _info_visited = CXX_NEW(UINT64_SET(3,
                                       __gnu_cxx::hash<uintptr_t>(),
                                       std::equal_to<uintptr_t>(),
                                       mempool_allocator<uintptr_t>(Mem_pool())),
                            Mem_pool());
  Is_True(_info_visited != NULL, ("ipsa cg vcg out of memory"));

  // initialize _dna_visited
  Is_True(_dna_visited == NULL, ("_dna_visited already initialized"));
  _dna_visited = TYPE_MEM_POOL_ALLOC_N(mUINT8, Mem_pool(), dna_cnt);
  Is_True(_dna_visited != NULL, ("ipsa cg vcg out of memory"));
  memset(_dna_visited, 0, sizeof(mUINT8) * dna_cnt);

}

// =============================================================================
// IPSA_CG_VCG::Find_symbol_access_core()
//
//   Li Auto uses condition like "if (GetCoreId() == 0)" to restrict
//   'then' block codes to execute on Core0 only, so we go rcfg to
//   find out such restrictions. 'BrsHw_GetCore()' returns 'CoreId + 4'.
// =============================================================================
IDTYPE
IPSA_CG_VCG::Find_symbol_access_core(BB_NODE *bb, DNA_NODE *dna, IDTYPE_SET &visited)
{
  // [0, 1, 2, 4, 5, 6] are possible core IDs, we use 7 to represent unknown.
  IDTYPE ret = 7;
  if (bb == NULL)
    return ret;

  if (visited.find(bb->Id()) != visited.end())
    return ret;
  visited.insert(bb->Id());
  BB_NODE_SET_ITER dfz_iter;
  BB_NODE_SET *dfz = bb->Rcfg_dom_frontier();
  BB_NODE *rcfg_bb;
  FOR_ALL_ELEM(rcfg_bb, dfz_iter, Init(dfz)) {
    STMTREP *last_stmt = rcfg_bb->Last_stmtrep();
    // check if 'if' statement is CoreID related
    if (last_stmt != NULL && (last_stmt->Opr() == OPR_FALSEBR ||
                              last_stmt->Opr() == OPR_TRUEBR)) {
      CODEREP *cmp = last_stmt->Rhs();
      if (cmp->Kind() == CK_OP && (cmp->Opr() == OPR_EQ ||
                                   cmp->Opr() == OPR_NE)) {
        CODEREP *rhs = cmp->Opnd(1);
        CODEREP *lhs = cmp->Opnd(0);
        CODEREP *def_cr = NULL;
        IDTYPE core_id = 0;
        if (rhs->Kind() == CK_CONST) {
          core_id = rhs->Const_val();
          def_cr = lhs;
        }
        else if (lhs->Kind() == CK_CONST) {
          core_id = lhs->Const_val();
          def_cr = rhs;
        }
        else
          continue;
        if (def_cr != NULL && def_cr->Kind() == CK_VAR &&
            !def_cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI))) {
          STMTREP *defstmt = def_cr->Defstmt();
          while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
            def_cr = defstmt->Rhs();
            if (def_cr != NULL && def_cr->Kind() == CK_VAR && defstmt != def_cr->Defstmt())
              defstmt = def_cr->Defstmt();
            else
              break;
          }
          if (defstmt != NULL && (defstmt->Opr() == OPR_CALL)) {
            ST *st = defstmt->St();
            char *fname = st != NULL ? ST_name(dna->File_idx(), st) : NULL;
            if (fname != NULL && (strncmp(fname, "BrsHw_GetCore", 13) == 0 ||
                                  strncmp(fname, "GetCoreID", 9) == 0)) {
              ret = core_id;
              break;
            }
          }
        }
      }
    }
    // not yet found & recursively check rcfg BBs
    ret = Find_symbol_access_core(rcfg_bb, dna, visited);
    if (ret < 7)
      break;
  }
  return ret;
}

// =============================================================================
// IPSA_CG_VCG::Add_symbol()
// =============================================================================
BOOL
IPSA_CG_VCG::Add_symbol_expr(VCGGraph &vcg, DNA_NODE *dna, STMTREP *sr,
                             CODEREP *cr, PTR_SET *inlcxt_set, BOOL use)
{
  Is_True(dna != NULL && cr != NULL, ("invalid dna or cr"));

  BOOL has_global = FALSE;
  ST *st = NULL;

  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
    return FALSE;
  case CK_LDA:  // should add LDA of global to graph?
    st = cr->Lda_base_st();
    break;

  case CK_VAR:
    st = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
    if (st == NULL)
      return FALSE;
    break;

  case CK_IVAR:
    if (cr->Opr() == OPR_MLOAD) {
      CODEREP *sz = cr->Mload_size() ? cr->Mload_size() : cr->Mstore_size();
      has_global = Add_symbol_expr(vcg, dna, sr, sz, inlcxt_set, TRUE);
    }
    cr = cr->Ilod_base() ? cr->Ilod_base() : cr->Istr_base();
    has_global |= Add_symbol_expr(vcg, dna, sr, cr, inlcxt_set, TRUE);
    return has_global;

  case CK_OP:
    for (INT i = 0; i < cr->Kid_count(); ++i) {
      has_global |= Add_symbol_expr(vcg, dna, sr, cr->Opnd(i), inlcxt_set, TRUE);
    }
    return has_global;
  }

  Is_True(st != NULL, ("bad st"));
  if (ST_level(st) != GLOBAL_SYMTAB)
    return FALSE;
  if (ST_class(st) != CLASS_VAR)
    return FALSE;

  UINT32 file_idx = dna->File_idx();
  const char *name = ST_name(file_idx, st);
  Is_True(name && name[0] != '\0', ("bad st name"));
  if (name[0] == '.')
    return FALSE;

  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
  Is_True(file_idx == 0 || mgr != NULL, ("invalid xfa mode"));

  UINT32 st_idx = ST_st_idx(st);
  if (mgr && ST_sclass(st) == SCLASS_EXTERN) {
    // resolve extern symbol
    mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
    if (file_idx != dna->File_idx())
      st = St_ptr(file_idx, st_idx);
  }

  if (ST_sclass(st) == SCLASS_EXTERN && _ext_var == FALSE)
    return FALSE;

  INLCXT *inlcxt = sr->Bb()->Inlinecxt();
  NODE_INFO *ninfo = NULL;
  if (_exp_inlcxt && inlcxt != NULL) {
    ninfo = Get_inlcxt_info(vcg, dna, inlcxt);
  }
  else {
    ninfo = &_dna_info[dna->Dna_idx()];
  }
  Is_True(ninfo && ninfo->Title()[0] == FUNC_NODE, ("invalid title"));

  NODE_INFO *st_info = NULL;
  SRCPOS spos = ST_Srcpos(*st);
  if (ST_sclass(st) == SCLASS_EXTERN) {
    st_info = Lookup_info(_ext_info_map, name, EXT_VAR_NODE,
                          vcg, name, file_idx, spos);
    Is_True(st_info && st_info->Title()[0] == EXT_VAR_NODE, ("bad ext var title"));
  }
  else {
    uint64_t key = ((uint64_t)file_idx << 32) | st_idx;
    st_info = Lookup_info(_st_info_map, key, VAR_NODE,
                          vcg, name, file_idx, spos);
    Is_True(st_info && st_info->Title()[0] == VAR_NODE, ("bad var title"));
  }

  IDTYPE_SET visited;
  IDTYPE core_id = Find_symbol_access_core(sr->Bb(), dna, visited);
  if (core_id < 7) {
    Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
             (TFile, "VCG: symbol %s accessed in Core%d in %s\n",
              name, core_id, dna->Fname()));
    st_info->Add_access_core(core_id%4);
  }

  if (_name_info_map->find(name) == _name_info_map->end()) {
    _name_info_map->insert(std::pair<const char*, NODE_INFO*>(name, st_info));
  }

  if (st_info->Add_clby(ninfo, ACCESS_STYLE, sr->Linenum()) == TRUE) {
    if (Visited(ninfo) == FALSE) {
      VCGNode *node = Build_node(ninfo->Title(), ninfo->Label(), Box);
      vcg.addNode(*node);
      Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
               (TFile, "VCG: add node [%s, %s] for defined function\n",
                       ninfo->Title(), ninfo->Label()));
    }

    Add_edge(vcg, ninfo->Title(), st_info->Title(), ACCESS_STYLE,
             sr->Linenum());
  }

  if (_exp_inlcxt && inlcxt &&
      inlcxt_set->find((uintptr_t)inlcxt) == inlcxt_set->end()) {
    inlcxt_set->insert((uintptr_t)inlcxt);
    Add_cg_inlcxt(vcg, dna, inlcxt, ninfo);
  }

  return TRUE;
}

// =============================================================================
// IPSA_CG_VCG::Add_symbol_stmt()
// =============================================================================
BOOL
IPSA_CG_VCG::Add_symbol_stmt(VCGGraph &vcg, DNA_NODE *dna, PTR_SET *inlcxt_set)
{
  Is_True(dna && !dna->Non_functional(), ("invalid dna node"));
  CFG *cfg = dna->Comp_unit()->Cfg();
  Is_True(cfg != NULL, ("invalid dna cfg"));
  BOOL has_global = FALSE;

  for (BB_NODE *bb = cfg->First_bb(); bb != NULL; bb = bb->Next()) {
    for (STMTREP *sr = bb->First_stmtrep(); sr != NULL; sr = sr->Next()) {
      if (sr->Rhs()) {
        has_global |= Add_symbol_expr(vcg, dna, sr, sr->Rhs(), inlcxt_set, TRUE);
      }
      OPERATOR opr = sr->Opr();
      if (OPERATOR_is_store(opr)) {
        BOOL use = (opr != OPR_STID && opr != OPR_STBITS);
        has_global |= Add_symbol_expr(vcg, dna, sr, sr->Lhs(), inlcxt_set, use);
      }
    }
  }

  return has_global;
}

// =============================================================================
// IPSA_CG_VCG::Add_cg_inlcxt()
// Add inlcxt and it's parents to vcg.
// return FALSE if parent has been added before
// =============================================================================
BOOL
IPSA_CG_VCG::Add_cg_inlcxt(VCGGraph &vcg, DNA_NODE *dna,
                           INLCXT *inlcxt, NODE_INFO *info)
{
  Is_True(inlcxt != NULL, ("bad inlcxt"));
  Is_True(info != NULL && info->Is_valid(), ("bad inl info"));

  SRCPOS prev_spos = inlcxt->Inlcxt_line_num();

  inlcxt = inlcxt->Parent();
  while (inlcxt != NULL) {
    NODE_INFO *pinfo = Get_inlcxt_info(vcg, dna, inlcxt);
    Is_True(pinfo && pinfo->Is_valid(), ("bad pinfo"));
    Is_True(pinfo->Title() && pinfo->Title()[0] == FUNC_NODE,
            ("invalid node title"));

    if (info->Add_clby(pinfo, CALL_STYLE, prev_spos)) {
      Add_edge(vcg, pinfo->Title(), info->Title(),
               CALL_STYLE, prev_spos);
    }

    info = pinfo;
    prev_spos = inlcxt->Inlcxt_line_num();
    inlcxt = inlcxt->Parent();
  }

  NODE_INFO *dna_info = Add_dna(vcg, dna, TRUE);
  Is_True(dna_info != NULL && dna_info->Is_valid(),
          ("invalid dna NODE_INFO"));
  Is_True(dna_info->Title() && dna_info->Title()[0] == FUNC_NODE,
          ("invalid dna title"));
  if (info->Add_clby(dna_info, CALL_STYLE, prev_spos)) {
    Add_edge(vcg, dna_info->Title(), info->Title(), CALL_STYLE, prev_spos);
  }

  return TRUE;
}

// =============================================================================
// IPSA_CG_VCG::Add_cg_caller()
// Add dna and it's clby to vcg
// =============================================================================
void
IPSA_CG_VCG::Add_cg_caller(VCGGraph &vcg, DNA_NODE *dna)
{
  Is_True(dna != NULL && !dna->Non_functional(), ("bad dna"));
  NODE_INFO &info = _dna_info[dna->Dna_idx()];
  Is_True(info.Is_valid(), ("bad dna info"));

  const char *dna_title = info.Title();
  const char *dna_label = info.Label();

  // add clby at first
  for (INT i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++) {
    RNA_NODE* rna = (*dna->Clby_list())[i];
    Is_True(rna != NULL && rna->Callstmt() != NULL, ("bad rna"));

    // if (rna->Is_back_edge())
    //   continue;

    DNA_NODE* caller = _ipsa->Get_dna(rna->Caller_idx());
    Is_True(caller != NULL, ("bad caller"));
    if (caller->Non_functional())
      continue;

    STMTREP *callsite = rna->Callstmt();
    Is_True(callsite != NULL, ("bad callstmt"));
    EDGE_STYLE style = (callsite->Opr() == OPR_CALL)
                         ? CALL_STYLE : ICALL_STYLE;
    SRCPOS spos = callsite->Linenum();

    INLCXT *inlcxt = callsite->Bb()->Inlinecxt();
    if (_exp_inlcxt && inlcxt != NULL) {
      NODE_INFO *ninfo = Get_inlcxt_info(vcg, caller, inlcxt);
      Is_True(ninfo != NULL && ninfo->Is_valid(), ("bad inlcxt node info"));
      if (info.Add_clby(ninfo, style, spos) == TRUE) {
        Add_edge(vcg, ninfo->Title(), info.Title(), style, spos);
      }
      Add_cg_inlcxt(vcg, caller, inlcxt, ninfo);
    }
    else {
      NODE_INFO *ninfo = Add_dna(vcg, caller, TRUE);
      Is_True(ninfo != NULL && ninfo->Is_valid(), ("bad inlcxt node info"));
      if (info.Add_clby(ninfo, style, spos) == TRUE) {
        Add_edge(vcg, ninfo->Title(), info.Title(), style, spos);
      }
    }
  }
}

// =============================================================================
// IPSA_CG_VCG::Add_dna()
// =============================================================================
IPSA_CG_VCG::NODE_INFO *
IPSA_CG_VCG::Add_dna(VCGGraph &vcg, DNA_NODE *dna, BOOL required)
{
  Is_True(dna != NULL && !dna->Non_functional(), ("invalid dna node"));

  NODE_INFO& info = _dna_info[dna->Dna_idx()];
  if (Visited(dna)) {
    Is_True(info.Is_valid(), ("bad dna info"));
    return &info;
  }
  Is_True(info.Is_null(), ("dna already added"));

  if (_name_info_map->find(dna->Fname()) == _name_info_map->end()) {
    _name_info_map->insert(std::pair<const char*, NODE_INFO*>(dna->Fname(), &info));
  }

  // title
  char *title = TYPE_MEM_POOL_ALLOC_N(char, Mem_pool(), 16);
  Is_True(title != NULL, ("ipsa cg vcg out of memory"));
  snprintf(title, 16, "%c%d", FUNC_NODE, dna->Dna_idx());

  // label
  SRCPOS spos = WN_Get_Linenum(dna->Comp_unit()->Input_tree());
  const char *label = Get_label(dna->Fname(), dna->File_idx(), spos);

  info.Init(title, label, Mem_pool());
  if (dna->Is_set(DNA_ADDR_TAKEN)) {
    info.Set_addr_taken();
  }

  PTR_SET *inlcxt_set = CXX_NEW(PTR_SET(3,
                                        __gnu_cxx::hash<uintptr_t>(),
                                        std::equal_to<uintptr_t>(),
                                        mempool_allocator<uintptr_t>(Mem_pool())),
                                Mem_pool());
  Is_True(inlcxt_set != NULL, ("ipsa cg vcg out of memory"));

  BOOL has_global = Add_symbol_stmt(vcg, dna, inlcxt_set);

  // delete inlcxt_set
  CXX_DELETE(inlcxt_set, Mem_pool());

  if (has_global || required) {
    // add caller
    Add_cg_caller(vcg, dna);
    // add node
    if (Visited(&info) == FALSE) {
      VCGNode *node = Build_node(title, label, Box);
      vcg.addNode(*node);
      Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
               (TFile, "VCG: add node [%s, %s] for DNA %d\n",
                       title, label, dna->Dna_idx()));
    }
  }

  return &info;
}

// =============================================================================
// IPSA_CG_VCG::Add_rbc_dna()
// =============================================================================
IPSA_CG_VCG::NODE_INFO *
IPSA_CG_VCG::Add_rbc_dna(VCGGraph &vcg, DNA_NODE *dna)
{
  Is_True(dna != NULL && !dna->Non_functional() &&
          dna->Is_set_rbc_flag(DNA_RBC_GLOBAL_USED|DNA_RBC_DEFINE_CALL),
          ("invalid dna node"));

  NODE_INFO& info = _dna_info[dna->Dna_idx()];
  Is_True(!info.Is_null(), ("dna not yet added"));
  Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
           (TFile, "VCG: ASM func [%s, %s]\n",
            info.Title(), dna->Fname()));

  // processing RBC annotation
  DNODE_VECTOR *rbc_nodes = _ipsa->Rbc()->Get_rbc_nodes(dna);
  if (rbc_nodes != NULL) {
    DNODE_VECTOR::const_iterator iter = rbc_nodes->begin();
    for (; iter != rbc_nodes->end(); iter++) {
      DNA_NODE *rbc_callee = *iter;
      if (rbc_callee == NULL ||
          !rbc_callee->Is_set_rbc_flag(DNA_RBC_GLOBAL_USED|DNA_RBC_DEFINE_CALL))
        continue;
      CONTEXT_SWITCH ctx(rbc_callee);

      RNODE_VECTOR *rna_list = rbc_callee->Call_list();
      for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
        RNA_NODE *rna = (*rna_list)[i];
        if (!rna->Is_flag_set(RBC_SE_GLOBAL_USED|RBC_SE_DEFINE_CALL))
          continue;

        CODEREP *vcall = (*rna_list)[i]->Callstmt()->Rhs();
        // offset "this" parameter
        UINT32 offset = 0;
        if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
          offset = 1;
        }

        // #1 name
        CODEREP *st_cr = vcall->Get_opnd(0 + offset);
        Is_True(st_cr != NULL, ("RBC ERROR: st name is empty.\n"));
        if (st_cr == NULL)
          return NULL;
        char *name = _ipsa->Rbc()->Find_const_char(dna, st_cr);
        Is_True(name != NULL, ("RBC ERROR: st name is empty.\n"));
        if (name == NULL)
          return NULL;
        EXT_INFO_MAP::iterator it = _name_info_map->find(name);
        if (it == _name_info_map->end())
          return NULL;
        NODE_INFO *st_info = it->second;

        // #2 file name
        CODEREP *file_cr = vcall->Get_opnd(1 + offset);
        Is_True(file_cr != NULL, ("RBC ERROR: file name is empty.\n"));
        if (file_cr == NULL)
          return NULL;
        char *fname = _ipsa->Rbc()->Find_const_char(dna, file_cr);
        Is_True(fname != NULL, ("RBC ERROR: file name is empty.\n"));
        if (fname == NULL)
          return NULL;

        // #3 def line number
        CODEREP *def_line_cr = vcall->Get_opnd(2 + offset);
        Is_True(def_line_cr != NULL, ("RBC ERROR: line is empty.\n"));
        if (def_line_cr == NULL)
          return NULL;
        UINT64 def_line = 0;
        def_line_cr = def_line_cr->Ilod_base();
        if (def_line_cr != NULL && def_line_cr->Kind() == CK_CONST)
          def_line = def_line_cr->Const_val();

        // #4 call line number
        CODEREP *call_line_cr = vcall->Get_opnd(3 + offset);
        Is_True(call_line_cr != NULL, ("RBC ERROR: line is empty.\n"));
        if (call_line_cr == NULL)
          return NULL;
        UINT64 call_line = 0;
        call_line_cr = call_line_cr->Ilod_base();
        if (call_line_cr != NULL && call_line_cr->Kind() == CK_CONST)
          call_line = call_line_cr->Const_val();

        // update node info according to annotations
        const char *new_label = Get_label(dna->Fname(), fname, def_line);
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: update label [%s, %s] => [%s, %s]\n",
                  info.Title(), info.Label(), info.Title(), new_label));
        info.Update_label(new_label);
        // add edge & callby
        SRCPOS spos = 0;
        SRCPOS_filenum(spos) = dna->File_idx();
        SRCPOS_linenum(spos) = call_line;
        EDGE_STYLE style = rna->Is_flag_set(RBC_SE_GLOBAL_USED) ? ACCESS_STYLE : CALL_STYLE;
        if (st_info->Add_clby(&info, style, spos) == TRUE) {
          Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                   (TFile, "VCG: function %s %s %s in ASM\n",
                    info.Label(),
                    style == ACCESS_STYLE ? "access" : "call",
                    st_info->Label()));
          Add_edge(vcg, info.Title(), st_info->Title(), style, spos);
          Add_cg_caller(vcg, dna);
        }
      }
    }
  }
  return &info;
}

// =============================================================================
// IPSA_CG_VCG::Build_cg_vcg()
// Build VCG for IPSA Call Graph
// =============================================================================
void
IPSA_CG_VCG::Build_cg_vcg()
{
  UINT32 dna_cnt = _ipsa->Dna_count();
  Is_True(dna_cnt > 0, ("no dna node"));
  Is_True(_dna_info != NULL, ("_dna_info not initialized"));
  Is_True(_st_info_map != NULL, ("_st_info_map not initialized"));
  Is_True(_ext_info_map != NULL, ("_ext_info_map not initialized"));
  Is_True(_name_info_map != NULL, ("_name_info_map not initialized"));
  Is_True(_info_visited != NULL, ("_info_visited not initialized"));
  Is_True(_dna_visited != NULL, ("_dna_visited not initialized"));

  VCGGraph vcg("Call Graph");
  // bottom-up traversal DNA and construct the VCG
  for (DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> iter(_ipsa);
       !iter.Is_end(); iter.Next()) {
    DNA_NODE *dna = iter.Current();
    Is_True(dna != NULL, ("invalid dna node"));
    if (dna->Non_functional())
      continue;

    Add_dna(vcg, dna, FALSE);
  }

  // add annotated info to the VCG
  for (DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> iter(_ipsa);
       !iter.Is_end(); iter.Next()) {
    DNA_NODE *dna = iter.Current();
    Is_True(dna != NULL, ("invalid dna node"));
    if (dna->Non_functional())
      continue;
    if (!dna->Is_set_rbc_flag(DNA_RBC_GLOBAL_USED|DNA_RBC_DEFINE_CALL))
      continue;
    Add_rbc_dna(vcg, dna);
  }

  // emit to file
  char filename[1024];
  snprintf(filename, 1024, "%s_cg.vcg", Fname());
  FILE *vcgfile = fopen(filename, "w");
  if (vcgfile) {
    vcg.emit(vcgfile);
    fclose(vcgfile);
  }
}


// =============================================================================
// IPSA_CG_VCG::Add_clby_vcg()
// =============================================================================
void
IPSA_CG_VCG::Add_clby_vcg(VCGGraph &vcg, NODE_INFO *info, BOOL is_sym)
{
  Is_True(info != NULL && info->Is_valid(), ("invalid info"));

  if (Visited(info))
    return;

  const CALLBY_MAP *clby = info->Clby();
  Is_True(clby != NULL, ("invalid call-by"));
  for (CALLBY_MAP::const_iterator it = clby->begin();
       it != clby->end(); ++it) {
    NODE_INFO *caller = (NODE_INFO *)it->first;
    Is_True(caller != NULL && caller->Is_valid(),
            ("invalid caller info"));

    IDTYPE_SET *acc_core_set = info->Access_core_set();
    IDTYPE_SET *caller_core_set = caller->Access_core_set();
    // do filter on one core access symbols only
    if (VSA_Cd_Filter && acc_core_set != NULL && acc_core_set->size() == 1) {
      const char *caller_name = caller->Label();
      const char *fname_end = strstr(caller_name, "\\n");
      const char *str_core = strstr(caller_name, "Core");
      IDTYPE_SET::const_iterator iter = acc_core_set->begin();
      IDTYPE core_id = *iter;
      if (str_core == NULL || str_core > fname_end) {
        str_core = strstr(caller_name, "core");
      }
      if (str_core >= caller_name && str_core < fname_end) {
        // if it's Core/core related function
        INT fname_sz = strlen(str_core) - strlen(fname_end);
        if (fname_sz > 4)
          str_core += 4;
        char c_core_id = '0' + core_id;
        // if core ID mismatches, skip
        if (str_core[0] != c_core_id) {
          Is_Trace(Get_Trace(TP_VSA, VSA_VCG_DUMP_FLAG),
                   (TFile, "VCG: skip caller %s due to access core ID %d\n",
                    caller_name, core_id));
          continue;
        }
      }
      // assume core ID set is collected on symbols but not functions
      if (caller_core_set != NULL && caller_core_set->empty()) {
        caller->Set_access_core_set(acc_core_set);
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: propagate access core ID %d to caller %s\n",
                  core_id, caller_name));
      }
    }

    EDGE_STYLE style = (EDGE_STYLE)it->second.Style();
    SRCPOS spos = it->second.Spos();
    Add_edge(vcg, caller->Title(), info->Title(), style, spos);

    Add_clby_vcg(vcg, caller, FALSE);

    // reset caller's access core ID set info
    if (VSA_Cd_Filter && acc_core_set != NULL && acc_core_set->size() == 1) {
      if (caller_core_set != NULL && caller_core_set->empty()) {
        caller->Set_access_core_set(caller_core_set);
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: reset caller %s access core ID set\n",
                  caller->Label()));
      }
    }

    VCGNode *node = Build_node(caller->Title(), caller->Label(),
                               (NodeShape)FUNC_SHAPE);
    vcg.addNode(*node);
    Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
             (TFile, "VCG: add node [%s, %s] for defined function\n",
                     caller->Title(), caller->Label()));
    if (node->backGroundColor() == LightGreen) {
      Increase_root_count();
    }
    else if (caller->Is_addr_taken()) {
      node->backGroundColor(White);
    }
  }
  if (clby->empty()) {
    if (!Is_root(info->Label())) {
      Set_has_top_not_root();
      if (info->Is_addr_taken()) {
        Set_has_top_not_root_addr_taken();
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: function %s: no caller, not root, address taken\n",
                  info->Label()));
      }
      else {
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: function %s: no caller, not root, not address taken\n",
                  info->Label()));
      }
    }
  }
}

// =============================================================================
// IPSA_CG_VCG::Build_sym_vcg()
// Build VCG for CG on each global symbol
// =============================================================================
void
IPSA_CG_VCG::Build_sym_vcg()
{
  UINT32 dna_cnt = _ipsa->Dna_count();
  Is_True(dna_cnt > 0, ("no dna node"));
  Is_True(_dna_info != NULL, ("_dna_info not initialized"));
  Is_True(_st_info_map != NULL, ("_st_info_map not initialized"));
  Is_True(_ext_info_map != NULL, ("_ext_info_map not initialized"));
  Is_True(_name_info_map != NULL, ("_name_info_map not initialized"));
  Is_True(_info_visited != NULL, ("_info_visited not initialized"));
  Is_True(_dna_visited != NULL, ("_dna_visited not initialized"));

  INT var_idx = 0;
  for (ST_INFO_MAP::iterator iter = _st_info_map->begin();
       iter != _st_info_map->end(); ++iter) {
    // reset visited flag
    Reset_visited();

    uint64_t key = iter->first;
    UINT32 file_id = key >> 32;
    UINT32 st_id = (UINT32)key;
    ST *st = St_ptr(file_id, st_id);
    Is_True(st && ST_level(st) == GLOBAL_SYMTAB &&
            ST_sclass(st) != SCLASS_EXTERN,
            ("invalid st"));

    NODE_INFO *info = iter->second;
    Is_True(info != NULL && info->Is_valid(), ("invalid info"));
    if (info->Title()[0] != VAR_NODE)
      continue;

    const char *st_label = info->Label();
    Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
             (TFile, "VCG: start build call graph for symbol %s\n", st_label));

    char graphname[1024];
    snprintf(graphname, 1024, "Call graph for %s", st_label);
    VCGGraph vcg(graphname);;

    Add_clby_vcg(vcg, info, TRUE);

    VCGNode *node = Build_node(info->Title(), st_label, (NodeShape)VAR_SHAPE);
    vcg.addNode(*node);
    Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
             (TFile, "VCG: add node [%s, %s] for global variable\n",
                     info->Title(), st_label));

    // emit to file
    char filename[1024];
    const char *st_name = ST_name(file_id, st);
    Is_True(st_name && st_name[0] != '\0', ("bad st name"));
    const char *g_fname = Get_file_name(file_id, ST_Srcpos(*st));
    if (g_fname == NULL || strcmp("<UNKNOWN>", g_fname) == 0)
      g_fname = Fname();
    if (st_name[0] == '_' && st_name[1] == 'Z') {
      // c++ name, ignore var name
      snprintf(filename, 1024, "%s_var_%d.vcg", g_fname, ++var_idx);
    }
    else {
      snprintf(filename, 1024, "%s_%s_%d.vcg", g_fname, st_name, ++var_idx);
    }
    FILE *vcgfile = fopen(filename, "w");
    if (vcgfile) {
      vcg.emit(vcgfile);
      fclose(vcgfile);
    }
    // classify problems
    if (Root_count() > 1) {
      Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
               (TFile, "VCG: M_P for %s\n", filename)); // multiple path
    }
    else if (Root_count() == 1) {
      if (Has_top_not_root()) {
        if (Has_top_not_root_addr_taken()) {
          Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                   (TFile, "VCG: UD_P for %s\n", filename)); // undecidable path
        }
        else {
          Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                   (TFile, "VCG: UR_S_P for %s\n", filename)); // unreachable + single path
        }
      }
      else {
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: S_P for %s\n", filename)); // single path
      }
    }
    else {
      if (Has_top_not_root_addr_taken()) {
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: UD_P for %s\n", filename)); // undecidable path
      }
      else {
        Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
                 (TFile, "VCG: UR_UD_P for %s\n", filename)); // unreachable + undecidable path
      }
    }
    Reset_has_top_not_root();
    Reset_has_top_not_root_addr_taken();
    Reset_root_count();
  }
}

// =============================================================================
// IPSA_CG_VCG::Build_vcg()
// Build VCG for IPSA Call Graph and Global Variables
// =============================================================================
void
IPSA_CG_VCG::Build_vcg()
{
  UINT32 dna_cnt = _ipsa->Dna_count();
  if (dna_cnt <= IPSA::DNA_INIT_IDX)
    return;

  // dump call graph
  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VCG_DUMP_FLAG),
               _ipsa->Print<DNA_TRAV_TOP_DOWN_ORDER>(TFile));

  // initialize data strucutres
  Initialize();

  // build vcg for whole call graph
  Is_Trace(Get_Trace(TP_VSA, VSA_VCG_TRACE_FLAG),
           (TFile, "VCG: start build call graph\n"));
  Build_cg_vcg();

  // dump node info
  Is_Trace_cmd(Get_Trace(TP_VSA, VSA_VCG_DUMP_FLAG),
               Print_node_info(TFile));

  // build vcg for each global variable
  Build_sym_vcg();
}

// =============================================================================
// Print node info for debug purpose
// =============================================================================
void
IPSA_CG_VCG::Print_node_info(FILE *fp) const
{
  fprintf(fp, "=============================================================================\n");
  // print DNA node info
  fprintf(fp, "DNA node info:\n");
  for (INT i = IPSA::DNA_INIT_IDX; i < _ipsa->Dna_count(); ++i) {
    const NODE_INFO &info = _dna_info[i];
    fprintf(fp, " DNA[%d]: %s ", i, _ipsa->Get_dna(i)->Fname());
    info.Print(fp);
  }
  // print ST node info
  fprintf(fp, "ST node info:\n");
  for (ST_INFO_MAP::const_iterator it = _st_info_map->begin();
       it != _st_info_map->end(); ++it) {
    uint64_t key = it->first;
    const NODE_INFO *info = it->second;
    UINT32 file_idx = key >> 32;
    UINT32 st_idx = (UINT32)key;
    ST *st = St_ptr(file_idx, st_idx);
    fprintf(fp, " %s[%d:%x]: %s ",
            ST_class(st) == CLASS_FUNC ? "FUNC" : "VAR",
            file_idx, st_idx, ST_name(file_idx, st_idx));
    info->Print(fp);
  }
  // print EXT ST node info
  fprintf(fp, "External ST info:\n");
  for (EXT_INFO_MAP::const_iterator it = _ext_info_map->begin();
       it != _ext_info_map->end(); ++it) {
    const NODE_INFO *info = it->second;
    fprintf(fp, " %s: ", it->first);
    info->Print(fp);
  }
  // print name node info
  fprintf(fp, "Name node info:\n");
  for (EXT_INFO_MAP::const_iterator it = _name_info_map->begin();
       it != _name_info_map->end(); ++it) {
    const NODE_INFO *info = it->second;
    fprintf(fp, " %s: ", it->first);
    info->Print(fp);
  }
  fprintf(fp, "=============================================================================\n");
}

// =============================================================================
// Print call graph to VCG
// =============================================================================
void
IPSA::Print_cg_vcg(const char *fname)
{
  IPSA_CG_VCG vcg(this, fname);
  vcg.Build_vcg();
}
