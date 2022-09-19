/*
   Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

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

#ifndef whirl_file_mgr_h
#define whirl_file_mgr_h "whirl_file_mgr.h"

// Manage all input whirl files
// ========
// Used by whirl ld plugin for cross file analysis
// File as added in ld-plugin claim_file_handler
// Symbols are extracted and added to ld for symbol resolution
// WHIRL sections are read into memory for future use
// Mappings are set up on-demand to map extern symbols to definitions in other files.
// API provided for cross file context switch and
// retrieve ST/TY/etc cross file by given file id and st_idx/ty_idx etc

#include <defs.h>
#include <stdint.h>
#include <stdio.h>
#include <vector>
#include "glob.h"
#include "plugin_defs.h"
#include "phase.h"
#include "whirl_file_ctx.h"
#include <ext/hash_map>

using __gnu_cxx::hash_map;

class WHIRL_FILE_MANAGER;

class LD_PLUGIN_OPTIONS {
private:
  // TODO: ld-plugin options
  const char *_libpath;

public:
  LD_PLUGIN_OPTIONS()
    : _libpath(NULL)
  {
  }

public:
  const char* Libpath() const        { return _libpath; }
  void Set_libpath(const char* path) { _libpath = path; }
};

class ST_ID_MAPPING {
private:
  ld_plugin_symbol *_ld_syms;   // .*:          ld_plugin_symbol array
  UINT32           *_ld_st_map; // .ir, .a>.ir: ld sym index -> whirl ST_IDX
                                // .a, .a>.o:   unused
  UINT32           *_st_ld_map; // .ir, .a>.ir: whirl ST_IDX -> ld sym index
                                // .a:          ld sym offset of each file in .a
                                // .a>.a:       unused
  UINT32            _ld_count;  // .*:          number of entries in ld_plugin_symbol
  UINT32            _st_count;  // .ir, .a>.ir: number of whirl syms
                                // .a:          num of files in .a
                                // .a>.o:       unused
  UINT32            _st_predef; // .ir, .a>.ir: number of predefined whirl syms
                                // .a, .a>.o:   the max number of _ld_syms

public:
  ST_ID_MAPPING() : _ld_syms(NULL), _ld_st_map(NULL), _st_ld_map(NULL),
                    _ld_count(0), _st_count(0), _st_predef(0) {
  }

  ~ST_ID_MAPPING() {
  }

public:
  ld_plugin_symbol *Get_ld_symbol() const {
    return _ld_syms;
  }

  UINT32 Get_ld_count() const {
    return _ld_count;
  }

  void Set_ld_count(UINT32 count) {
    _ld_count = count;
  }

  UINT32 Get_st_count() const {
    return _st_count;
  }

  const ld_plugin_symbol *Get_ld_symbol(UINT32 ld_index) const {
    return &(_ld_syms[ld_index]);
  }

  // for whirl ir only, st_index is the ST_index(ST*)
  UINT32 Get_ld_index(UINT32 st_index) const {
    Is_True(st_index >= _st_predef && st_index < _st_predef + _st_count,
            ("st_index out of range"));
    return _st_ld_map[st_index - _st_predef];
  }

  // for .a only, to get ld sym offset for a file
  // index is the order in .a file
  UINT32 Get_ld_offset(UINT32 index) const {
    Is_True(index <= _st_count,
            ("file index out of range"));
    return _st_ld_map[index];
  }

  // for whirl ir only
  UINT32 Get_st_index(UINT32 ld_index) const {
    Is_True(ld_index < _ld_count,
            ("ld_index out of range"));
    return _ld_st_map[ld_index];
  }

public:
  // handle whirl symtab
  bool Process_whirl_symtab();

  // handle elf symtab, only used for elf file in archive
  bool Process_elf_symtab();

  // merge mapping from whirl/elf file in archive to archive ld symtable
  void Merge(const ST_ID_MAPPING& mapping);

public:
  // initialize mapping for archive
  void Init_for_archive(UINT32 num_of_ld, UINT32 num_of_file);

  // initialize mapping for elf
  void Init_for_elf(UINT32 num_of_ld);

}; /* ST_ID_MAPPING */

class WHIRL_FILE_INFO {
  friend class WHIRL_FILE_MANAGER;
  friend class IPSA_COMP_DRIVER;
private:
  WHIRL_FILE_CONTEXT _context;
  ST_ID_MAPPING      _mapping;
  void              *_lhandle;  /* ld plugin handle */
  void              *_fhandle;  /* memory mapped file handle */
  elf_symtab_info    _symtab;
  elf_strtab_info    _strtab;
  char              *_filename;
  size_t             _filesize;
  uint32_t           _file_idx;
  uint32_t           _parent_idx; /* archive file index for embedded object or IR file */
  input_file_type    _file_type;  /* whirl or object, etc */
  std::vector<UINT>  _file_no_map;/* map local file no to global file no */

public:
  WHIRL_FILE_INFO(void *l, void *h, char *name, size_t size, uint32_t file_id,
                  uint32_t parent_id, input_file_type type)
    : _lhandle(l), _fhandle(h),
      _filename(name), _filesize(size),
      _file_idx(file_id), _parent_idx(parent_id), _file_type(type) {
  }

  WHIRL_FILE_INFO()
    : _lhandle(NULL), _fhandle(NULL), _filename(NULL), _filesize(0),
      _file_idx(UINT32_MAX), _parent_idx(UINT32_MAX), _file_type(file_unknown) {
  }

  ~WHIRL_FILE_INFO();

  // free fhandle and syms
  void Clean();

  // get file name
  const char* File_name() const {
    return _filename;
  }

  // get WHIRL FILE_INFO
  const FILE_INFO& Whirl_file_info() const {
    return *_context._file_info;
  }

  // get file index
  UINT32 File_index() const {
    return _file_idx;
  }

  // get file type
  input_file_type File_type() const {
    return _file_type;
  }

  // get parent's file index
  UINT32 Parent_index() const {
    return _parent_idx;
  }

  INT Get_global_file_num(INT filenum) const {
    Is_True(filenum > 0 && filenum < _file_no_map.size(),
            ("filenum %d out of bound %d", filenum, _file_no_map.size()));
    return _file_no_map[filenum];
  }

private:
  // get file context, only used by manager
  WHIRL_FILE_CONTEXT& Context() {
    return _context;
  }

  // get mapping, only used by manager
  ST_ID_MAPPING& Mapping() {
    return _mapping;
  }

private:
  // prepare source before load symtab and ir from memory
  void Prepare_file();

  // prepare buffer before read symtab & ir
  void Prepare_buffer();

  // Load symtab from memory mapped file
  bool Load_symtab();

  // process file table
  void Process_file_table(WHIRL_FILE_MANAGER* mgr);

  // Init file context
  void Create_file_context() {
    _context.Create_file_context();
  }
  // Save file context
  void Save_context() {
    _context.Save_context();
  }
  // Restore file context
  void Restore_context() const {
    _context.Restore_context();
  }

  void Update_context(BOOL syslib) {
    _context.Update_context(this, syslib);
  }

  void Save_be_scope_tab() {
    _context.Save_be_scope_tab();
  }

  // Load symtab and setup context
  void Setup(WHIRL_FILE_MANAGER* mgr) {
    Prepare_file();
    Create_file_context();
    Prepare_buffer();
    Restore_context();
    Load_symtab();
    Process_file_table(mgr);
    Save_context();
  }

public:
  // ld plugin only, implemented in ld_plugin/whirl_utils.cxx

  // summerize symtab in archive and add symbols to linker
  uint32_t Summerize_symtab(WHIRL_FILE_MANAGER& mgr);
  // process elf symbols and add to linker
  bool Process_elf_symtab(WHIRL_FILE_MANAGER& mgr);
  // process whirl symbols and add to linker if there is no elf symtab
  bool Process_whirl_symtab(WHIRL_FILE_MANAGER& mgr);
  // update symtab when all symbols are read
  bool Update_symtab(WHIRL_FILE_MANAGER& mgr);
  // dump symtab resolution results
  void Dump_symtab(FILE* fp) const;

public:
  // dump ir and symtab
  void Dump_irb(FILE* fp) const;

  // verify symtab
  void Verify_symtab() const;

public:
  char    *Str_ptr(STR_IDX idx) const
  {
    return Strtab_to_char(_context._str_tab, idx);
  }

  PU      *Pu_ptr(PU_IDX idx) const
  {
    return &(*_context._pu_tab)[idx];
  }
  ST      *St_ptr(ST_IDX idx) const
  {
    Is_True(ST_IDX_level(idx) == GLOBAL_SYMTAB,
            ("only work for global symtab"));
    return &(_context._scope_tab[GLOBAL_SYMTAB].st_tab->Entry(ST_IDX_index(idx)));
  }
  TY      *Ty_ptr(TY_IDX idx) const
  {
    return &(*_context._ty_tab)[TY_IDX_index(idx)];
  }
  FLD     *Fld_ptr(FLD_IDX idx) const
  {
    return &(*_context._fld_tab)[idx];
  }
  TYLIST  *Tylist_ptr(TYLIST_IDX idx) const
  {
    return &(*_context._tylist_tab)[idx];
  }
  ARB     *Arb_ptr(ARB_IDX idx) const
  {
    return &(*_context._arb_tab)[idx];
  }
  TCON    *Tcon_ptr(TCON_IDX idx) const
  {
    return &(*_context._tcon_tab)[idx];
  }
  char    *Tcon_str_ptr(UINT32 idx) const
  {
    return TCON_strtab_to_char(_context._tcon_str_tab, idx);
  }
  INITV   *Initv_ptr(INITV_IDX idx) const
  {
    return &(*_context._initv_tab)[idx];
  }
  INITO   *Inito_ptr(INITO_IDX idx) const
  {
    Is_True(INITO_IDX_level(idx) == GLOBAL_SYMTAB,
            ("only work for global symtab"));
    return &(_context._scope_tab[GLOBAL_SYMTAB].inito_tab->Entry(INITO_IDX_index(idx)));
  }
  ST_ATTR *St_attr_ptr(ST_ATTR_IDX idx) const
  {
    return &(_context._scope_tab[GLOBAL_SYMTAB].st_attr_tab->Entry(idx));
  }
  BLK     *Blk_ptr(BLK_IDX idx) const
  {
    return &(*_context._blk_tab)[idx];
  }
  // tab access
  INITO_TAB *Inito_tab() const
  {
    return _context._scope_tab[GLOBAL_SYMTAB].inito_tab;
  }
  INITV_TAB *Initv_tab() const
  {
    return _context._initv_tab;
  }
  TCON_TAB* Tcon_tab() const
  {
    return _context._tcon_tab;
  }
  INITO_IDX St_inito(ST_IDX st_idx) const
  {
    const ST_INITO_MAP *st_inito_map = _context.Get_st_inito_map();
    if (st_inito_map->find(st_idx) == st_inito_map->end()) {
      return (INITO_IDX) 0;
    }
    return st_inito_map->find(st_idx)->second;
  }
}; /* WHIRL_FILE_INFO */


class RULESET_INFO {
private:
  const char* _name;
  const char* _version;

  //RULESET_INFO(const RULESET_INFO&);            // disable copy ctor
  //RULESET_INFO& operator=(const RULESET_INFO&); // disable assign operator

public:
  RULESET_INFO(const char* name, const char* version)
   : _name(name), _version(version) { }

  const char* Name() const    { return _name;    }
  const char* Version() const { return _version; }
};

struct filename_euqal {
  bool operator()(const char *s1, const char *s2) const {
    return strcmp(s1, s2) == 0;
  }
};

class WHIRL_FILE_MANAGER {
private:
  typedef std::vector<RULESET_INFO> RULESET_TABLE;
  typedef std::vector<const char*> FILE_TABLE;
  typedef hash_map<const char*, UINT,
                   __gnu_cxx::hash<const char *>, filename_euqal> FILE_MAP;

private:
  plugin_callback              _ldpc;    /* ld plugin callbacks */
  LD_PLUGIN_OPTIONS            _opts;    /* options */
  std::vector<char*>           _args;    /* arguments */
  std::vector<char*>           _libs;    /* libraries */
  std::vector<WHIRL_FILE_INFO> _files;
  hash_map<uint64_t, uint64_t> _mapping; /* extern symbol mapping */
  FILE_TABLE                   _file_table; /* global file table */
  FILE_MAP                     _file_map;   /* helper map to create file table */
  RULESET_TABLE                _ruleset_table;  /* ruleset table */
  const char                  *_outfile;    /* output file name */

  static WHIRL_FILE_MANAGER   *_instance;

public:
  // get the unique instance without internal check
  // the caller to check the return value for cross-file analysis
  static WHIRL_FILE_MANAGER* Get()
  {
    return _instance;
  }

  // get the unique instance with internal check
  static WHIRL_FILE_MANAGER* Instance()
  {
    Is_True(_instance != NULL, ("WHIRL_FILE_MANAGER not initialized"));
    return _instance;
  }

  // create the unique instance
  static WHIRL_FILE_MANAGER* Create()
  {
    Is_True(_instance == NULL, ("WHIRL_FILE_MANAGER already initialized"));
    _instance = new WHIRL_FILE_MANAGER();
    Is_True(_instance != NULL, ("WHIRL_FILE_MANAGER out of memory"));
    return _instance;
  }

  // generate a unique key from (file, st)
  static uint64_t Encode_file_st(UINT32 file, UINT32 st)
  {
    return ((uint64_t)file << 32) | st;
  }

  // decode the unique key to (file, st)
  static void Decode_file_st(uint64_t key, UINT32& file, UINT32& st)
  {
    file = key >> 32;
    st   = (UINT32)key;
  }

public:
  // constructor
  WHIRL_FILE_MANAGER() : _outfile(NULL)
  {
  }

  // cleann all buffers
  void Clean();

  const char * Get_output_file() {
    return _outfile;
  }

  vector<WHIRL_FILE_INFO>& Get_files() {
    return _files;
  }

  // get file_info reference, idx starts from 1
  WHIRL_FILE_INFO& Get_file(UINT32 idx) {
    Is_True(idx > 0 && idx <= _files.size(), ("file index out of bound"));
    return _files[idx - 1];
  }

  // get file count
  UINT32 Get_file_count() const {
    return _files.size();
  }

  // register file
  UINT32 Register_file(const char* fname) {
    FILE_MAP::iterator it = _file_map.find(fname);
    if (it != _file_map.end())
      return it->second;  // return file id
    const char* fndup = strdup(fname);
    _file_table.push_back(fndup);
    UINT32 id = _file_table.size(); // reserve id 0
    _file_map[fndup] = id;
    return id;
  }

  // Get entry count of global file table
  UINT32 Get_file_table_size() const
  {
    return _file_table.size() + 1;
  }

  // get file name by global file table id
  const char* Get_file_name(UINT id) const
  {
    Is_True(id >= 1 &&
            id < _file_table.size() + 1, ("invalid file id %d", id));
    return id ? _file_table[id - 1] : NULL;
  }

  const vector<char*> *Get_args() const
  {
    return &_args;
  }

  const vector<char*> *Get_libs() const
  {
    return &_libs;
  }

  const char* Get_libpath() const
  {
    return _opts.Libpath();
  }

  UINT32 Get_ruleset_count() const
  {
    return _ruleset_table.size();
  }

  const RULESET_INFO& Get_ruleset(UINT id) const
  {
    Is_True(id >= 0 && id < Get_ruleset_count(), ("invalid ruleset id %d", id));
    return _ruleset_table[id];
  }

public:
  // ld plugin only, implemented in ld_plugin/whirl_utils.cxx
  plugin_callback& callback() {
    return _ldpc;
  }

  // global initialization, called when ld-plugin onload is called
  void Initialize();

  // set output file name from linker plugin call back
  void Set_output_name(const char *str);

  // add an command line option, called when ld-plugin option is called
  void Add_arg(const char *arg) {
    // TODO, handle ld_plugin args here and pass the rest to backend
    // need strdup() ???
    if (strncmp(arg, "libpath=", 8) == 0) {
      _opts.Set_libpath(arg + 8);
      return;
    } else if(strncmp(arg, "-IPSA:-l", 8) == 0) {
      const char *library = arg + strlen("-IPSA:");
      _libs.push_back(strdup(library));
      return;
    } else if (strncmp(arg, "-TARG:abi=", 10) == 0) {
      if (strcmp(arg + 10, "n32") == 0) {
        Target_ABI = ABI_n32;
      } else if (strcmp(arg + 10, "n64") == 0) {
        Target_ABI = ABI_n64;
      }
    }
    if (strcmp(arg, "-sw") == 0) {  // MASTIFF-OPT: "-show" --> "-sw"
      // turn off for now, need another option to control, too many debug info
      // verbose = true;
    }
    _args.push_back(const_cast<char *>(arg));
  }

  // add a ruleset
  BOOL   Add_ruleset(const char* name, const char* version) {
    RULESET_TABLE::iterator end = _ruleset_table.end();
    for (RULESET_TABLE::iterator it = _ruleset_table.begin();
         it != end; ++it) {
      if (strcmp(it->Name(), name) == 0)
        return FALSE;
    }
    _ruleset_table.push_back(RULESET_INFO(name, version));
    return TRUE;
  }

  // check if a ruleset name is valid
  BOOL   Is_valid_ruleset(const char* name) {
    RULESET_TABLE::iterator end = _ruleset_table.end();
    for (RULESET_TABLE::iterator it = _ruleset_table.begin();
         it != end; ++it) {
      if (strcmp(it->Name(), name) == 0)
        return TRUE;
    }
    return FALSE;
  }

  // add a whirl file
  UINT32 Add_file(void *lhandle, void *fhandle, char *name, size_t size,
                  UINT32 parent_id, input_file_type type);

  // clean files when archive doesn't contain whirl symtab
  void Clean_files(UINT32 ar_file_index);

  // update symtab when all symbols are read
  bool Update_symtab();

  // dump symbol resolution result
  void Dump_symtab(FILE* fp) const;

public:
  // Set file scope context to file_id's context
  void Set_file_context(UINT32 file_id)
  {
    Get_file(file_id).Restore_context();
  }

  // Save be scope tab to file_id's context
  void Save_be_scope_tab(UINT32 file_id)
  {
    Get_file(file_id).Save_be_scope_tab();
  }

  // resolve symbol from ref(file, st_idx) to def(file, st_idx)
  BOOL Resolve(UINT32 ref_file, ST_IDX ref_st_idx, UINT32& def_file, ST_IDX& def_st_idx) const
  {
    UINT32 ref_st = ST_IDX_index(ref_st_idx);
    uint64_t key = Encode_file_st(ref_file, ref_st);
    hash_map<uint64_t, uint64_t>::const_iterator it = _mapping.find(key);
    if (it == _mapping.end())
      return FALSE;
    UINT32 def_st; 
    Decode_file_st(it->second, def_file, def_st);
    def_st_idx = make_ST_IDX(def_st, GLOBAL_SYMTAB);
    return TRUE;
  }

  // add mapping from ref(file, st_idx) to def(file, st_idx)
  void Add_symbol(UINT32 ref_file, ST_IDX ref_st_idx, UINT32& def_file, ST_IDX& def_st_idx)
  {
    uint64_t key = Encode_file_st(ref_file, ST_IDX_index(ref_st_idx));
    uint64_t value = Encode_file_st(def_file, ST_IDX_index(def_st_idx));
    _mapping.insert(std::make_pair(key, value));
  }

  // dump whirl ir & symtab
  void Dump_irb(FILE* fp) const;

  //verify symtab
  void Verify_symtab() const;

private:
  // update symbol mapping when all symbols are resolved
  bool Update_mapping();

  // set backend parameters and load components
  void Load_components();

  // initialize the backend, called after all symbols are resolved
  void Initialize_backend();

public:
  // Backend driver
  bool Backend_process();

};


class FILE_CONTEXT_SWITCH {
private:
  UINT32 _file_idx;

public:
  FILE_CONTEXT_SWITCH(UINT32 file_idx)
  {
    _file_idx = File_Index;
    if (file_idx == 0 || file_idx == File_Index)
      return;
    WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
    // file ctx may be changed druing preorder process pu
    // save be scope tab before switch
    mgr->Save_be_scope_tab(_file_idx);
    // switch to new file context
    mgr->Set_file_context(file_idx);
    File_Index = file_idx;
  }

  ~FILE_CONTEXT_SWITCH()
  {
    if (_file_idx == 0 || _file_idx == File_Index)
      return;
    WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
    mgr->Set_file_context(_file_idx);
    File_Index = _file_idx;
  }
};

#endif /* whirl_file_mgr_h */
