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

#include "plugin_defs.h"
#include "whirl_file_mgr.h"
#include "defs.h"
#include <vector>
using namespace std;
#include "symtab.h"
#include "pu_info.h"
#include "wn.h"
#include "ir_bread.h"
#ifdef R_68K_NUM
#undef R_68K_NUM
#endif
#ifdef E_MIPS_ARCH_3
#undef E_MIPS_ARCH_3
#undef E_MIPS_ARCH_2
#undef E_MIPS_ARCH_1
#endif
#include "elf_stuff.h"
#include <sys/mman.h>

/*
 * helper function used by ld-plugin
 */

// map file into memory
void*
whirl_read_file(const struct ld_plugin_input_file *file) {
  // TODO: reuse map for archive ??
  if (file->fd < 0 || file->filesize <= 0 || file->offset >= file->filesize)
    return NULL;
  char* map_addr = (char *)::mmap(0, file->filesize, PROT_READ | PROT_WRITE,
                                  MAP_PRIVATE, file->fd, 0);
  return map_addr ? map_addr + file->offset : NULL;
}

// free memory mapped for the file
void whirl_release_file(void *fhandle, size_t length) {
  ::munmap(fhandle, length);
}

// get whirl version and file type for a memory mapped file
input_file_type
whirl_revision(void *fhandle, off_t size, char *revision) {
  revision[0] = '\0';
  if (WN_massage_input((char *)fhandle, size, revision) > 0)
    return file_whirl;
  if (memcmp(fhandle, "!<arch>", 6) == 0)
    return file_archive;
  if (memcmp(fhandle, "\177ELF", 4) == 0)
    return file_elf;
  else
    return file_unknown;
}

/*
 * ST_ID_MAPPING Implementations
 */
// ld-plugin symbol kind name
static const char* ldps_kind_name[] = {
  "def", "weakdef", "undef", "weakundef", "common"
};

// ld-plugin symbol visibility name
static const char* ldps_visibility_name[] = {
  "default", "protected", "internal", "hidden"
};

// ld-plugin symbol resolution name
static const char* ldps_resolution_name[] = {
  "unknown",
  "undef",
  "prevailing_def",
  "prevailing_def_ironly",
  "preempted_reg",
  "preempted_ir",
  "resolved_ir",
  "resolved_exec",
  "resolved_dyn",
  "prevailing_def_ironly_exp"
};

// convert elf symbol to ld-plugin visibility
static ld_plugin_symbol_visibility
elf_to_ldps_visibility(UINT vis) {
  switch (vis) {
  case STV_DEFAULT:   return LDPV_DEFAULT;
  case STV_INTERNAL:  return LDPV_INTERNAL;
  case STV_HIDDEN:    return LDPV_HIDDEN;
  case STV_PROTECTED: return LDPV_PROTECTED;
  default:
    FmtAssert(FALSE, ("unexpected visibility %d.", vis));
  }
}

// convert whirl symbol to ld-plugin visibility
static ld_plugin_symbol_visibility
whirl_to_ldps_visibility(ST* st) {
  switch(ST_export(st)) {
  case EXPORT_INTERNAL:  return LDPV_INTERNAL;
  case EXPORT_HIDDEN:    return LDPV_HIDDEN;
  case EXPORT_PROTECTED: return LDPV_PROTECTED;
  case EXPORT_PREEMPTIBLE:  // LDPV_DEFAULT
  case EXPORT_OPTIONAL:  return LDPV_DEFAULT;
  default:
    FmtAssert(FALSE, ("unexpected export %d.", ST_export(st)));
  }
}

// convert elf symbol to ld-plugin kind
static ld_plugin_symbol_kind
elf_to_ldps_kind(UINT type, UINT bind) {
  switch (type) {
  case STT_NOTYPE:
    return bind == STB_WEAK ? LDPK_WEAKUNDEF : LDPK_UNDEF;
  case STT_OBJECT:
  case STT_FUNC:
    return bind == STB_WEAK ? LDPK_WEAKDEF : LDPK_DEF;
  case STT_COMMON:
    return LDPK_COMMON;
  default:
    FmtAssert(FALSE, ("unexpected type %d.", type));
  }
}

// convert whirl symbol to ld-plugin kind
static ld_plugin_symbol_kind
whirl_to_ldps_kind(ST* st) {
  BOOL is_weak = (ST_export(st) == EXPORT_OPTIONAL) ||
                 ST_is_weak_symbol(st);
  switch (ST_sclass(st)) {
  case SCLASS_COMMON:
    return LDPK_COMMON;
  case SCLASS_EXTERN:
    return is_weak ? LDPK_WEAKUNDEF : LDPK_UNDEF;
  case SCLASS_TEXT:
  case SCLASS_UGLOBAL:
  case SCLASS_DGLOBAL:
    return is_weak ? LDPK_WEAKDEF : LDPK_DEF;
  default:
    FmtAssert(FALSE, ("unexpected sclass %d.", ST_sclass(st)));
  }
}

// convert all Elf32_Sym or Elf64_Sym to ld-plugin symbols
template<class Sym> static bool
process_symbol(const Sym *sym, UINT n, ld_plugin_symbol *ld_syms, char* strbuf, UINT& ld_sym_count) {
  UINT j = 0;
  for (INT i = 0; i < n; ++i) {
    DEBUG_OUTPUT(" -sym[%d] name=%s info=%d other=%d shndx=%d value=%ld size=%ld\n",
                 i,
                 strbuf + sym[i].st_name, sym[i].st_info,
                 sym[i].st_other, sym[i].st_shndx,
                 (long)sym[i].st_value, (long)sym[i].st_size);
    // st_info is unsigned char, there is no difference between
    // Elf32_Sym and Elf64_Sym for bind, type, info and visibility
    UINT bind = ELF32_ST_BIND(sym[i].st_info);
    if (bind == STB_LOCAL) {
      DEBUG_OUTPUT("  *ignore local symbol %s(bind=%d)\n",
                   strbuf + sym[i].st_name, bind);
      continue;
    }
    UINT type = ELF32_ST_TYPE(sym[i].st_info);
    // Hack: open64 ipl issue? UNDEF FUNC is not NOTYPE
    if (sym[i].st_shndx == SHN_UNDEF)
      type = STT_NOTYPE;
    if (type != STT_NOTYPE &&
        type != STT_OBJECT &&
        type != STT_FUNC &&
        type != STT_COMMON) {
      DEBUG_OUTPUT("  *ignore section or file symbol %s(type=%d)\n",
                   strbuf + sym[i].st_name, type);
      continue;
    }
    UINT vis = ELF32_ST_VISIBILITY(sym[i].st_other);
    ld_syms[j].name = strbuf + sym[i].st_name;
    ld_syms[j].version = NULL;     // TODO version??
    ld_syms[j].def = elf_to_ldps_kind(type, bind);
    ld_syms[j].visibility = elf_to_ldps_visibility(vis);
    ld_syms[j].size = sym[i].st_size;
    ld_syms[j].comdat_key = NULL;  // TODO: comdat??
    ld_syms[j].resolution = LDPR_UNKNOWN;
    DEBUG_OUTPUT("  *ld_sym[%d] %s def=%s vis=%s size=%ld\n",
                 j, ld_syms[j].name,
                 ldps_kind_name[ld_syms[j].def],
                 ldps_visibility_name[ld_syms[j].visibility],
                 ld_syms[j].size);
                 
    ++ j;
  }
  ld_sym_count = j;
  return true;
}

#if 0
// read elf symtab and convert to ld-plugin symbols
bool
ST_ID_MAPPING::Process_elf_symtab() {
  FmtAssert(FALSE, ("Not supported"));
  return false;
}
#endif

// read whirl global symtab and convert to ld-plugin symbols
bool
ST_ID_MAPPING::Process_whirl_symtab() {
  Is_True(_ld_syms == NULL &&
          _ld_st_map == NULL &&
          _st_ld_map == NULL, ("map already created"));

  ST* st;
  INT i, count = ST_Table_Size(GLOBAL_SYMTAB);
  // skip predefined preg st
  for (i = 1; i < count; ++i) {
    st = &St_Table(GLOBAL_SYMTAB, i);
    if (ST_sclass(st) != SCLASS_REG ||
        strncmp(ST_name(st), ".preg", 5) != 0)
      break;
  }
  _st_predef = i;

  // allocate arrays
  _st_count = count - i;
  _ld_syms = (ld_plugin_symbol *)malloc(
                 sizeof(ld_plugin_symbol) * _st_count);
  Is_True(_ld_syms != NULL, ("out of memory?"));
  memset(_ld_syms, 0, sizeof(ld_plugin_symbol) * _st_count);
  _ld_st_map = (UINT32 *)malloc(
                 sizeof(UINT32) * _st_count);
  Is_True(_ld_st_map != NULL, ("out of memory?"));
  memset(_ld_st_map, 0, sizeof(UINT32) * _st_count);
  _st_ld_map = (UINT32 *)malloc(
                 sizeof(UINT32) * _st_count);
  Is_True(_st_ld_map != NULL, ("out of memory?"));
  memset(_st_ld_map, 0, sizeof(UINT32) * _st_count);

  INT j = 0;
  // traverse symtab and fill in maps
  for (; i < count; ++i) {
    st = &St_Table(GLOBAL_SYMTAB, i);
    if (ST_is_odr(st) && ST_export(st) == EXPORT_LOCAL) {
      // change export class back to preemptible so that only
      // one definition can be picked up
      Set_ST_export(st, EXPORT_PREEMPTIBLE);
      Set_ST_is_weak_symbol(st);
    }

    // skip local symbols
    if (ST_export(st) == EXPORT_LOCAL ||
        ST_export(st) == EXPORT_LOCAL_INTERNAL)
      continue;
    // skip non-variable or non-function
    if (ST_sclass(st) != SCLASS_COMMON &&
        ST_sclass(st) != SCLASS_EXTERN &&
        ST_sclass(st) != SCLASS_UGLOBAL &&
        ST_sclass(st) != SCLASS_DGLOBAL &&
        ST_sclass(st) != SCLASS_TEXT)
      continue;
    DEBUG_OUTPUT(" -st[%d] name=%s class=%s sclass=%s export=%s size=%lld, weak=%d\n",
                 i, ST_name(st), Class_Name(ST_sym_class(st)),
                 Sclass_Name(ST_sclass(st)), Export_Name(ST_export(st)),
                 ST_size(st), ST_is_weak_symbol(st));
    // fill in ld symbol array
    _ld_syms[j].name = ST_name(st);
    _ld_syms[j].version = NULL;     // TODO: version??
    _ld_syms[j].def = whirl_to_ldps_kind(st);
    _ld_syms[j].visibility = whirl_to_ldps_visibility(st);
    _ld_syms[j].size = ST_size(st);
    _ld_syms[j].comdat_key = ST_is_odr(st) ? ST_name(st) : NULL;
    _ld_syms[j].resolution = LDPR_UNKNOWN;
    DEBUG_OUTPUT("  *ld_sym[%d] %s def=%s vis=%s size=%ld\n",
                 j, _ld_syms[j].name,
                 ldps_kind_name[_ld_syms[j].def],
                 ldps_visibility_name[_ld_syms[j].visibility],
                 _ld_syms[j].size);
    // setup ld sym id to whirl st_idx map
    _ld_st_map[j] = i;
    // setup whirl st_idx to ld sym id map
    _st_ld_map[i - _st_predef] = j;
    ++ j;
  }
  _ld_count = j;
}

void
ST_ID_MAPPING::Merge(const ST_ID_MAPPING& mapping) {
  Is_True(_st_ld_map[_st_count] == 0, ("archive file number overflow."));
  Is_True(_ld_count + mapping.Get_ld_count() <= _st_predef,
          ("archive ld sym overflow."));
  _st_ld_map[_st_count] = _ld_count;
  memcpy(&_ld_syms[_ld_count], mapping.Get_ld_symbol(),
         sizeof(ld_plugin_symbol) * mapping.Get_ld_count());
  ++ _st_count;
  _ld_count += mapping.Get_ld_count();
}

void
ST_ID_MAPPING::Init_for_archive(UINT32 num_of_ld, UINT32 num_of_file) {
  Is_True(_ld_syms == NULL && _ld_st_map == NULL && _st_ld_map == NULL,
          ("object initialized wrongly"));
  _ld_syms = (ld_plugin_symbol *)malloc(
                 sizeof(ld_plugin_symbol) * num_of_ld);
  Is_True(_ld_syms != NULL, ("out of memory?"));
  //_ld_st_map = NULL; // not used
  _st_ld_map = (UINT32 *)malloc(
                 sizeof(UINT32) * (num_of_file + 1));
  Is_True(_st_ld_map != NULL, ("out of memory?"));
  memset(_st_ld_map, 0, sizeof(UINT32) * num_of_file);
  _st_ld_map[num_of_file] = UINT32_MAX;  // set a sentinial
  _ld_count = 0;
  _st_count = 0;
  _st_predef = num_of_ld;
}

void
ST_ID_MAPPING::Init_for_elf(UINT32 num_of_ld) {
  Is_True(_ld_syms == NULL && _ld_st_map == NULL && _st_ld_map == NULL,
          ("object initialized wrongly"));
  _ld_syms = num_of_ld > 0 ? (ld_plugin_symbol *)malloc(
                                 sizeof(ld_plugin_symbol) * num_of_ld)
                           : NULL;;
  Is_True(num_of_ld == 0 || _ld_syms != NULL,
          ("out of memory?"));
  // _ld_st_map = NULL; 
  // _st_ld_map = NULL;
  _ld_count = 0;
  _st_count = 0;
  _st_predef = num_of_ld;
}

/*
 * WHIRL_FILE_INFO Implementations
 */
// read elf symtab section and convert to ld-plugin symbols
bool
WHIRL_FILE_INFO::Process_elf_symtab(WHIRL_FILE_MANAGER& mgr) {
  Is_True(_file_type == file_ar_elf,
          ("only object file in archive is allowed"));

  // no symtab
  if (_symtab.size == 0) {
    _mapping.Set_ld_count(0);
    return true;
  }

  UINT32 syms_count = _symtab.size / _symtab.ent_size;
  Is_True(_symtab.size % _symtab.ent_size == 0,
          ("symtab corrupted?"));
  char *strbuf = (char *)_fhandle + _strtab.offset;

  _mapping.Init_for_elf(syms_count);
  ld_plugin_symbol *ld_syms = _mapping.Get_ld_symbol();

  bool ret;
  if (_symtab.ent_size == sizeof(Elf64_Sym)) {
    const Elf64_Sym* sym = (Elf64_Sym*)
            ((char *)_fhandle + _symtab.offset);
    ret = process_symbol(sym, syms_count, ld_syms, strbuf, syms_count);
  }
  else if (_symtab.ent_size == sizeof(Elf32_Sym)) {
    const Elf32_Sym* sym = (Elf32_Sym*)
            ((char *)_fhandle + _symtab.offset);
    ret = process_symbol(sym, syms_count, ld_syms, strbuf, syms_count);
  }
  else {
    Is_True(FALSE, ("not Elf32 or Elf64 symtab?"));
    return false;
  }

  _mapping.Set_ld_count(syms_count);

  return ret;
}

bool
WHIRL_FILE_INFO::Process_whirl_symtab(WHIRL_FILE_MANAGER& mgr) {
  // process whirl symtab to setup mapping
  _mapping.Process_whirl_symtab();

  UINT32 syms_count = _mapping.Get_ld_count();
  if (syms_count == 0)
    return true;

  // register to linker
  if (_file_type == file_whirl) {
    Is_True(_lhandle != NULL, ("whirl file ld handle is null"));
    ld_plugin_symbol* syms = _mapping.Get_ld_symbol();
    ld_plugin_status status =
            mgr.callback().add_symbols(_lhandle,
                                       syms_count,
                                       syms);
    if (status != LDPS_OK) {
      mgr.callback().message(LDPL_ERROR,
                             "add_dymbols(handle=%p count=%d) failed(%d).",
                             _lhandle, syms_count, status);
      return false;
    }
  }
  else {
    Is_True(_lhandle == NULL && _file_type == file_ar_whirl,
            ("not a whirl file in archive"));
  }

  return true;
}

// call ld-plugin callback to ger resolution result
bool
WHIRL_FILE_INFO::Update_symtab(WHIRL_FILE_MANAGER& mgr) {
  if (_file_type == file_whirl || _file_type == file_archive) {
    Is_True(_lhandle != NULL, ("ld handle for whirl or archive is null"));

    ld_plugin_status status;
    UINT32 count = _mapping.Get_ld_count();
    status = mgr.callback().get_symbols(_lhandle,
                                        count,
                                        _mapping.Get_ld_symbol());
    if (status != LDPS_OK) {
      mgr.callback().message(LDPL_ERROR,
                             "get_symbols(handle=%p count=%d) failed(%d).",
                             _lhandle, count, status);
      return false;
    }
  }
  else {
    // copy symtab from archive mapping to local
    Is_True(_lhandle == NULL &&
            (_file_type == file_ar_elf || _file_type == file_ar_whirl),
            ("ld handle or file type wrong"));
    WHIRL_FILE_INFO& ar = mgr.Get_file(_parent_idx);
    Is_True(ar.File_index() == _parent_idx && ar.File_type() == file_archive,
            ("wrong parent file info"));
    UINT32 i;
    ST_ID_MAPPING& map = ar.Mapping();
    UINT32 start = ar.File_index() + 1;
    UINT32 end = start + map.Get_st_count();
    for (i = start; i < end; ++i) {
      WHIRL_FILE_INFO& sub = mgr.Get_file(i);
      Is_True(sub.File_index() == i, ("wrong file index"));
      Is_True(sub.File_type() == file_ar_whirl ||
              sub.File_type() == file_ar_elf, ("wrong file type"));
      UINT32 ofst = map.Get_ld_offset(i - start);
      Is_True(ofst + sub.Mapping().Get_ld_count() <= map.Get_ld_count(),
              ("ld sym out of bound"));
      memcpy(sub.Mapping().Get_ld_symbol(),
             map.Get_ld_symbol(ofst),
             sizeof(ld_plugin_symbol) * sub.Mapping().Get_ld_count());
    }
  }

  return true;
}

UINT32
WHIRL_FILE_INFO::Summerize_symtab(WHIRL_FILE_MANAGER& mgr) {
  Is_True(_lhandle != NULL, ("ld handle is null"));
  Is_True(_file_type == file_archive, ("file not an archive"));

  UINT32 i;
  // iteration 1, collect number of symbols
  UINT32 whirl_sym_count = 0;
  UINT32 ld_count = 0;
  UINT32 max_file = mgr.Get_file_count() + 1;
  for (i = _file_idx + 1; i < max_file; ++i) {
    WHIRL_FILE_INFO& info = mgr.Get_file(i);
    Is_True(info.File_index() == i, ("file index mismatch"));
    Is_True(info.File_type() == file_ar_elf ||
            info.File_type() == file_ar_whirl, ("file type mismatch"));
    ld_count += info.Mapping().Get_ld_count();
    if (info.File_type() == file_ar_whirl) {
      whirl_sym_count += info.Mapping().Get_ld_count();
    }
  }
  // return 0 no whirl to process
  if (whirl_sym_count == 0)
    return 0;
  // allocate memory to hold ld symbols and sub file mapping offset
  _mapping.Init_for_archive(ld_count, max_file - _file_idx - 1);
  // iteration 2, merge symbols in whirl and elf symtab to archive
  for (i = _file_idx + 1; i < max_file; ++i) {
    WHIRL_FILE_INFO& info = mgr.Get_file(i);
    _mapping.Merge(info.Mapping());
  }
  //register to ld
  ld_plugin_status status =
      mgr.callback().add_symbols(_lhandle,
                                 ld_count,
                                 _mapping.Get_ld_symbol());
  if (status != LDPS_OK) {
    mgr.callback().message(LDPL_ERROR,
                           "add_dymbols(handle=%p count=%d) failed(%d).",
                           _lhandle, ld_count, status);
    return 0;
  }
  return ld_count;
}

// dump symtab for debug purpose
void
WHIRL_FILE_INFO::Dump_symtab(FILE *fp) const {
  fprintf(fp, "%02d %s\n", _file_idx, _filename);
  UINT32 count = _mapping.Get_ld_count();
  for (UINT i = 0; i < count; ++i) {
    const ld_plugin_symbol* sym = _mapping.Get_ld_symbol(i);
    fprintf(fp, "  %02d %s def=%s vis=%s size=%ld "
                "res=%s",
                i, sym->name, //sym->version,
                ldps_kind_name[sym->def],
                ldps_visibility_name[sym->visibility],
                sym->size,
                //sym->comdat_key,
                ldps_resolution_name[sym->resolution]);
    if (_file_type == file_whirl || _file_type == file_ar_whirl)
      fprintf(fp, " st=%d\n", _mapping.Get_st_index(i));
    else
      fprintf(fp, "\n");
  }
}

// unmap buffer for file and free symbols
void
WHIRL_FILE_INFO::Clean() {
  Is_True(_filename != NULL, ("file name already freed"));
  ::free(_filename);
  _filename = NULL;

  // release file CONTEXT for whirl files
  if (_file_type == file_whirl || _file_type == file_ar_whirl)
    _context.Destroy_file_context();

  // no need to free memory map for whirl or elf in ar
  if (_file_type == file_ar_whirl || _file_type == file_ar_elf)
    return;

  Is_True(_fhandle != NULL, ("file handle already freed"));
  whirl_release_file(_fhandle, _filesize);
  _fhandle = NULL;
}


/*
 * WHIRL_FILE_MANAGER Implementations
 */
// helper function to get strtab and symtab from elf file
template<class Shdr> static bool
elf_shdr_get_tables(const Shdr* shdr, UINT n,
                    elf_strtab_info& strtab, elf_symtab_info& symtab) {
  INT strtab_index = -1;
  INT symtab_index = -1;
  INT symtab_link_index = -1;
  for (INT i = 1; i < n; ++i) {
    if (shdr[i].sh_type == SHT_STRTAB &&
        strtab_index == -1) {
      // possibly .shstrtab or .strtab, .strtab is needed
      Is_True(symtab_link_index == -1 ||
              symtab_link_index == i, ("symtab link wrong?"));
      strtab_index = i;
      strtab.index = i;
      strtab.offset = shdr[i].sh_offset;
      strtab.size = shdr[i].sh_size;
    }
    else if (shdr[i].sh_type == SHT_SYMTAB) {
      Is_True(symtab_index == -1, ("two symtab?"));
      if (strtab_index != -1 && strtab_index != shdr[i].sh_link)
        strtab_index = -1; // reset strtab index if .shstrtab goes first
      symtab_index = i;
      symtab_link_index = shdr[i].sh_link;
      symtab.index = i;
      symtab.ent_size = shdr[i].sh_entsize;
      symtab.offset = shdr[i].sh_offset;
      symtab.size = shdr[i].sh_size;
    }
  }
  return (strtab_index != -1 && symtab_index != -1) ?
             true : false;
}

// get size and offset for strtab and symtab from elf files
static bool
elf_get_tables(void* fhandle, elf_strtab_info& strtab, elf_symtab_info& symtab) {
  Elf64_Ehdr *ehdr = (Elf64_Ehdr *)fhandle;
  if (ehdr->e_ident[EI_CLASS] == ELFCLASS32) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)fhandle;
    const Elf32_Shdr* shdr = (const Elf32_Shdr *)
             ((char *)fhandle + ehdr->e_shoff);
    return elf_shdr_get_tables(shdr, ehdr->e_shnum, strtab, symtab);
  }
  else if (ehdr->e_ident[EI_CLASS] == ELFCLASS64) {
    const Elf64_Shdr* shdr = (const Elf64_Shdr *)
              ((char *)fhandle + ehdr->e_shoff);
    return elf_shdr_get_tables(shdr, ehdr->e_shnum, strtab, symtab);
  }
  else {
    Is_True(FALSE, ("unexpected elf class %d.", ehdr->e_ident[EI_CLASS]));
    return false;
  }
}

// add whirl file into file manager
UINT32
WHIRL_FILE_MANAGER::Add_file(void *lhandle, void *fhandle, char* name, size_t size,
                             UINT32 parent_id, input_file_type type) {
  Is_True(type != file_unknown, ("unknown file type"));
  Is_True(type != file_elf, ("unexpected elf file type"));

  // initialize backend before processing the first file
  if (_files.size() == 0)
    Initialize_backend();

  // add file to vector, id starts from 1
  uint32_t id = _files.size() + 1;
  _files.push_back(WHIRL_FILE_INFO(lhandle, fhandle, name, size, id, parent_id, type));
  if (type == file_archive) // don't process archive's symtab
    return id;

  WHIRL_FILE_INFO& info = _files.back();
  if (type == file_ar_elf) {
    // get strtab and symtab
    if (elf_get_tables(fhandle, info._strtab, info._symtab) == false) {
      // no symtab, set the size to 0
      info._symtab.size = 0;
    }
    info.Process_elf_symtab(*this);
    return id;
  }

  Is_True(type == file_whirl || type == file_ar_whirl,
          ("only process whirl file"));
  // load symtab and setup info context
  info.Setup(this);

  // process whirl symtab if symbol isn't processed
  info.Process_whirl_symtab(*this);

  return id;
}

// update ld-plugin resolution for all files
bool
WHIRL_FILE_MANAGER::Update_symtab() {
  for (INT i = 0; i < _files.size(); ++i) {
    WHIRL_FILE_INFO& info = _files[i];
    info.Update_symtab(*this);
  }
  return true;
}

// dump symtab resolution for all files
void
WHIRL_FILE_MANAGER::Dump_symtab(FILE* fp) const {
  fprintf(fp, "per-file symtab dump:\n");
  for (INT i = 0; i < _files.size(); ++i) {
    const WHIRL_FILE_INFO& info = _files[i];
    info.Dump_symtab(fp);
  }
  fprintf(fp, "extern symbol mapping dump:\n");
  hash_map<uint64_t, uint64_t>::const_iterator it = _mapping.begin();
  for (; it != _mapping.end(); ++it) {
    uint64_t src = it->first;
    uint64_t dst = it->second;
    fprintf(fp, "  (%ld, %ld) --> (%ld, %ld)\n",
                src >> 32, src & 0xffffffff,
                dst >> 32, dst & 0xffffffff);
  }
}

// clean file buffer for archive if it deosn't contain whirl ir
void
WHIRL_FILE_MANAGER::Clean_files(UINT32 ar_file_index) {
  WHIRL_FILE_INFO& info = Get_file(ar_file_index);
  Is_True(info.File_index() == ar_file_index,
          ("files array corrupted"));
  Is_True(info.File_type() == file_archive,
          ("File is not a archive"));
  for (UINT32 i = ar_file_index + 1; i < _files.size() + 1; ++i) {
    WHIRL_FILE_INFO& sub = Get_file(i);
    Is_True(sub.Parent_index() == ar_file_index,
            ("invalid parent index"));
    Is_True(sub.File_type() == file_ar_whirl ||
            sub.File_type() == file_ar_elf,
            ("invalid file type %d.", sub.File_type()));
    sub.Clean();
  }
  // add the archive file to libraries
  _libs.push_back(strdup(info.File_name()));
  info.Clean();
  // remove all entries include the archive file
  _files.resize(ar_file_index - 1);
}

// clean all file buffer
void
WHIRL_FILE_MANAGER::Clean() {
  std::vector<WHIRL_FILE_INFO>::iterator it = _files.begin();
  for (; it != _files.end(); ++it) {
    it->Clean();
  }
  std::vector<char*>::iterator libit = _libs.begin();
  for(; libit != _libs.end(); ++libit) {
    free(*libit);
  }
  _libs.clear();
  _files.clear();
}

