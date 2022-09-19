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

/*
 * ld -plugin=./whirl-plugin.so -plugin-opt="-opt0" -plugin-opt="-opt1" a.o b.o
 */
#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <plugin-api.h>
#include "plugin_defs.h"
#include "whirl_file_mgr.h"
#include "libelf.h"
#include "rapidjson/reader.h"
#include "rapidjson/memorystream.h"

class rsj_handler /* : public rapidjson::BaseReaderHandler<UTF8<>, rsj_handler> */ {
private:
  const char* _name;
  const char* _version;
  int         _level;
  enum { ignore, for_name, for_version }
              _state;

public:
  rsj_handler() : _name(NULL), _version(NULL), _level(0), _state(ignore) { }

  const char* Name() const    { return _name; }
  const char* Version() const { return _version; }

public:
  bool Null()
  {
    if (_level == 1) {
      if (_state == for_name || _state == for_version)
        return false;
    }
    return true;
  }

  bool String(const char* str, size_t len, bool copy)
  {
    if (_level == 1) {
      if (_state == for_name) {
        _name = strdup(str);
        return _version == NULL ? true : false;
      }
      else if (_state == for_version) {
        _version = strdup(str);
        return _name == NULL ? true : false;
      }
    }
    return true;
  }

  bool StartObject()
  {
    ++ _level;
    return true;
  }

  bool Key(const char* str, size_t length, bool copy)
  {
    if (_level == 1) {
      if (strcmp(str, "name") == 0)
        _state = for_name;
      else if (strcmp(str, "version") == 0)
        _state = for_version;
      else
        _state = ignore;
    }
    return true;
  }

  bool EndObject(size_t count)
  {
    -- _level;
    return true;
  }

  bool RawNumber(const char* str, size_t len, bool copy) { return true; }
  bool Bool(bool b)           { return true; }
  bool Double(double d)       { return true; }
  bool Int64(int64_t i)       { return true; }
  bool Uint64(uint64_t u)     { return true; }
  bool Int(int i)             { return true; }
  bool Uint(unsigned int u)   { return true; }
  bool StartArray()           { return true; }
  bool EndArray(size_t count) { return true; }
};

static bool
ruleset_json_handler(WHIRL_FILE_MANAGER* mgr, char* buf, size_t len) {
  rsj_handler handler;
  rapidjson::Reader reader;
  rapidjson::MemoryStream ss(buf, len);
  reader.Parse(ss, handler);
  if (handler.Name() != NULL && handler.Version() != NULL)
    return mgr->Add_ruleset(handler.Name(), handler.Version());
  return false;
}

static enum ld_plugin_status
claim_file_handler(const struct ld_plugin_input_file *file, int *claimed) {
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Instance();

  // map the file
  void *fhandle = whirl_read_file(file);
  if (fhandle == NULL) {
    mgr->callback().message(LDPL_ERROR, "claim_file failed to map file %s.", file->name);
    return LDPS_ERR;
  }

  // construct elf object in memory
  Elf *elf, *entry;
  elf = elf_memory((char *)fhandle, file->filesize);
  if (elf == NULL) {
    // not an ELF file, ignore
    DEBUG_OUTPUT("libelf failed to open %s.\n", file->name);
    whirl_release_file(fhandle, file->filesize);
    *claimed = 0;
    return LDPS_OK;
  }
 
  if (elf_kind(elf) != ELF_K_AR &&
      elf_kind(elf) != ELF_K_ELF) {
    // not handle archive or elf
    DEBUG_OUTPUT("unknowd elf kind %d.\n", elf_kind(elf));
    whirl_release_file(fhandle, file->filesize);
    *claimed = 0;
    return LDPS_OK;
  }

  char revision[128];
  if (elf_kind(elf) == ELF_K_ELF) {
    // this is a standalone elf file
    elf_end(elf); // end processing elf at first

    // try to get whirl revision
    input_file_type type = whirl_revision(fhandle, file->filesize, revision);
    DEBUG_OUTPUT("input file=%s offset=%ld size=%ld fd=%d handle=%p fhandle=%p %s\n",
                 file->name, file->offset, file->filesize,
                 file->fd, file->handle, fhandle,
                 type == file_whirl ? revision :
                   type == file_archive ? "ARCHIVE" : 
                     type == file_elf ? "ELF" : "UNKNOWN");
    // check if file is whirl
    if (type != file_whirl) {
      *claimed = 0;
      whirl_release_file(fhandle, file->filesize);
      return LDPS_OK;
    }
    // add file to manager
    UINT32 id = mgr->Add_file(file->handle, fhandle, strdup(file->name), file->filesize,
                                         UINT32_MAX, file_whirl);
    if (id == UINT32_MAX) {
      *claimed = 0;
      whirl_release_file(fhandle, file->filesize);
      return LDPS_ERR;
    }
    //claim this file
    *claimed = 1;
    return LDPS_OK;
  }
  else {
    // this is an archive file
    // add archive to file list at first
    UINT32 ar_file_index = mgr->Add_file(file->handle, fhandle, strdup(file->name), file->filesize,
                                         UINT32_MAX, file_archive);
    if (ar_file_index == UINT32_MAX) {
      elf_end(elf);
      *claimed = 0;
      whirl_release_file(fhandle, file->filesize);
      return LDPS_ERR;
    }

    // the name needs to match with vsa/meta_manager.cxx and rbc/Makefile.gbase
    const char* json_suffix = "ruleset.json";
    const int   json_len    = strlen(json_suffix);
    int file_name_len       = strlen(file->name);
    // go through each file in archive
    while ((entry = elf_begin(file->fd, ELF_C_READ, elf)) != NULL) {
      Elf_Arhdr *arh;
      arh = elf_getarhdr(entry);
      if (arh == NULL) {
        DEBUG_OUTPUT("archive %s header is null\n", file->name);
        elf_end(entry);
        elf_end(elf);
        *claimed = 0;
        whirl_release_file(fhandle, file->filesize);
        return LDPS_ERR;
      }
      if (strcmp(arh->ar_name, "/") == 0 ||
          strcmp(arh->ar_name, "//") == 0) {
        elf_next(entry);
        elf_end(entry);
        continue;
      }

      off_t  ofst = elf_getbase(entry); // offset of this object file in archive
      size_t size = arh->ar_size;       // size of the object file
      char* base = (char*)fhandle + ofst;

      int arn_len = strlen(arh->ar_name);
      if (arn_len > json_len &&
          strcmp(arh->ar_name + arn_len - json_len, json_suffix) == 0) {
        // this is a ruleset json file
        ruleset_json_handler(mgr, base, size);
        elf_next(entry);
        elf_end(entry);
        continue;
      }

      input_file_type type = whirl_revision(base, arh->ar_size, revision);
      DEBUG_OUTPUT("input file=%s!%s offset=%ld size=%ld fd=%d handle=%p fhandle=%p %s\n",
                   file->name, arh->ar_name, ofst, size,
                   file->fd, file->handle, fhandle,
                   type == file_whirl ? revision :
                     type == file_archive ? "ARCHIVE" : 
                       type == file_elf ? "ELF" : "UNKNOWN");
      Is_True(type != file_archive,
              ("file in archive is an archive"));
      if (type != file_whirl && type != file_elf) {
        elf_next(entry);
        elf_end(entry);
        continue;
      }
                
      // generate name X.a!Y.o
      int ar_name_len = strlen(arh->ar_name);
      char* name = (char*)malloc(file_name_len + ar_name_len + 2);
      Is_True(name != NULL, ("out-of-memory?"));
      strcpy(name, file->name);
      name[file_name_len] = '!';
      strcpy(name + file_name_len + 1, arh->ar_name);
      // move to next file
      elf_next(entry);
      elf_end(entry);
      // add file to manager
      if (mgr->Add_file(NULL, base, name, size, ar_file_index,
                       type == file_whirl ? file_ar_whirl : file_ar_elf) == UINT32_MAX) {
        elf_end(elf);
        *claimed = 0;
        // don't call whirl_release_file because the handle can be freed by
        // file_info entry for archive
        return LDPS_ERR;
        
      }
    }
    elf_end(elf);
    // summarize symtab in archive and add symbols to linker
    WHIRL_FILE_INFO& ar_info = mgr->Get_file(ar_file_index);
    if (ar_info.Summerize_symtab(*mgr) == 0) {
      // no whirl symtab
      // remove all files in archive include archive itself
      // don't call whirl_release_file because the handle can be freed by
      // file_info entry for archive
      mgr->Clean_files(ar_file_index);
    }
    *claimed = 1;
    return LDPS_OK;
  }
  return LDPS_ERR;
#if 0
  if (fhandle == NULL) {
    mgr->callback().message(LDPL_ERROR, "claim_file failed to map file %s.", file->name);
    return LDPS_ERR;
  }
  // get whirl revision
  input_file_type type = whirl_revision(fhandle, file->filesize, revision);
  if (type == file_unknown || type == file_elf) {
    // dosn't handle unknown or elf type
    *claimed = 0;
    whirl_release_file(fhandle, file->filesize);
    return LDPS_OK;
  }

  if (type == file_archive) {
    // TODO handle archive
    *claimed = 0;
    whirl_release_file(fhandle, file->filesize);
    return LDPS_OK;
  }

  // add file to manager
  if (mgr->Add_file(file, fhandle) == false) {
    *claimed = 0;
    whirl_release_file(fhandle, file->filesize);
    return LDPS_ERR;
  }
  *claimed = 1;
#endif
  return LDPS_OK;
}

static enum ld_plugin_status
all_symbols_read_handler(void) {
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Instance();
  if (mgr->Backend_process() == false)
    return LDPS_ERR;
  return LDPS_OK;
}

static enum ld_plugin_status
cleanup_handler(void) {
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Instance();
  mgr->Clean();
  return LDPS_OK;
}

extern "C" enum ld_plugin_status
onload(struct ld_plugin_tv *tv) {
  // initialize WHIRL_FILE_MANAGER
  WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Create();
  mgr->Initialize();

  enum ld_plugin_status status;
  struct ld_plugin_tv *p = tv;
  plugin_callback& callback = mgr->callback();

  while (p->tv_tag) {
    switch (p->tv_tag) {
    case LDPT_API_VERSION:
    case LDPT_GET_INPUT_FILE:
    case LDPT_RELEASE_INPUT_FILE:
    case LDPT_SET_EXTRA_LIBRARY_PATH:
    case LDPT_GNU_LD_VERSION:
    case LDPT_GET_VIEW:
    case LDPT_GET_INPUT_SECTION_COUNT:
    case LDPT_GET_INPUT_SECTION_TYPE:
    case LDPT_GET_INPUT_SECTION_NAME:
    case LDPT_GET_INPUT_SECTION_CONTENTS:
    case LDPT_UPDATE_SECTION_ORDER:
    case LDPT_ALLOW_SECTION_ORDERING:
    case LDPT_ALLOW_UNIQUE_SEGMENT_FOR_SECTIONS:
    case LDPT_UNIQUE_SEGMENT_FOR_SECTIONS:
    case LDPT_GET_SYMBOLS_V3:
    case LDPT_GET_INPUT_SECTION_ALIGNMENT:
    case LDPT_GET_INPUT_SECTION_SIZE:
    case LDPT_REGISTER_NEW_INPUT_HOOK:
      break;
    case LDPT_OUTPUT_NAME:
      mgr->Set_output_name(p->tv_u.tv_string);
      break;
    case LDPT_GOLD_VERSION:
      callback.gold_version = p->tv_u.tv_val;
      break;
    case LDPT_LINKER_OUTPUT:
      callback.output_file_type = (enum ld_plugin_output_file_type) p->tv_u.tv_val;
      callback.output_file_type_set = 1;
      break;
    case LDPT_OPTION:
      mgr->Add_arg(p->tv_u.tv_string);
      break;
    case LDPT_REGISTER_CLAIM_FILE_HOOK:
      callback.register_claim_file = p->tv_u.tv_register_claim_file;
      break;
    case LDPT_REGISTER_ALL_SYMBOLS_READ_HOOK:
      callback.register_all_symbols_read = p->tv_u.tv_register_all_symbols_read;
      break;
    case LDPT_REGISTER_CLEANUP_HOOK:
      callback.register_cleanup = p->tv_u.tv_register_cleanup;
      break;
    case LDPT_ADD_SYMBOLS:
      callback.add_symbols = p->tv_u.tv_add_symbols;
      break;
    case LDPT_GET_SYMBOLS:
      if (callback.get_symbols == NULL)
        callback.get_symbols = p->tv_u.tv_get_symbols;
      break;
    case LDPT_ADD_INPUT_FILE:
      callback.add_input_file = p->tv_u.tv_add_input_file;
      break;
    case LDPT_MESSAGE:
      callback.message = p->tv_u.tv_message;
      break;
    case LDPT_ADD_INPUT_LIBRARY:
      callback.add_input_library = p->tv_u.tv_add_input_library;
      break;
    case LDPT_GET_SYMBOLS_V2:
      callback.get_symbols = p->tv_u.tv_get_symbols;
      break;
    }
    ++ p;
  }

  assert(callback.register_all_symbols_read != NULL);
  assert(callback.register_claim_file != NULL);
  assert(callback.register_cleanup != NULL);
  assert(callback.add_symbols != NULL);
  assert(callback.get_symbols != NULL);

  if (elf_version(EV_CURRENT) == EV_NONE)
    return LDPS_ERR;

  status = callback.register_claim_file(claim_file_handler);
  if (status != LDPS_OK)
    return status;

  status = callback.register_cleanup(cleanup_handler);
  if (status != LDPS_OK)
    return status;

  status = callback.register_all_symbols_read(all_symbols_read_handler);
  if (status != LDPS_OK)
    return status;

  return LDPS_OK;
}

