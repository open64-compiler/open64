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

#ifndef whirl_plugin_defs_h
#define whirl_plugin_defs_h "whirl_plugin_defs.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <plugin-api.h>
#include <vector>

extern bool verbose;
extern FILE *trace_file;
#define DEBUG_OUTPUT(...) if(verbose && trace_file) fprintf(trace_file, "DEBUG: " __VA_ARGS__)

class plugin_callback {
public:
  ld_plugin_register_claim_file       register_claim_file;
  ld_plugin_register_all_symbols_read register_all_symbols_read;
  ld_plugin_get_symbols               get_symbols;
  ld_plugin_register_cleanup          register_cleanup;
  ld_plugin_add_input_file            add_input_file;
  ld_plugin_add_input_library         add_input_library;
  ld_plugin_message                   message;
  ld_plugin_add_symbols               add_symbols;
  ld_plugin_output_file_type          output_file_type;
  int  gold_version;
  bool output_file_type_set;
};

enum input_file_type {
  file_unknown,
  file_whirl,    // standalone whirl IR file (ELF)
  file_elf,      // standalone object file (ELF)
  file_archive,  // archive file (AR), container
  file_ar_whirl, // embedded whirl IR file (ELF in AR)
  file_ar_elf,   // embedded object file (ELF in AR)
};

struct elf_strtab_info {
  off_t offset;
  off_t size;
  uint32_t index;
};

struct elf_symtab_info {
  off_t offset;
  off_t size;
  uint32_t index;
  uint32_t ent_size;
};

/* mmap file into memory and return the start address */
extern "C" void* whirl_read_file(const struct ld_plugin_input_file *file);

/* unmap the file */
extern "C" void  whirl_release_file(void* fhandle, size_t length);

/* check if input file is whirl or not */
extern "C" input_file_type whirl_revision(void *fhandle, off_t size, char *revision);

/* xexit */
extern "C" void xexit(int code);

#endif /* whirl_plugin_defs_h */
