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
// Module: meta_mgr.cxx
//
// ====================================================================
//

#include "opt_vsa_meta.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dlfcn.h>
#include <time.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <sys/wait.h>
#include <fcntl.h>    // O_RDONLY
#include <libgen.h>   // dirname basename
#include <libelf.h>

#include "opt_vsa_meta.h"
#include "builtin_rule_defs.h"

// ====================================================================
// dump_meta_from_dll
//
// load so and invoke Get_engine_meta()/Get_rulesets_meta() to dump
// the meta info
// ====================================================================
extern "C" struct ENGINE_META*   Get_engine_meta();
extern "C" struct RULESET_META** Get_rulesets_meta();
int
dump_meta_from_builtin(FILE* fp)
{
  ENGINE_META* engine = Get_engine_meta();
  if (engine != NULL) {
    engine->Print(0, fp);
    return 0;
  }
  return 1;
}

// ====================================================================
// dump_meta_from_dll
//
// load so and invoke Get_engine_meta()/Get_rulesets_meta() to dump
// the meta info
// ====================================================================

// global variables undef in dll
void* g_comp_unit;
void* cplus_demangle;
void* Write_vsarpt_footer;

// used by dump_meta_from_dll
typedef struct ENGINE_META*   (*engine_fptr)();
typedef struct RULESET_META** (*rulesets_fptr)();

static int
dump_meta_from_dll(const char* input, FILE* fp)
{
  void *dll = dlopen("/home/laijx/binroot2/lib/1.0/maccom.so", RTLD_LAZY | RTLD_LOCAL);
  if (dll == NULL) {
    fprintf(stderr, "Error: failed to open `%s\' for read. (%s)\n",
                    "maccom", dlerror());
    return 3;
  }

  dll = dlopen("/home/laijx/binroot2/lib/1.0/macdfa.so", RTLD_LAZY | RTLD_LOCAL);
  if (dll == NULL) {
    fprintf(stderr, "Error: failed to open `%s\' for read. (%s)\n",
                    "macdfa", dlerror());
    return 3;
  }

  dll = dlopen(input, RTLD_LAZY | RTLD_LOCAL);
  if (dll == NULL) {
    fprintf(stderr, "Error: failed to open `%s\' for read. (%s)\n",
                    input, dlerror());
    return 3;
  }

  engine_fptr engine = (engine_fptr)dlsym(dll, "Get_engine_meta");
  if (engine != NULL) {
    struct ENGINE_META* meta = engine();
    if (meta != NULL) {
      printf("%s\n", meta->_name);
    }
  }
  rulesets_fptr ruleset = (rulesets_fptr)dlsym(dll, "Get_rulesets_meta");
  if (ruleset != NULL) {
    struct RULESET_META** rule_meta = ruleset();
    if (rule_meta != NULL) {
      if (*rule_meta != NULL)
        printf("%s\n", (*rule_meta)->_name);
      ++rule_meta;
    }
  }
  return 0;
}


// ====================================================================
// dump_meta_from_archive
//
// open archive and find the ruleset meta file in archive and print
// the meta info
// ====================================================================
static int
dump_meta_from_archive(const char* input, FILE* fp)
{
  int fd = open(input, O_RDONLY);
  if (fd < 0) {
    fprintf(stderr, "Error: failed to open `%s\' for read. (%d:%s)\n",
                    input, errno, strerror(errno));
    return 2;
  }
  if (elf_version(EV_CURRENT) == EV_NONE) {
    fprintf(stderr, "Error: failed to initialize libelf.\n");
    return 2;
  }
  Elf* elf = elf_begin(fd, ELF_C_READ, NULL);
  if (elf == NULL) {
    fprintf(stderr, "Error: input file `%s\' format is not recognized.\n",
                    input);
    close(fd);
    return 2;
  }
  if (elf_kind(elf) != ELF_K_AR) {
    fprintf(stderr, "Error: input file `%s\' is not archive.\n", input);
    close(fd);
    return 2;
  }
  Elf* entry;
  const char* json_suffix = "ruleset.json";
  const int   json_len    = strlen(json_suffix);
  int ret = 1;
  while ((entry = elf_begin(fd, ELF_C_READ, elf)) != NULL) {
    Elf_Arhdr *arh = elf_getarhdr(entry);;
    if (arh == NULL) {
      fprintf(stderr, "Error: failed to read archive header in file `%s\'.\n",
                      input);
      elf_end(entry);
      ret = 2;
      break;
    }
    const char* ar_name = arh->ar_name;
    if (ar_name != NULL) {
      int len = strlen(ar_name);
      if (len > json_len && strcmp(ar_name + len - json_len, json_suffix) == 0) {
        // found the entry
        off_t ofst = elf_getbase(entry);
        size_t size = arh->ar_size;
        char* buf = (char*)malloc(size);
        if (buf != NULL && pread(fd, buf, size, ofst) == size) {
          if (fwrite(buf, 1, size, fp) == size) {
            ret = 0;
          }
          else {
            fprintf(stderr, "Error: failed to write %ld bytes to output file. (%d:%s)\n",
                           size, errno, strerror(errno));
            ret = 2;
          }
        }
        else {
          if (buf == NULL)
            fprintf(stderr, "Error: failed to allocate %ld bytes memory.\n", size);
          else
            fprintf(stderr, "Error: failed to read %ld bytes from input file `%s\' offset=%ld. (%d:%s).\n",
                            size, input, ofst, errno, strerror(errno));
          ret = 2;
        }
        // close entry
        elf_end(entry);
        break;
      }
    }
    // move to next entry
    elf_next(entry);
    elf_end(entry);
  }
  // close elf
  elf_end(elf);
  close(fd);
  if (ret == 1) {
    fprintf(stderr, "Error: not find ruleset description in `%s\'.\n", input);
  }
  return ret;
}

// ====================================================================
// usage
// ====================================================================
void
usage(char* arg0)
{
  char* arg0_copy = strdup(arg0);
  struct tm tm_info;
  time_t current_time = time(NULL);
  localtime_r(&current_time, &tm_info);
  fprintf(stderr, "Xcalibyte Vulnerability Static Analyzer Ruleset Meta Information Dumper\n");
  if (tm_info.tm_year + 1900 > 2019)
    fprintf(stderr, "Copyright (c) 2019-%d Xcalibyte Limited\n", tm_info.tm_year + 1900);
  else
    fprintf(stderr, "Copyright (c) 2019 Xcalibyte Limited\n");
  fprintf(stderr, "Confidential under the terms of the NDA between Xcalibyte and the licensee.\n");
  fprintf(stderr, "For the use of the licensee only. Internal use only. No redistribution.\n\n");
  fprintf(stderr, "Usage: %s [-o output-file] [builtin | <input-file>]\n", basename(arg0_copy));
  fprintf(stderr, " -o output-file: Write meta info into <output-file>. If omitted, meta info is\n");
  fprintf(stderr, "                 written to stdout.\n");
  fprintf(stderr, " builtin:        Dump builtin rules. When builtin rule is dumpped, the meta\n");
  fprintf(stderr, "                 info for Xcalibyte scan engine is also dumpped.\n");
  fprintf(stderr, " <input-file>:   Dump rules defined in given input rule file.\n");
  free(arg0_copy);
}

// ====================================================================
// main
// ====================================================================
int
main(int argc, char* argv[])
{
  const char* output = NULL;
  const char* input = NULL;

  // parse parameter
  int i;
  for (i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "-o") == 0 && i + 1 < argc)
      output = argv[++i];
    else if (strcmp(argv[i], "-v") == 0 ||
             strcmp(argv[i], "-vvv") == 0 ||
             strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "-help") == 0 ||
             strcmp(argv[i], "--help") == 0) {
      usage(argv[0]);
      return 1;
    }
    else if (input == NULL) {
      input = argv[i];
    }
    else {
      usage(argv[0]);
      return 1;
    }
  }

  if (input == NULL) {
    usage(argv[0]);
    return 1;
  }

  // open output file
  FILE* out = output == NULL ? stdout :
                               fopen(output, "w");
  if (out == NULL) {
    fprintf(stderr, "Error: failed to open `%s\' for write. (%d:%s)\n",
                    output, errno, strerror(errno));
    return 2;
  }

  // dump rule meta
  int ret = 2;
  if (strcmp(input, "builtin") == 0) {
    ret = dump_meta_from_builtin(out);
  }
  else {
    int input_len = strlen(input);
    if (input_len > 3 && strcmp(input + input_len - 3, ".so") == 0) {
      ret = dump_meta_from_dll(input, out);
    }
    if (ret != 0)
      ret = dump_meta_from_archive(input, out);
  }
  return ret;
}

