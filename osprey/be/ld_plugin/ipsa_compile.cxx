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

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include "ipsa_compile.h"
#include "glob.h"
#include "errors.h"
#include "whirl_file_mgr.h"
#include "file_util.h"

#define DRIVER_REL_PATH "/../../bin/xvsa"

// =============================================================================
//
// IPSA_COMP_DRIVER::Initialize
//   process options
//   set xvsa driver path
//   set workdir
//
// =============================================================================
void
IPSA_COMP_DRIVER::Initialize()
{
  // process options
  Process_options();

  // set driver path
  const char * libpath = _mgr->Get_libpath();
  int driver_len = strlen(libpath) + strlen(DRIVER_REL_PATH) + 1;
  _driver_path = (char *) MEM_POOL_Alloc(Mem_pool(), driver_len);
  strcpy(_driver_path, libpath);
  strcat(_driver_path, DRIVER_REL_PATH);
  _driver_path[driver_len - 1] = '\0';

  // set workdir - <outfile>.ipsakeep or <outfile>.ipsaXXXXXX
  const char *prefix = _mgr->Get_output_file() ?
                       _mgr->Get_output_file() : DEFAULT_OUT_NAME;
  const char *suffix = Keep_Flag ? IPSA_KP_DIR_SUFFIX : IPSA_DIR_SUFFIX;
  size_t len = strlen(prefix) + strlen(suffix);
  _workdir = (char *) MEM_POOL_Alloc(Mem_pool(), len + 1);
  sprintf(_workdir, "%s%s", prefix, suffix);
  if(!Keep_Flag) {
    char *tmpdir = mkdtemp(_workdir);
    if(!tmpdir) {
      perror(_workdir);
      exit(1);
    }
    _workdir = tmpdir;
  } else if(Create_dir(_workdir)) {
    perror(_workdir);
    exit(1);
  }

  // add input files
  std::vector<WHIRL_FILE_INFO>::iterator it;
  for(it = _mgr->Get_files().begin(); it != _mgr->Get_files().end(); ++it) {
    WHIRL_FILE_INFO &info = *it;
    // skip all non-whirl files
    if (info.File_type() != file_whirl &&
        info.File_type() != file_ar_whirl)
      continue;
    if(FILE_INFO_is_vtable(info.Whirl_file_info()) ||
       FILE_INFO_is_rbc(info.Whirl_file_info()))
      continue;
    File_Index = info.File_index();
    info.Restore_context();
    Add_input_file(Ipsa_File_Name);
  }
}

// =============================================================================
//
// IPSA_COMP_DRIVER::Process_options - process options from IPSA_manager
//
// =============================================================================
void
IPSA_COMP_DRIVER::Process_options()
{
  const vector<char *> *args = _mgr->Get_args();
  for(int arg_idx = 1; arg_idx < args->size(); arg_idx++) {
    char *arg = args->at(arg_idx);
    if(strcmp(arg, "xvsa-xfa-dummy.c") == 0 ||
       strcmp(arg, "-ipsa") == 0) {
      continue;
    }
    _options.push_back(arg);
  }
  _options.push_back((char *) "-noxa");
}

// =============================================================================
//
// IPSA_COMP_DRIVER::gen_option - generate one line compile option
//
// =============================================================================
void
IPSA_COMP_DRIVER::Gen_option(vector<char *> &arg_list)
{
  const vector<char *> *args = _mgr->Get_args();
  // add driver path
  arg_list.push_back(_driver_path);

  // add options
  arg_list.insert(arg_list.end(), _options.begin(), _options.end());

  // add input files
  arg_list.insert(arg_list.end(), _inputs.begin(), _inputs.end());

  // add libs
  const vector<char *> *libs = _mgr->Get_libs();
  arg_list.insert(arg_list.end(), libs->begin(), libs->end());

  // add outputfile
  const char *outfile = _mgr->Get_output_file();
  if(outfile) {
    arg_list.push_back((char*)"-o");
    arg_list.push_back((char*)outfile);
  }
}


// =============================================================================
//
// IPSA_COMP_DRIVER::Compile - invoke compile directly with one xvsa command
//
// =============================================================================
int
IPSA_COMP_DRIVER::Compile()
{
  vector<char *> arg_list;
  Gen_option(arg_list);

  int argc = arg_list.size() + 1;
  char **argv = (char **)MEM_POOL_Alloc(Mem_pool(), argc * sizeof(char *));
  for(int j = 0; j < arg_list.size(); j++) {
    argv[j] = arg_list[j];
  }
  argv[argc - 1] = NULL;
  int ret = Exec(argc, argv);
  return ret;
}

// =============================================================================
//
// IPSA_COMP_DRIVER::compile_with_make - compile inputs through makefile for parallel
//
// =============================================================================
int
IPSA_COMP_DRIVER::Compile_with_make()
{
  char *makefile = Gen_makefile();
  char *sh_file = Gen_cmdfile(makefile);
  int argc = 3;
  char *argv[argc];
  argv[0] = (char *)"/bin/sh";
  argv[1] = sh_file;
  argv[2] = NULL;
  return Exec(argc, argv);
}

// =============================================================================
//
// IPSA_COMP_DRIVER::Exec - execute commands in argv
// @Return 0 for success, 1 for fail
//
// =============================================================================
int
IPSA_COMP_DRIVER::Exec(int argc, char **argv)
{
  if(Show_Progress) {
    fprintf(stdout, "\nIPSA COMPILE: ");
    for(int j = 0; j < argc - 1; j++) {
      fprintf(stdout, "%s ", argv[j]);
    }
    fprintf(stdout, "\n");
  }

  int phase_pid = fork();
  if(phase_pid == -1) {
    fprintf(stderr, "IPSA: failed fork child process\n");
    return 1;
  }

  if(phase_pid == 0) {
    // child process
    execv(argv[0], argv);
    fprintf(stderr, "IPSA: cannot exec prog %s, errno:%d\n", argv[0], errno);
    return 1;
  } else {
    // parent process
    int waitstatus;
    int waitpid;
    while((waitpid = wait(&waitstatus)) != phase_pid) {
      if(waitpid == -1)  {
        fprintf(stderr, "IPSA: wait child error\n");
        return 1;
      }
    }
  }
  return 0;
}

// =============================================================================
//
// IPSA_COMP_DRIVER::gen_makefile - generate makefile
// Format as below:
//   XVSA_BIN =
//   IPSA_TMPDIR =
//   IPSA_OUTPUT_FILE =
//   IPSA_INPUT_FILES =
//   OPTIONS =
//   LIBS =
//   IPSA_OBJ_FILES = $(IPSA_INPUT_FILES:.I=.o)
//
//   .PHONY: default
//
//   default: $(IPSA_OUTPUT_FILE)
//
//   %.o: %.I
//           $(XVSA_BIN) -c $(OPTIONS) -o $@ $<
//
//   $(IPSA_OUTPUT_FILE) : $(IPSA_OBJ_FILES)
//           $(XVSA_BIN) $(OPTIONS) $(IPSA_OBJ_FILES) $(LIBS) -o $(IPSA_OUTPUT_FILE)
//
//   clean:
//           rm -rf $(IPSA_OBJ_FILES) $(IPSA_OUTPUT_FILE)
//
// =============================================================================
char *
IPSA_COMP_DRIVER::Gen_makefile()
{
  char makefile_name[256];
  sprintf(makefile_name, "makefile.ipsa%ld", (long) getpid());
  char *full_path = (char *) MEM_POOL_Alloc(
    Mem_pool(), strlen(_workdir) + strlen(makefile_name) + 2);
  sprintf(full_path, "%s/%s", _workdir, makefile_name);
  FILE * fp = fopen(full_path, "w");
  if(fp == NULL) {
    perror(full_path);
    exit(1);
  }

  fprintf(fp, "XVSA_BIN = %s\n", _driver_path);
  fprintf(fp, "IPSA_TMPDIR = %s\n", _workdir);
  fprintf(fp, "IPSA_OUTPUT_FILE = %s\n", _mgr->Get_output_file());
  fprintf(fp, "IPSA_INPUT_FILES = ");
  vector<char *>::const_iterator it;
  for(it = _inputs.begin(); it != _inputs.end(); it++) {
    fprintf(fp, "%s/%s ", _workdir, *it);
  }
  fprintf(fp, "\n");
  fprintf(fp, "OPTIONS = ");
  for(it = _options.begin(); it != _options.end(); it++) {
    fprintf(fp, "%s ", *it);
  }
  fprintf(fp, "\n");
  fprintf(fp, "LIBS = ");
  for(it = _mgr->Get_libs()->begin(); it != _mgr->Get_libs()->end(); it++) {
    fprintf(fp, "%s ", *it);
  }
  fprintf(fp, "\n");

  const char *makefile_pattern = "\
IPSA_OBJ_FILES = $(IPSA_INPUT_FILES:.I=.o) \n\n\
.PHONY: default \n\n\
default: $(IPSA_OUTPUT_FILE) \n\n\
%.o: %.I \n\
\t$(XVSA_BIN) -c $(OPTIONS) -o $@ $< \n\n\
$(IPSA_OUTPUT_FILE) : $(IPSA_OBJ_FILES) \n\
\t$(XVSA_BIN) $(OPTIONS) $(IPSA_OBJ_FILES) $(LIBS) -o $(IPSA_OUTPUT_FILE) \n\n\
clean: \n\
\trm -rf $(IPSA_OBJ_FILES) $(IPSA_OUTPUT_FILE) \n\n\
";
  fprintf(fp, "%s", makefile_pattern);
  fclose(fp);
  return full_path;
}

// =============================================================================
//
// IPSA_COMP_DRIVER::Gen_cmdfile - generate shell script to call makefile
//
// =============================================================================
char *
IPSA_COMP_DRIVER::Gen_cmdfile(char *makefile)
{
  char sh_cmdfile_name[256];
  sprintf(sh_cmdfile_name, "cmdfile.%ld", (long) getpid());
  char *full_path = (char *) MEM_POOL_Alloc(
    Mem_pool(), strlen(_workdir) + strlen(sh_cmdfile_name) + 2);
  sprintf(full_path, "%s/%s", _workdir, sh_cmdfile_name);
  FILE * fp = fopen(full_path, "w");
  if(fp == NULL) {
    perror(full_path);
    exit(1);
  }
  chmod(full_path, 0644);

  const char *sh_top = "\
#! /bin/sh -f               \n\
MAKEFLAGS=                  \n\
export MAKEFLAGS            \n\
";
  fprintf(fp, "%s", sh_top);
  fprintf(fp, "make -f %s clean\n", makefile);
  fprintf(fp, "make -f %s", makefile);
  // TODO: emit -j
  fprintf(fp, "\nretval=$?\n");
  fprintf(fp, "exit $retval\n");
  fclose(fp);
  return full_path;
}

// =============================================================================
//
// IPSA_COMP_DRIVER::Create_dir - create directory for given path
// @Return 0 for success, 1 for fail
//
// =============================================================================
int
IPSA_COMP_DRIVER::Create_dir(char *path)
{
  int ret = 0;
  mode_t cmask = umask (0);
  umask (cmask);
  if(mkdir(path, 0777 & ~cmask) != 0) {
    if(errno == EEXIST && Keep_Flag) {
      // clear files under path, ignore dirs (IPSA only create regular files)
      DIR *dirp;
      struct dirent *entryp;
      dirp = opendir(path);
      if(dirp != NULL ) {
        while((entryp = readdir(dirp)) != NULL) {
          if(!strcmp(entryp->d_name, ".") || !strcmp(entryp->d_name, ".."))
            continue;
          char *fname = (char *)MEM_POOL_Alloc(
            Mem_pool(), strlen(path) + strlen(entryp->d_name) + 2);
          sprintf(fname, "%s/%s", path, entryp->d_name);
          unlink(fname);
        }
        closedir ( dirp );
      } else {
        ret = 1;
      }
    } else {
      ret = 1;
    }
  }
  return ret;
}
