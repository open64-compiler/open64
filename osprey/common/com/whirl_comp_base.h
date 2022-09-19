/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//-*-c++-*-
// ============================================================================
// ============================================================================
//
// Module: whirl_comp_base.h
//
// ============================================================================
//

#ifndef whirl_comp_base_INCLUDED
#define whirl_comp_base_INCLUDED

#include <vector>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include "defs.h"
#include "mempool.h"
#include "glob.h"
#include "errors.h"
#include "file_util.h"

// =============================================================================
//
//  class WHIRL_COMP_BASE provide basic interfaces to compile whirl files
//  Implement COMP_DRIVER to custom compile for AOT/IAST
//  Example Usage:
//    CUSTOM_COMP_DRIVER driver;
//    WHIRL_COMP_BASE<CUSTOM_COMP_DRIVER> comp_driver(driver);
//    comp_driver.Compile(T *);
//
//=============================================================================
template<typename COMP_DRIVER>
class WHIRL_COMP_BASE {
private:
  COMP_DRIVER        &_driver;

  WHIRL_COMP_BASE(const WHIRL_COMP_BASE&);              // REQUIRED UNDEFINED UNWANTED methods
  WHIRL_COMP_BASE& operator = (const WHIRL_COMP_BASE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  WHIRL_COMP_BASE(COMP_DRIVER &driver) : _driver(driver) {}
  ~WHIRL_COMP_BASE(void)                                 {}

  MEM_POOL      *Mem_pool(void)                { return _driver.Mem_pool();    }
  const char    *Get_workdir(void) const       { return _driver.Get_workdir(); }
  const char    *Get_phase(void) const         { return _driver.Get_phase();   }

  template<typename T>
  int Compile(T *config)
  {
    _driver.Initialize(config);
    char *makefile = _driver.Gen_makefile();
    char *sh_file = Gen_cmdfile(makefile);
    int argc = 3;
    char *argv[argc];
    argv[0] = (char *)"/bin/sh";
    argv[1] = sh_file;
    argv[2] = NULL;
    return Exec(argc, argv);
  }

  int Create_dir(char *path)
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
private:
  int Exec(int argc, char **argv)
  {
    if(Show_Progress) {
      fprintf(stdout, "\n%s: ", Get_phase());
      for(int j = 0; j < argc - 1; j++) {
        fprintf(stdout, "%s ", argv[j]);
      }
      fprintf(stdout, "\n");
    }

    int phase_pid = fork();
    if(phase_pid == -1) {
      fprintf(stderr, "%s: failed fork child process\n", Get_phase());
      return 1;
    }

    if(phase_pid == 0) {
      // child process
      execv(argv[0], argv);
      fprintf(stderr, "%s: cannot exec prog %s, errno:%d\n",
              Get_phase(), argv[0], errno);
      return 1;
    } else {
      // parent process
      int waitstatus;
      int waitpid;
      while((waitpid = wait(&waitstatus)) != phase_pid) {
        if(waitpid == -1)  {
          fprintf(stderr, "%s: wait child error\n", Get_phase());
          return 1;
        }
      }
    }
    return 0;
  }

  char *Gen_cmdfile(char *makefile)
  {
    char sh_cmdfile_name[256];
    sprintf(sh_cmdfile_name, "cmdfile.%ld", (long) getpid());
    char *full_path = (char *) MEM_POOL_Alloc(
      Mem_pool(), strlen(Get_workdir()) + strlen(sh_cmdfile_name) + 2);
    sprintf(full_path, "%s/%s", Get_workdir(), sh_cmdfile_name);
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
    fprintf(fp, "make -f %s -s clean\n", makefile);
    fprintf(fp, "make -f %s -s ", makefile);
    // TODO: emit -j
    fprintf(fp, "\nretval=$?\n");
    // do cleanups
    if (!_driver.Keep()) {
      fprintf(fp, "rm -rf %s\n", Get_workdir());
    }
    fprintf(fp, "exit $retval\n");
    fclose(fp);
    return full_path;
  }
};

#endif