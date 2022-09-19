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

#ifndef ipsa_compile_INCLUDED
#define ipsa_compile_INCLUDED        "ipsa_compile.h"

#include <vector>
#include <stdio.h>
#include "defs.h"
#include "mempool.h"
using namespace std;

class WHIRL_FILE_MANAGER;

#define IPSA_KP_DIR_SUFFIX ".ipsakeep"
#define IPSA_DIR_SUFFIX    ".ipsaXXXXXX"
#define DEFAULT_OUT_NAME   "a.out"

class IPSA_COMP_DRIVER {
private:
  CXX_MEM_POOL        _pool;
  WHIRL_FILE_MANAGER *_mgr;
  char               *_workdir;
  char               *_driver_path;
  vector<char *>      _inputs;
  vector<char *>      _options;

public:
  IPSA_COMP_DRIVER(WHIRL_FILE_MANAGER* mgr)
    : _mgr(mgr), _pool("IPSA_POOL", FALSE)    { Initialize(); }

  ~IPSA_COMP_DRIVER(void) {}

  MEM_POOL *Mem_pool()                        { return _pool(); }
  char     *Workdir()                         { return _workdir; }
  void      Add_input_file(char *f)           { _inputs.push_back(f); }
  void      Initialize();
  void      Process_options();
  int       Compile();
  int       Compile_with_make();
  int       Exec(int argc, char **argv);
  void      Gen_option(vector<char *> &arg_list);
  char     *Gen_makefile();
  char     *Gen_cmdfile(char *makefile);
  int       Create_dir(char *dir);
};

#endif
