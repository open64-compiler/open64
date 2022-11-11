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

#include "sys_base.h"
#include "rbc_base.h"

#ifdef __cplusplus
extern "C" {
#endif

RBC_ENGINE rbc;

void __RBC_GLOBAL__()
{
  rbc.Rbc_enable_builtin("CRF");
  // limit stack size 32k for aw
  // rbc.For_all_exec_path(rbc.Get_max_stack_size() < 32*1024, "CSS");
  // limit call depth 50 for aw
  // rbc.For_all_exec_path(rbc.Get_max_call_depth() < 50, "CSL");
}


#ifdef __cplusplus
} // extern C
#endif

