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

/* macros to declare rbc for c++ common intrinsic */

#define DECL_RBC_for_container_init() \
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_func_container_init(rbc.Get_arg(1), 0));


#define DECL_RBC_for_container_append() \
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2))); \
  rbc.Model_decl(rbc.Merge_tag(rbc.Get_arg(1), rbc.Get_arg(1), rbc.Get_arg(2))); \
  rbc.Model_decl(rbc.Set_func_coll_append_ref(rbc.Get_arg(1), rbc.Get_arg(2)));

#define DECL_RBC_for_container_back() \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_func_coll_back_ref(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Eval_tag(rbc.Get_ret(), rbc.Get_arg(1)));

#define DECL_RBC_for_map_get() \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_func_map_get_ref(rbc.Get_arg(1), rbc.Get_arg(2)));

#include "rbc_intrn_cxx_defs_std_vector.inc"
#include "rbc_intrn_cxx_defs_std_list.inc"
#include "rbc_intrn_cxx_defs_std_map.inc"
