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

/* macros to declare rbc for c++ intrinsic */
#include "rbc_intrn_cxx_defs.inc"

/* collection of all c++ intrinsic model and check files */

#include "rbc_intrn_cxx_std_map.inc"
#include "rbc_intrn_cxx_std_vector.inc"
#include "rbc_intrn_cxx_std_tie.inc"
#include "rbc_intrn_cxx_gnu_hash_map.inc"
#include "rbc_intrn_cxx_gnu_hash_multimap.inc"
#include "rbc_intrn_cxx_gnu_hash_multiset.inc"
#include "rbc_intrn_cxx_gnu_hash_set.inc"
#include "rbc_intrn_cxx_std_array.inc"
#include "rbc_intrn_cxx_std_deque.inc"
#include "rbc_intrn_cxx_std_list.inc"
#include "rbc_intrn_cxx_std_multimap.inc"
#include "rbc_intrn_cxx_std_multiset.inc"
#include "rbc_intrn_cxx_std_set.inc"
#include "rbc_intrn_cxx_std_unordered_map.inc"
#include "rbc_intrn_cxx_std_unordered_multimap.inc"
#include "rbc_intrn_cxx_std_unordered_multiset.inc"
#include "rbc_intrn_cxx_std_unordered_set.inc"
/* DO NOT EDIT THIS MARK */

#ifdef __cplusplus
} // extern C
#endif

