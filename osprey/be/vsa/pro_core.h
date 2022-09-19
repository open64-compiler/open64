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
// pro_core.h
//
// wrapper switches for VSA
// ====================================================================
#ifndef pro_core_INCLUDED
#define pro_core_INCLUDED

#include "config_vsa.h"

#define STRING_DATA const char
#define S3_FINISH FALSE

#define PRO_anchor_each(a, b, c, d, e)
#define PRO_option_padding()
#define PRO_phase_one_fetch_patch_data() ""
#define PRO_phase_two_apply(a)

#define VSA_Aob() VSA_Aob
#define VSA_Builtin_Jni() VSA_Builtin_Jni
#define VSA_Compiletime_Triage() VSA_Compiletime_Triage
#define VSA_Dbf() VSA_Dbf
#define VSA_EH() VSA_EH
#define VSA_Enable_FSM() VSA_Enable_FSM
#define VSA_Enable_Taint_Model() VSA_Enable_Taint_Model
#define VSA_Experimental() VSA_Experimental
#define VSA_Json_Path_Max() VSA_Json_Path_Max
#define VSA_Model_Lda() VSA_Model_Lda
#define VSA_Msf() VSA_Msf
#define VSA_New_Npd_Checker() VSA_New_Npd_Checker
#define VSA_New_Heap_Checker() VSA_New_Heap_Checker
#define VSA_New_Uiv_Checker() VSA_New_Uiv_Checker
#define VSA_Npd() VSA_Npd
#define VSA_Path_Head_Max() VSA_Path_Head_Max
#define VSA_Path_Tail_Max() VSA_Path_Tail_Max
#define VSA_Ral() VSA_Ral
#define VSA_Rbc() VSA_Rbc
#define VSA_Uaf() VSA_Uaf
#define VSA_Uiv() VSA_Uiv
#define VSA_Udr() VSA_Udr
#define VSA_Vra() VSA_Vra
#define VSA_Vsym_Memcall() VSA_Vsym_Memcall

#endif /* pro_core_INCLUDED */

