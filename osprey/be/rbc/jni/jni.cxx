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

#include <stdio.h>
#include <signal.h>
#include <wchar.h>
#include "rbc_base.h"
#ifdef __cplusplus
#include <stdio.h>
extern "C" {
#endif
extern RBC_ENGINE rbc;
void *CallObjectMethod(void *env, void *object, void *methodID)
{
  rbc.Model_decl(rbc.Jni_model_pragma());
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  return NULL;
}
void *GetFieldID(void *env, void * cls, void *field_name, void *type_name)
{
  rbc.Model_decl(rbc.Jni_model_pragma());
  // rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  return NULL;
}

void *GetObjectField(void *env, void *object, void *fid)
{
  rbc.Model_decl(rbc.Jni_model_pragma());
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_parm_base_and_fld_name(rbc.Get_arg(2), rbc.Get_arg(3)));
  return NULL;
}

void *GetObjectClass(void *env, void *object)
{
  rbc.Model_decl(rbc.Jni_model_pragma());
  return NULL;
}

void SetObjectField(void *env, void *object, void *fid, void *value)
{
  rbc.Model_decl(rbc.Jni_model_pragma());
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_parm_base_and_fld_name(rbc.Get_arg(2), rbc.Get_arg(3)));
}

#ifdef __cplusplus
} // extern C
#endif
