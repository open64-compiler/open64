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

#ifndef opt_vsa_jni_INCLUDED
#define opt_vsa_jni_INCLUDED "opt_vsa_jni.h"

class VFR_CAND
{
  private:
  char   *_class_name;
  char   *_fld_name;
  IDTYPE  _fld_id;

  VFR_CAND(void);                               // REQUIRED UNDEFINED UNWANTED methods
  VFR_CAND(const VFR_CAND&);                    // REQUIRED UNDEFINED UNWANTED methods
  VFR_CAND& operator = (const VFR_CAND&);       // REQUIRED UNDEFINED UNWANTED methods

  public:
  VFR_CAND(char *cls_name, char *fld_name, IDTYPE id=0) : 
    _class_name(cls_name), _fld_name(fld_name),
    _fld_id(id) {}

  void   Set_fld_id(IDTYPE fld_id) { _fld_id = fld_id; }
  IDTYPE Get_fld_id()            { return _fld_id; }
  char * Get_class_name()        { return _class_name; }
  char * Get_fld_name()          { return _fld_name; }
  BOOL   Match(char *cls_name, IDTYPE fid)
  {
    if(fid == _fld_id && strcmp(cls_name, _class_name) == 0) {
      return TRUE;
    }
    return FALSE;
  }
};

typedef vector<VFR_CAND *> VFR_CANDS;
#endif
