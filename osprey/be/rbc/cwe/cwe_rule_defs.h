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

#ifndef cwe_rule_defs_INCLUDED
#define cwe_rule_defs_INCLUDED

// DO NOT change this file
// To add new rule, please modify cwe_rule_meta.desc

enum {
#define DECLARE_RULE(id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost) \
                     id,
#include "cwe_rule_meta.desc"
#undef  DECLARE_RULE
};

extern const char* Get_cwe_rule_code(mUINT32 rule);

#endif /* cwe_rule_defs_INCLUDED */
