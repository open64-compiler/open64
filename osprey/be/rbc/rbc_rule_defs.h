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

#ifndef rbc_rule_defs_INCLUDED
#define rbc_rule_defs_INCLUDED

// DO NOT change this file
// To add new rule, please modify rbc_rule_meta.desc

enum RBC_RULE_ID {
#define DECLARE_RULE(id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost) \
                     _##id,
#include "builtin_rbc_rule_meta.desc"
#include "builtin_certc_rule_meta.desc"
#include "builtin_fsm_rule_meta.desc"
#undef  DECLARE_RULE
  RBC_rule_last,
};

const char*
Get_rbc_rule_name(mUINT32 rule);

#define CERT_C_RULESET     "CERT"

#endif /* rbc_rule_defs_INCLUDED */

