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

#include "opt_vsa_meta.h"
#include "rbc_rule_defs.h"
#include <stdio.h>

// DO NOT change the struct initialization data
// To add new rule, please modify builtin_rule_meta.desc
static struct RULE_META    _rbc_rules[] =
{
#define DECLARE_RULE(id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost) \
                    {_##id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost},
#include "builtin_rbc_rule_meta.desc"
#include "builtin_certc_rule_meta.desc"
#include "builtin_fsm_rule_meta.desc"
#undef  DECLARE_RULE
  // sentinel
  {
    __UINT32_MAX__, 0, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0
  }
};

static struct RULESET_META _rbc_ruleset =
{ 
  0,
  VL_C | VL_CXX | VL_JAVA,
  RSC_BUILTIN,
  "XVSA-BUILTIN-RBC",
  "1",
  "1.0",
  "Xcalibyte static analyzer builtin RBC ruleset",
  "",
  "Xcalibyte",
  "http://www.xcalibyte.com",
  "Xcalibyte",
  "",
  _rbc_rules
};

static struct RULESET_META* _rbc_rulesets[] =
{
  &_rbc_ruleset,
  NULL
};

const char*
Get_rbc_rule_name(mUINT32 rule)
{
  if (rule < sizeof(_rbc_rules)/sizeof(_rbc_rules[0]))
    return _rbc_rules[rule]._code;
  return NULL;
}

extern "C" struct RULESET_META**
Get_rulesets_meta()
{
  return _rbc_rulesets;
}

