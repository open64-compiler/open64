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
#include "builtin_rule_defs.h"
#include <stdio.h>

#define ENGINE_VERSION   "1"
#define ENGINE_REVISION  "1.0"
#define RULESET_VERSION  "1"
#define RULESET_REVISION "1.0"

// DO NOT change the struct initialization data
// To add new rule, please modify builtin_rule_meta.desc
static struct RULE_META    _builtin_rules[] =
{
#define DECLARE_RULE(id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost) \
                    {id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost},
#include "builtin_rule_meta.desc"
#undef  DECLARE_RULE
  // sentinel
  {
    __UINT32_MAX__, 0, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0
  }
};

static struct RULESET_META _builtin_ruleset =
{ 
  0,
  VL_C | VL_CXX | VL_JAVA,
  RSC_BUILTIN,
  "BUILTIN",
  RULESET_VERSION,
  RULESET_REVISION,
  "Xcalibyte static analyzer builtin ruleset",
  "",
  "Xcalibyte",
  "http://www.xcalibyte.com",
  "Xcalibyte commercial license",
  "",
  _builtin_rules
};

static struct RULESET_META* _xvsa_rulesets[] =
{
  &_builtin_ruleset,
  NULL
};

static struct ENGINE_META _xvsa_engine =
{
  "Xcalibyte",
  ENGINE_VERSION,
  ENGINE_REVISION,
  "Xcalibyte static analyzer",
  VL_C | VL_CXX | VL_JAVA,
  "http://www.xcalibyte.com",
  "Xcalibyte",
  "http://www.xcalibyte.com",
  "Xcalibyte commercial license",
  "",
  _xvsa_rulesets
};

const char*
Get_builtin_rule_code(mUINT32 rule)
{
  if (rule < sizeof(_builtin_rules)/sizeof(_builtin_rules[0]))
    return _builtin_rules[rule]._code;
  return NULL;
}

extern mUINT32
Get_builtin_rule_fix_cost(mUINT32 rule)
{
  if (rule < sizeof(_builtin_rules)/sizeof(_builtin_rules[0]))
    return _builtin_rules[rule]._fix_cost * 2 + 1;
  return 1;
}

extern "C" struct ENGINE_META*
Get_engine_meta()
{
  return &_xvsa_engine;
}

extern "C" struct RULESET_META**
Get_rulesets_meta()
{
  return _xvsa_rulesets;
}

