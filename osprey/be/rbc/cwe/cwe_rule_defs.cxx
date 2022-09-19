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
#include "cwe_rule_defs.h"
#include <stdio.h>

// DO NOT change the struct initialization data
// To add new rule, please modify cwe_rule_meta.desc
static struct RULE_META    _cwe_rules[] =
{
#define DECLARE_RULE(id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost) \
                    {id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost},
#include "cwe_rule_meta.desc"
#undef  DECLARE_RULE
  // sentinel
  {
    -1, 0, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0
  }
};

static struct RULESET_META _cwe_ruleset =
{ 
  0,
  VL_C | VL_CXX,
  RSC_STANDARD,
  "XVSA-STANDARD",
  "1.0",
  "Xcalibyte static analyzer standard ruleset",
  "",
  "Xcalibyte",
  "http://www.xcalibyte.com",
  "Xcalibyte",
  "",
  _cwe_rules
};

static struct RULESET_META* _xvsa_rulesets[] =
{
  &_cwe_ruleset,
  NULL
};

static struct ENGINE_META _xvsa_engine =
{
  "XVSA",
  "1.0",
  "Xcalibyte static analyzer",
  VL_C | VL_CXX | VL_JAVA,
  "http://www.xcalibyte.com",
  "Xcalibyte",
  "http://www.xcalibyte.com",
  "Xcalibyte",
  "",
  _xvsa_rulesets
};

const char*
Get_cwe_rule_code(mUINT32 rule)
{
  if (rule < sizeof(_cwe_rules)/sizeof(_cwe_rules[0]))
    return _cwe_rules[rule]._code;
  return NULL;
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
