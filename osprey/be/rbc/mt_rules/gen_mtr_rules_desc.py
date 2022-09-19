"""
   Copyright (C) 2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
"""

import ruleset_meta as rsm
import rule_meta as rm
from cst_to_py import *

rs = rsm.Ruleset("XCALMTR",
                 "XCALMTR",
                 "1",
                 "1.0",
                 "Xcalibyte multi-thread ruleset",
                 "c,c++,java",
                 "Xcalibyte multi-thread ruleset\n"
                 "Implemented by Xcalibyte",
                 "Xcalibyte",
                 "http://www.xcalibyte.com",
                 "Xcalibyte commercial license",
                 "")

def add_rule(lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost) :
    rs.addRule(rm.Rule(code,
                       name,
                       desc,
                       detail,
                       lang_to_str(lang),
                       c2p[category],
                       c2p[severity],
                       c2p[priority],
                       c2p[likelihood],
                       c2p[fix_cost],
                       url))
    pass


#define DECLARE_RULE(id, lang, code, name, desc, detail, url, category, severity, priority, likelihood, fix_cost) \
add_rule(lang, code, name, desc, detail, url, #category, #severity, #priority, #likelihood, #fix_cost)
#include "mtr_rules_meta.desc"
#undef DECLARE_RULE

if __name__ == "__main__":
    import json
    print (json.dumps(rs, default=lambda o: o.__dict__, indent=1, sort_keys=True))


