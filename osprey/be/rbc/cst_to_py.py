#!/usr/bin/env python3

#  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

import ruleset_meta as rsm
import rule_meta as rm

c2p = {}
# rule category
c2p["RC_SECURITY"]     = rm.RuleCategory.Security
c2p["RC_ROBUSTNESS"]   = rm.RuleCategory.Robustness
c2p["RC_CORRECTNESS"]  = rm.RuleCategory.Correctness
c2p["RC_PERFORMANCE"]  = rm.RuleCategory.Performance
c2p["RC_BAD_PRACTICE"] = rm.RuleCategory.BadPractice
c2p["RC_CODING_STYLE"] = rm.RuleCategory.CodingStyle
c2p["RC_USER"]         = rm.RuleCategory.User

# severity
c2p["SEV_LOW"]         = rm.RuleSeverity.Low
c2p["SEV_MEDIUM"]      = rm.RuleSeverity.Medium
c2p["SEV_HIGH"]        = rm.RuleSeverity.High

# priority
c2p["PRI_LOW"]         = rm.RulePriority.Low
c2p["PRI_MEDIUM"]      = rm.RulePriority.Medium
c2p["PRI_HIGH"]        = rm.RulePriority.High
c2p["ORI_CRITICAL"]    = rm.RulePriority.Critical

# likelihood
c2p["RL_UNLIKELY"]     = rm.RuleLikelyhood.Unlikely
c2p["RL_POSSIBLE"]     = rm.RuleLikelyhood.Possible
c2p["RL_LIKELY"]       = rm.RuleLikelyhood.Likely

# fix cost
c2p["FC_LOW"]          = rm.RuleFixCost.Low
c2p["FC_MEDIUM"]       = rm.RuleFixCost.Medium
c2p["FC_HIGH"]         = rm.RuleFixCost.High

# language
VL_C    = 1
VL_CXX  = 2
VL_JAVA = 4


def append_str(str1, str2):
    if str1 is None or str1 == "":
        return str2
    else:
        return str1 + "," + str2
    pass

def lang_to_str(lang):
    ret = ""
    if lang & VL_C == VL_C:
        ret = "c"
    if lang & VL_CXX == VL_CXX:
        ret = append_str(ret, "c++")
    if lang & VL_JAVA == VL_JAVA:
        ret = append_str(ret, "java")
    return ret
