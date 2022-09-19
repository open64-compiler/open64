#!/usr/bin/env python3
# encoding: utf-8

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

# ATTENTION: temporarily copied from scanner-connector/model/rule_meta.py
# Do NOT modify this file directly.
# TODO: find a better way to organize the project to avoid the file copy

# Module Name: rule_meta.py
# model definition for rule
# a rule is defined as a template for an issue

from enum import Enum
import json

# RuleCategory
class RuleCategory:
    """
    define rule category
    """
    Correctness = "CORRECTNESS"
    Security = "VUL"
    Robustness = "ROBUSTNESS"
    Performance = "PFM"
    BadPractice = "BAD_PRACTICE"
    CodingStyle = "CODING_STYLE"
    User = "USER"

# RuleSeverity
class RuleSeverity:
    """
    define rule severity
    """
    Low = "LOW"
    Medium = "MEDIUM"
    High = "HIGH"

# RulePriority
class RulePriority:
    """
    define rule priority
    """
    Low = "LOW"
    Medium = "MEDIUM"
    High = "HIGH"
    Critical = "CRITICAL"

# RulePriority
class RuleLikelyhood:
    """
    define rule likelyhood
    """
    Unlikely = "UNLIKELY"
    Possible = "POSSIBLE"
    Likely = "LIKELY"

# RulePriority
class RuleFixCost:
    """
    define rule fix cost
    """
    Low = "LOW"
    Medium = "MEDIUM"
    High = "HIGH"


# Rule
class Rule():
    """
    describe the meta info for a rule
    """
    def __init__(self,
                 code,               # unique code of the rule
                 name,               # name of the rule
                 description,        # description
                 details,            # defails
                 languages,          # language
                 category,           # category
                 severity,           # severity
                 priority,           # priority
                 likelyhood,         # likelyhood
                 fix_cost,           # fix cost
                 rule_url = "",      # url to rule webpage
                 extra_attrs = None  # extra attributes, must be dict
                ):
        self.code = code
        self.name = name
        self.description = description
        self.details = details
        self.languages = languages
        self.category = category
        self.severity = severity
        self.priority = priority
        self.likelyhood = likelyhood
        self.fix_cost = fix_cost
        self.rule_url = rule_url
        self.extra_attrs = extra_attrs

    """
    set  methods
    """
    def setCode(self, code):
        self.code = code

    def setCategory(self, cate):
        self.category = cate

    def setDetails(self, detail):
        self.details = detail

    def setSDesc(self, desc):
        self.description = desc

    def setName(self, name):
        self.name = name

    def setExtraAttrs(self, val):
        self.extra_attrs = val

    def addExtraAttrs(self, val):
        self.extra_attrs.update(val)

    """
    get methods
    """
    def getExtraAttrs(self):
        return self.extra_attrs

# Test main
if __name__ == "__main__":
    import json
    rl = Rule("rule_code_1",
              "check_issue_1",
              "description for the rule",
              "details with code sample",
              "c",
              RuleCategory.Security,
              RuleSeverity.Medium,
              RulePriority.High,
              RuleLikelyhood.Likely,
              RuleFixCost.Medium,
              "http://company.com/ruleset/rule_code_1",
              {
                "CWE": "cwe10",
                "CVE": "cve-10-2345"
              }
             )
    print (json.dumps(rl, default=lambda o: o.__dict__, indent=1, sort_keys=True))
