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

# ATTENTION: temporarily copied from scanner-connector/model/ruleset_meta.py
# Do NOT modify this file directly.
# TODO: find a better way to organize the project to avoid the file copy

# Module Name: ruleset_meta.py
# model definition for ruleset
# a ruleset is defined as a stonealone package contains many rules

from enum import Enum
import json

# RuleLanguage
class RuleLanguage(Enum):
    """
    define languages applicable to a rule set
    """
    C = "c"
    CXX = "c++"
    Java = "java"

# RulesetCategory
class RulesetCategory(Enum):
    """
    define ruleset cateogory
    """
    Builtin = "BUILTIN"
    Standard = "STANDARD"
    User = "USER"

# Ruleset
class Ruleset:
    """
    define meta info for a rule set
    """
    def __init__(self,
                 name,               # name of the ruleset
                 display_name,       # display name of the ruleset
                 version,            # version of the ruleset for compatibility purpose
                 revision,           # revision of the ruleset
                 description,        # description of the ruleset
                 languages,          # languages applicable to this ruleset
                 ruleset_url = "",   # url to ruleset webpage
                 provider = "",      # ruleset provider
                 provider_url = "",  # url to provider webpage
                 license = "",       # license of this ruleset
                 license_url = "",   # url to license webpage
                 extra_attrs = None  # dict for extra attributes
                ):
        self.name = name
        self.display_name = display_name
        self.version = version
        self.revision = revision
        self.description = description
        self.languages = languages
        self.ruleset_url = ruleset_url
        self.provider = provider
        self.provider_url = provider_url
        self.license = license
        self.license_url = license_url
        self.extra_attrs = extra_attrs
        self.rules = []

    """
    add a rule to ruleset
    """
    def addRule(self, rule):
        self.rules.append(rule)


"""
test method to test Ruleset
"""
if __name__ == "__main__":
    import json
    rs = Ruleset("test_ruleset",
                 "test ruleset",
                 "1",
                 "1.3",
                 "test ruleset",
                 "c, c++, java",
                 "http://company.com/ruleset/test_ruleset",
                 "company",
                 "http://company.com",
                 "commercial license",
                 "http://company.com/license.html",
                 {
                     "update_url": "http://company.com/ruleset/update?test_ruleset",
                     "secret" : "AAAABBBBCCCCDDDDEEEEFFFF"
                 }
                )
    print (json.dumps(rs, default=lambda o: o.__dict__, indent=1, sort_keys=True))

    rs = Ruleset("test_ruleset",
                 "test ruleset",
                 "1.0",
                 "1.0.4",
                 "test ruleset",
                 "c, c++, java",
                )
    rs.addRule("test-rule-1")
    rs.addRule("test-rule-2")
    print (json.dumps(rs, default=lambda o: o.__dict__, indent=1, sort_keys=True))
