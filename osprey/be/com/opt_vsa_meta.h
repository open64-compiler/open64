//-*-c++-*-

/*
   Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

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

// ====================================================================
// ====================================================================
//
// Module: opt_vsa_meta.h
//
// ====================================================================
//

#ifndef opt_vsa_meta_INCLUDED
#define opt_vsa_meta_INCLUDED        "opt_vsa_meta.h"

#include <stdio.h>
typedef unsigned char  mUINT8;
typedef unsigned short mUINT16;
typedef unsigned int   mUINT32;

enum VSA_LANGUAGE {
  VL_C     = 0x01,                 // C
  VL_CXX   = 0x02,                 // C++
  VL_C_CXX = VL_C | VL_CXX,
  VL_JAVA  = 0x04,                 // Java
};

enum RULESET_CATEGORY {
  RSC_BUILTIN,                     // builtin ruleset
  RSC_STANDARD,                    // standard ruleset
  RSC_USER,                        // user ruleset
};

enum RULE_CATEGORY {
  RC_SECURITY,                     // rule about security
  RC_ROBUSTNESS,                   // rule about robustness
  RC_CORRECTNESS,                  // rule about correctness
  RC_PERFORMANCE,                  // rule about performance
  RC_BAD_PRACTICE,                 // rule about bad practice
  RC_CODING_STYLE,                 // rule about coding style
  RC_USER,                         // user rule category
};

enum RULE_SEVERITY {
  SEV_LOW,                         // low severity
  SEV_MEDIUM,                      // medium severity
  SEV_HIGH,                        // high severity
};

enum RULE_PRIORITY {
  PRI_LOW,                         // low priority
  PRI_MEDIUM,                      // medium priority
  PRI_HIGH,                        // high priority
  ORI_CRITICAL,                    // critical priority
};

enum RULE_LIKELIHOOD {
  RL_UNLIKELY,                     // unlikely occur
  RL_POSSIBLE,                     // possible occur
  RL_LIKELY,                       // likely occur
};

enum RULE_FIX_COST {
  FC_LOW,                          // low cost to fix
  FC_MEDIUM,                       // medium cost to fix
  FC_HIGH,                         // high cost to fix
};

enum ISSUE_CERTAINTY {
  IC_DEFINITELY,                   // issue definitely happen
  IC_MAYBE,                        // issue maybe happen
};

struct RULE_META {
  mUINT32     _id;                 // rule id used internally
  mUINT32     _languages;          // languages supported by the rule
  const char *_code;               // rule unique code
  const char *_name;               // rule name
  const char *_description;        // rule description
  const char *_details;            // rule details
  const char *_rule_url;           // url to rule web page
  mUINT8      _category;           // category
  mUINT8      _severity;           // severity
  mUINT8      _priority;           // priority
  mUINT8      _likelihood;         // likelihood
  mUINT8      _fix_cost;           // fix cost
  void        Print(int indent, FILE* fp);
};

struct RULESET_META {
  mUINT32     _id;                 // ruleset id used internally
  mUINT32     _languages;          // languages supported by the rule set
  mUINT32     _category;           // ruleset category
  const char *_name;               // ruleset name
  const char *_version;            // ruleset version
  const char *_revision;           // ruleset revision
  const char *_description;        // ruleset description
  const char *_ruleset_url;        // url to ruleset web page
  const char *_provider;           // ruleset provider
  const char *_provider_url;       // url to provider web page
  const char *_license;            // ruleset license
  const char *_license_url;        // url to ruleset license
  struct RULE_META *_rules;        // rules in this ruleset
  void        Print(int indent, FILE* fp);
};

struct ENGINE_META {
  const char *_name;               // engine name
  const char *_version;            // engine version
  const char *_revision;           // engine revision
  const char *_description;        // engine description
  mUINT32     _languages;          // languages supported by the engine
  const char *_engine_url;         // url to engine web page
  const char *_provider;           // engine provider
  const char *_provider_url;       // url to provider web page
  const char *_license;            // license
  const char *_license_url;        // url to license web page
  struct RULESET_META **_rulesets; // ruleset supported by this engine
  void        Print(int indent, FILE* fp);
};

inline const char*
VSA_LANGUAGE_name(char* buf, int max_len, mUINT32 lang)
{
  int need_comma = 0;
  int bit = 0;
  const char* bit_desc[] = { "c", "c++", "java", ""};
  int offset = 0;
  int temp = lang;
  while (temp != 0 && bit < sizeof(bit_desc)/sizeof(bit_desc[0])) {
    if ((temp & (1<<bit)) != 0) {
      if (need_comma == 0) {
        need_comma = 1;
      }
      else if (offset < max_len - 2) {
        buf[offset++] = ',';
      }
      const char* str = bit_desc[bit];
      while (*str != '\0' && offset < max_len) {
        buf[offset++] = *(str++);
      }
      temp &= ~(1<<bit);
    }
    buf[offset] = '\0';
    ++bit;
  }
  return buf;
}

inline const char*
RULESET_CATEGORY_name(mUINT32 cat)
{
  switch (cat) {
  case RSC_BUILTIN:     return "BUILTIN";
  case RSC_STANDARD:    return "STANDARD";
  case RSC_USER:        return "USER";
  default:              return "-error-";
  }
}

inline const char*
RULE_CATEGORY_name(mUINT32 cat)
{
  switch (cat) {
  case RC_SECURITY:     return "Vul";
  case RC_ROBUSTNESS:   return "ROBUSTNESS";
  case RC_CORRECTNESS:  return "CORRECTNESS";
  case RC_PERFORMANCE:  return "Pfm";
  case RC_BAD_PRACTICE: return "BAD_PRACTICE";
  case RC_CODING_STYLE: return "CODING_STYLE";
  case RC_USER:         return "USER";
  default:              return "-error-";
  }
}

inline const char*
RULE_SEVERITY_name(mUINT32 sev)
{
  switch (sev) {
  case SEV_LOW:         return "LOW";
  case SEV_MEDIUM:      return "MEDIUM";
  case SEV_HIGH:        return "HIGH";
  default:              return "-error-";
  }
}

inline const char*
RULE_PRIORITY_name(mUINT32 pri)
{
  switch (pri) {
  case PRI_LOW:         return "LOW";
  case PRI_MEDIUM:      return "MEDIUM";
  case PRI_HIGH:        return "HIGH";
  case ORI_CRITICAL:    return "CRITICAL";
  default:              return "-error-";
  }
}

inline const char*
RULE_LIKELIHOOD_name(mUINT32 likely)
{
  switch (likely) {
  case RL_UNLIKELY:     return "UNLIKELY";
  case RL_POSSIBLE:     return "POSSIBLE";
  case RL_LIKELY:       return "LIKELY";
  default:              return "-error-";
  }
}

inline const char*
RULE_FIX_COST_name(mUINT32 cost)
{
  switch (cost) {
  case FC_LOW:          return "LOW";
  case FC_MEDIUM:       return "MEDIUM";
  case FC_HIGH:         return "HIGH";
  default:              return "-error-";
  }
}

inline const char*
ISSUE_CERTAINTY_name(mUINT32 certainty)
{
  switch (certainty) {
  case IC_DEFINITELY:   return "D";
  case IC_MAYBE:        return "M";
  default:              return "-error-";
  }
}

inline const char*
ISSUE_CERTAINTY_desc(mUINT32 certainty)
{
  switch (certainty) {
  case IC_DEFINITELY:   return "definitely";
  case IC_MAYBE:        return "maybe";
  default:              return "-error-";
  }
}

#endif /* opt_vsa_meta_INCLUDED */

