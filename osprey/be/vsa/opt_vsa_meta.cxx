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

//-*-c++-*-
//
// ====================================================================
//
// Module: opt_vsa_meta.cxx
//
// ====================================================================
//
#include <stdio.h>
#include "opt_vsa_meta.h"

#define MAX_LANG_LEN 32

static void
Print_str(FILE* fp, const char* str)
{
  char ch;
  while ((ch = *str++) != '\0') {
    switch (ch) {
    case '\r': continue;
    case '\n': fputs("\\n", fp); break;
    case '\\': fputs("\\", fp);  break;
    case '"' : fputs("\\\"", fp);  break;
    default:   fputc(ch, fp);      break;
    }
  }
}

static void
Print_kv(int indent, FILE* fp, const char* key, const char* value, bool comma)
{
  fprintf(fp, "%*s\"%s\": \"",
              indent, "", key);
  Print_str(fp, value);
  if (comma)
    fprintf(fp, "\",\n");
  else
    fprintf(fp, "\"");
}

void
ENGINE_META::Print(int indent, FILE* fp)
{
  char lang[MAX_LANG_LEN];
  VSA_LANGUAGE_name(lang, sizeof(lang), _languages);

  fprintf(fp, "%*s{\n", indent, "");
  Print_kv(indent + 1, fp, "description", _description, true);
  Print_kv(indent + 1, fp, "engine_url", _engine_url, true);
  Print_kv(indent + 1, fp, "languages", lang, true);
  Print_kv(indent + 1, fp, "license", _license, true);
  Print_kv(indent + 1, fp, "license_url", _license_url, true);
  Print_kv(indent + 1, fp, "name", _name, true);
  Print_kv(indent + 1, fp, "provider", _provider, true);
  Print_kv(indent + 1, fp, "provider_url", _provider_url, true);
  Print_kv(indent + 1, fp, "version", _version, true);
  Print_kv(indent + 1, fp, "revision", _revision, false);

  struct RULESET_META **rs = _rulesets;
  struct RULESET_META *r = *rs;
  if (r != NULL) {
    fprintf(fp, ",\n%*s\"rulesets\": [\n",
                indent + 1, "");
    int need_comma = 0;
    while (r != NULL) {
      if (need_comma == 0)
        need_comma = 1;
      else
        fprintf(fp, ",\n");
      r->Print(indent + 2, fp);
      r = *(++rs);
    }
    fprintf(fp, "\n%*s]\n",
                indent + 1, "");
  }
  fprintf(fp, "%*s}\n",
              indent, "");
}

void
RULESET_META::Print(int indent, FILE* fp)
{
  char lang[MAX_LANG_LEN];
  VSA_LANGUAGE_name(lang, sizeof(lang), _languages);
  fprintf(fp, "%*s{\n",
              indent, "");
  Print_kv(indent + 1, fp, "description", _description, true);
  Print_kv(indent + 1, fp, "languages", lang, true);
  Print_kv(indent + 1, fp, "license", _license, true);
  Print_kv(indent + 1, fp, "license_url", _license_url, true);
  Print_kv(indent + 1, fp, "name", _name, true);
  Print_kv(indent + 1, fp, "display_name", _name, true);
  Print_kv(indent + 1, fp, "provider", _provider, true);
  Print_kv(indent + 1, fp, "provider_url", _provider_url, true);
  Print_kv(indent + 1, fp, "ruleset_url", _ruleset_url, true);
  Print_kv(indent + 1, fp, "version", _version, true);
  Print_kv(indent + 1, fp, "revision", _revision, false);
  struct RULE_META* rule = _rules;
  if (rule->_id != -1) {
    fprintf(fp, ",\n%*s\"rules\": [\n",
                indent + 1, "");
    int need_comma = 0;
    while (rule->_id != -1) {
      if (need_comma == 0)
        need_comma = 1;
      else
        fprintf(fp, ",\n");
      rule->Print(indent + 2, fp);
      ++rule;
    }
    fprintf(fp, "\n%*s]\n",
                indent + 1, "");
  }
  fprintf(fp, "%*s}",
              indent, "");
}

void
RULE_META::Print(int indent, FILE* fp)
{
  char lang[MAX_LANG_LEN];
  VSA_LANGUAGE_name(lang, sizeof(lang), _languages);
  fprintf(fp, "%*s{\n",
              indent, "");
  Print_kv(indent + 1, fp, "category", RULE_CATEGORY_name(_category), true);
  Print_kv(indent + 1, fp, "code", _code, true);
  Print_kv(indent + 1, fp, "description", _description, true);
  Print_kv(indent + 1, fp, "details", _details, true);
  Print_kv(indent + 1, fp, "fix_cost", RULE_FIX_COST_name(_fix_cost), true);
  Print_kv(indent + 1, fp, "languages", lang, true);
  Print_kv(indent + 1, fp, "likelyhood", RULE_LIKELIHOOD_name(_likelihood), true);
  Print_kv(indent + 1, fp, "name", _name, true);
  Print_kv(indent + 1, fp, "priority", RULE_PRIORITY_name(_priority), true);
  Print_kv(indent + 1, fp, "severity", RULE_SEVERITY_name(_severity), false);
  fprintf(fp, "\n%*s}",
              indent, "");
}

