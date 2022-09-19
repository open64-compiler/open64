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

#include "vsa_annot.h"
#include "vsa_annot_ext.h"
#include "erglob.h"
#include "opt_vsa.h"

// ==================================================================
// VANT_UTIL
// ==================================================================

// each annotation is named with 1 character
static const char*
BuiltinAnnotName(ANT_KIND annot)
{
  static const char *name[ANT_VSYM] = {
    "zero",
    "const",
    "read",
    "write",
    "divisor",
    "lb",
    "ub",
    "malloc",
    "free",
    "realloc",
    "formal",
    "actual",
    "global",
    "retval"
  };
  Is_True(annot < ANT_VSYM, ("bad annot"));
  return name[annot];
}

void
VANT_UTIL::Dump(FILE *fp, V_ANNOT v)
{
  INT i;
  BOOL space = FALSE;
  for (i = 0; i < ANT_KIND_LAST; ++i) {
    ANT_KIND annot = (ANT_KIND)i;
    ANT_STAT stat = Get(v, annot);
    if (stat == ANT_NO)
      continue;
    if (space)
      fprintf(fp, " ");
    else
      space = TRUE;
    const char *prefix = "";
    if (annot >= ANT_VSYM) {
      prefix = "v";
      annot = (ANT_KIND)(annot - ANT_VSYM);
    }
    const char *name = BuiltinAnnotName(annot);
    fprintf(fp, "%s%s-%s%s",
            prefix, name,
            (stat & ANT_MAYBE) ? "m" : "",
            (stat & ANT_YES) ? "y" : "");
  }
}

void
VANT_UTIL::Dump(V_ANNOT v)
{
  Dump(stdout, v);
  fputs("\n", stdout);
}

// ==================================================================
// EANT_UTIL
// ==================================================================

static const char *
ExtAnnotName(EANT_KIND annot)
{
  static const char * name[ANT1_LAST] = {
    "length", "assign", "is_null", "not_null"
  };
  return annot < ANT1_LAST ? name[annot] : "ANT_err";
}

void
EANT_UTIL::Dump(FILE *fp) const
{
  BOOL space = FALSE;
  UINT32 idx = 0;
  while (idx < _size) {
    if (space)
      fprintf(fp, " ");
    else
      space = TRUE;
    UINT8 v = _annot[idx];
    EANT_KIND x = (EANT_KIND)(v & 0x7);
    fprintf(fp, "%s(%d)",
            ExtAnnotName(x), v >> 3);
    idx += Skip(x);
  }
  fprintf(fp, "\n");
}

// =============================================================================
// V_ANNOT Get_cr_annot(VSA *vsa, CODEREP *cr)
// return annotation on general coderep
// =============================================================================
V_ANNOT
Get_cr_annot(VSA *vsa, CODEREP *cr)
{
  VSYM_OBJ_REP *vor;
  V_ANNOT v = VANT_UTIL::Empty();
  switch (cr->Kind()) {
  case CK_LDA:
    v = VANT_UTIL::Init(ANT_CONST, ANT_YES);
    break;
  case CK_CONST:
    v = VANT_UTIL::Init(ANT_CONST, ANT_YES);
    if (cr->Const_val() == 0)
      v = VANT_UTIL::Set(v, ANT_ZERO, ANT_YES);
    break;
  case CK_RCONST:
    v = VANT_UTIL::Init(ANT_CONST, ANT_YES);
    if (cr->Const_fval() == 0.0)
      v = VANT_UTIL::Set(v, ANT_ZERO, ANT_YES);
    break;
  case CK_VAR:
    v = cr->Vsa_annot();
    break;
  case CK_IVAR:
    if (vsa && (vor = vsa->Cr_2_vor(cr)) != NULL)
      v = vor->Vsa_annot();
    break;
  case CK_OP:
    if (cr->Kid_count() == 1) {
      return Get_cr_annot(vsa, cr->Opnd(0));
    }
    else if (cr->Kid_count() == 2) {
      V_ANNOT lhs_v = Get_cr_annot(vsa, cr->Opnd(0));
      V_ANNOT rhs_v = Get_cr_annot(vsa, cr->Opnd(1));
      return VANT_UTIL::Or(lhs_v, rhs_v);
    }
    // TODO: more than 2 kids?
    break;
  default:
    Is_True(FALSE, ("bad cr kind"));
  }
  return v;
}
