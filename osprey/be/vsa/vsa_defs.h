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

// ====================================================================
// vsa_defs.h
//
// common definitions for VSA
// ====================================================================
#ifndef vsa_defs_INCLUDED
#define vsa_defs_INCLUDED

#include "defs.h"
#include "tracing.h"

// Tracing flag for VSA, enable by -xtvsa:flag
#define VSA_PROP_DUMP_FLAG        0x00000001  /* dump flag for propagation */
#define VSA_PROP_TRACE_FLAG       0x00000002  /* trace flag for propagation */
#define VSA_HPROP_TRACE_FLAG      0x00000004  /* trace flag for heap_obj annotation propagation */
#define VSA_FPROP_TRACE_FLAG      0x00000008  /* trace flag for field annotation propagation */
#define VSA_DU_DUMP_FLAG          0x00000010  /* dump flag for D-U chain */
#define VSA_DU_TRACE_FLAG         0x00000020  /* trace flag for D-U chain */
#define VSA_DU_MGR_FLAG           0x00000040  /* trace flag for D0U manager */
#define VSA_CDA_DUMP_FLAG         0x00000080  /* dump flag for control dependency annotation */
#define VSA_CDA_TRACE_FLAG        0x00000100  /* trace flag for control dependency annotation */
#define VSA_VSYM_TRACE_FLAG       0x00000200  /* trace flag for vsym object phase */
#define VSA_VSYM_DUMP_FLAG        0x00000400  /* dump flag for vsym object phase */
#define VSA_REG_TRACE_FLAG        0x00000800  /* trace register function related */
#define VSA_VCG_DUMP_FLAG         0x00001000  /* dump IPSA CG before VCG phase */
#define VSA_VCG_TRACE_FLAG        0x00002000  /* trace IPSA CG VCG phase */
#define VSA_VARPROP_TRACE_FLAG    0x00004000  /* trace prop in vardef phase */
#define VSA_TAG_PROP_DUMP_FLAG    0x00008000  /* dump flag for tag propagation */
#define VSA_TAG_PROP_TRACE_FLAG   0x00010000  /* trace flag for tag propgation */
#define VSA_NHV_TRACE_FLAG        0x00800000  /* Trace New HO/VO creation/renaming */
#define VSA_NHV_DUMP_FLAG         0x01000000  /* Dump New HO/VO annotation with IR */
#define VSA_CHA_TRACE_FLAG        0x02000000  /* Dump class hierarchy analysis result */
#define VSA_CHA_DUMP_FLAG         0x04000000  /* Dump class hierarchy analysis result */

// Tracing flag for VSA Checker, enabled by -xtchk:flag or -xt100:flag
#define CHK_NPD_TRACE_FLAG        0x00000001  /* Trace NPD checker */
#define CHK_UIV_TRACE_FLAG        0x00000002  /* Trace UIV checker */
#define CHK_HEAP_TRACE_FLAG       0x00000004  /* Trace HEAP checker */
#define CHK_TAG_TRACE_FLAG        0x00000008  /* Trace TAG checker */
#define CHK_VAR_DEF_TRACE_FLAG    0x00000010  /* Trace VAR DEF checker */
#define CHK_ICALL_TRACE_FLAG      0x00000020  /* Trace ICALL devirutalize */
#define CHK_AOFB_TRACE_FLAG       0x00000040  /* Trace array out of fixed bounds */
#define CHK_VG_TRACE_FLAG         0x00010000  /* Trace value graph */

// VSA_STATS for statistic
#ifdef Is_True_On

// define statistic items
#define FOR_ALL_STAT_ITEM(V) \
  V(ho,           "heap obj")        \
  V(vo,           "vsym obj")        \
  V(hor,          "heap obj rep")    \
  V(vor,          "vsym obj rep")    \
  V(ho_mu,        "ho mu")           \
  V(ho_chi,       "ho chi")          \
  V(vo_mu,        "vo mu")           \
  V(vo_chi,       "vo chi")          \
  V(alias_vo_chi, "alias vo chi")    \
  V(ulist_vo_chi, "ulist vo chi")    \
  V(hva_round,    "hva round")       \
  V(hoa,          "heap_obj_annot")  \
  V(value_objs,   "value object")

struct VSA_STATS {
#define DECL_FIELD(id, desc) UINT _num_##id;
  FOR_ALL_STAT_ITEM(DECL_FIELD)
#undef DECL_FIELD

  void Print(FILE *fp = stderr) const;
  void Print_delta(const char* fname, FILE *fp = stderr) const;
}; // VSA_STATS

extern struct VSA_STATS _vsa_stats;

#define VSA_STATS_inc(x)            (++_vsa_stats._num_##x)
#define VSA_STATS_inc_n(x, n)       (_vsa_stats._num_##x += n)
#define VSA_STATS_Print(x)          (_vsa_stats.Print(x))
#define VSA_STATS_Print_delta(f, x) (_vsa_stats.Print_delta(f, x))

#else

#define VSA_STATS_inc(x)
#define VSA_STATS_inc_n(x, n)
#define VSA_STATS_Print(x)
#define VSA_STATS_Print_delta(f, x)

#endif /* Is_True_On */

#endif /* vsa_defs_INCLUDED */

