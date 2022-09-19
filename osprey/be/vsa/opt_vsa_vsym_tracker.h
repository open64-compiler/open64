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
// ATTENTION:
//   This file is from opt_vsa_checker.h
//   Old checker need this file to trace vsym U-D, once all old checkers
// are replaced, the content of this file need to be moved to opt_vsa_checker.h
//
// ====================================================================

#ifndef OPT_VSA_VSYM_TRACKER_H
#define OPT_VSA_VSYM_TRACKER_H

#include "opt_htable.h"
#include "opt_vsa_check_status.h"
#include "opt_vsa_rsc.h"

using namespace std;

class VSA;
class VSYM_OBJ_REP;
class VSYM_FLD_REP;

// ====================================================================
// VSYM_INFO
// VSYM infor for all indirect levels during traversal. For example:
//  a->b->c->d
//  Level 0: vor of x->d
//  Level 1: vor of x->c
//  Level 2: vor of x->b
//  Level 3: var of a
// ====================================================================
class VSYM_TRACKER {
public:
  typedef vector<VSYM_FLD_REP *> VSYM_VECTOR;

private:
  VSYM_VECTOR     _vsym_info;
  BOOL            _tracing;

public:
  VSYM_TRACKER() : _tracing(FALSE) { }

  BOOL             Tracing()       { return _tracing; }
  void             Set_tracing(BOOL trace)
                                   { _tracing = trace; }
  VSYM_FLD_REP    *Fld_rep() const { return _vsym_info.empty() ? NULL : Top(); }
  CHECKER_STATUS   Init(const VSA *vsa, CODEREP** cr, STMTREP* sr, MEM_POOL *mem_pool);
  void             Push(VSYM_FLD_REP* fld)
                                   { Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER: PUSH vfr "));
                                     Is_Trace_cmdn(Tracing(), fld->Print(TFile), TFile);
                                     _vsym_info.push_back(fld); }
  void             Pop()           { Is_True(_vsym_info.size() > 0, ("VSYM_TRACKER: invalid pop"));
                                     Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER: POP vfr "));
                                     Is_Trace_cmdn(Tracing(), _vsym_info.back()->Print(TFile), TFile);
                                     _vsym_info.pop_back(); }
  BOOL             Empty() const   { return _vsym_info.empty(); }
  INT              Size() const    { return _vsym_info.size(); }
  void             Save_stack(VSYM_VECTOR *vsym_info)
                                   { vsym_info->assign(_vsym_info.begin(), _vsym_info.end()); }
  void             Restore_stack(const VSYM_VECTOR *sav)
                                   { _vsym_info.assign(sav->begin(), sav->end()); }
  void             Clear()         { Is_Trace(Tracing(), (TFile, "  @VSYM_TRACKER: Clear"));
                                     _vsym_info.clear(); }
  VSYM_FLD_REP*    Top() const     { return _vsym_info.back(); }
  VSYM_OBJ_REP    *Compress_old(VSA *vsa, HEAP_OBJ_REP *hor, VSYM_FLD_REP *vfr,
                            VSYM_OBJ_REP *vor, STMTREP *stmt, CODEREP *cr);
  VSYM_OBJ_REP    *Compress(VSA *vsa, HEAP_OBJ_REP *hor, STMTREP *stmt, CODEREP *cr, BOOL mu, BOOL &maybe);
  BOOL             Expand(VSA *vsa, STMTREP *stmt, VSYM_OBJ_REP *vor, CODEREP *base_cr);
  VSYM_OBJ_REP    *Expand(VSA *vsa, STMTREP *stmt, HEAP_OBJ_REP *cur_hor, HEAP_OBJ_REP *base_hor, CODEREP *base_cr);
  void             Print(FILE *fp) const;
};
#endif
