//-*-c++-*-

// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company, 
// All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
//
// ====================================================================

#ifndef opt_alias_analysis_INCLUDED
#define opt_alias_analysis_INCLUDED

#include "opt_points_to_summary.h"

// ====================================================================
// 
// OPT_PU_POINTS_TO_SUMMARIZER 
// =============================
//
//   It does verything about points-to summary. 
//   
//  o. BOOL Bind_name (UNIFORM_NAME* uname, UNIFORM_NAME* bound_name, 
//                 WN* call_site);
//    The uniform name <uname> is from callee's perspective, 
//    Bind_name() try to bind that name with concrete/physical name 
//    visible from current lexical scope. 
//  
//    The <call_site> figures out place where binding happens. It will
//    used to bind actual argument with formal parameter.
// 
//    Upon return, <bound_name> is the bound name of <uname>. Iff
//    <bound_name> is not identical to <uname>, TRUE if returned. 
//
//  o. void Bind_callee_points_to_summary (void);
//    Try to bind callee's points-to summary with the name visible 
//    from current lexcical scope.
//
//  o. void Annotate_points_to_summary (void);
//    Annotate the points-to summary to the CHI nodes associated with 
//    the call-site. 
// 
//  o. void Summarize_points_to (void);
//     Summarize points-to of current PU.
//
// ====================================================================
//
class OPT_PU_POINTS_TO_SUMMARIZER {
private:

  class VER_ID_VISIT_CNT {
    ID_MAP<VER_ID, BOOL> _visited;
  public:
    VER_ID_VISIT_CNT(MEM_POOL* mp) :
      _visited(256, FALSE, mp, FALSE) {_visited.Init();}

    BOOL Visited (VER_ID ver) const { return _visited.Lookup (ver);}
    void Set_visited (VER_ID ver) { _visited.Insert (ver, TRUE); }
    void Reset_visited (VER_ID ver) {_visited.Insert (ver, FALSE); } 
  };

  OPT_STAB* _opt_stab;
  PU_POINTS_TO_SUMMARY* _cur_pt_sum; // points-to summary of current pu
  MEM_POOL* _mem_pool;
  WN_MAP   _callsite_2_pt_sum;  

  BOOL _tracing;

  void Summarize_points_to (VER_ID ver, 
             OPT_PU_POINTS_TO_SUMMARIZER::VER_ID_VISIT_CNT& visited, 
             PT_SET& pt_set);

  void Bind_callee_points_to_summary 
             (UNIFORM_NAME*, PT_SET_MGR*, PT_SET_MGR*);

  void Annotate_points_to_summary (UNIFORM_NAME*, PT_SET_MGR*, CHI_LIST*);

public:
   OPT_PU_POINTS_TO_SUMMARIZER (MEM_POOL* mp) : _mem_pool(mp) {
     _opt_stab = NULL;
     _cur_pt_sum = NULL;
     _tracing = Get_Trace (TP_WOPT2, PT_SUMMARY_FLAG);
     _callsite_2_pt_sum = WN_MAP_Create (mp);  
   }

   ~OPT_PU_POINTS_TO_SUMMARIZER (void) {
     WN_MAP_Delete (_callsite_2_pt_sum);
   }

   void Set_opt_stab (OPT_STAB* stab) { _opt_stab = stab; }

   PU_POINTS_TO_SUMMARY* Pu_summary (void) { return _cur_pt_sum; }
   void Set_pu_summary (PU_POINTS_TO_SUMMARY* sum) 
      { _cur_pt_sum = sum; }

   // Accessors 
   OPT_STAB* Opt_stab (void) const { return _opt_stab; }
   BOOL Tracing (void) const { return _tracing; }
   MEM_POOL* Mem_pool (void) const { return _mem_pool; }

   BOOL Bind_name (UNIFORM_NAME* uname, UNIFORM_NAME* bound_name, 
                   WN* call_site);

   void Bind_callee_points_to_summary (WN* entry_wn);

   void Annotate_points_to_summary (void);

   void Summarize_points_to (void);

   PU_POINTS_TO_SUMMARY* Get_bound_pt_sum (WN* callsite) {
     Is_True (WN_operator(callsite) == OPR_CALL, 
              ("WN is not a call"));
     return (PU_POINTS_TO_SUMMARY*)WN_MAP_Get (_callsite_2_pt_sum, callsite);
   }

   void Set_bound_pt_sum (WN* callsite, PU_POINTS_TO_SUMMARY* sum) {
     Is_True (WN_operator(callsite) == OPR_CALL, 
              ("WN is not a call"));
     return WN_MAP_Set (_callsite_2_pt_sum, callsite, sum);
   }
};

#endif // opt_alias_analysis_INCLUDED
