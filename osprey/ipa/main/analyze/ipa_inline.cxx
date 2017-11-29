/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
// ====================================================================
// ====================================================================
//
// Module: ipa_inline.cxx
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_inline.cxx,v $
//
// Revision history:
//  16-Nov-96 - Original Version
//
// Description:
//
// Inlining analysis.
//
// ====================================================================
// ====================================================================

#ifdef __MINGW32__
#include <WINDOWS.h>
#endif /* __MINGW32__ */
#include <stdint.h>
#include "defs.h"
#include "tracing.h"			// trace flags
#include "errors.h"			// error handling

#include "ipa_option.h"			// ipa options

#include "ipa_inline.h"
#include "ipa_nested_pu.h"
#include "ipa_summary.h"
#include "ipc_symtab_merge.h"		// IPC_GLOBAL_IDX_MAP
#include "ir_reader.h"                  // fdump_tree

#include "symtab_access.h"

//^INLINING_TUNING
#define MINI_APPLICATION	10000
#define LARGE_APPLICATION	118000   //for GAP benchmark

#define MINI_HOTNESS_THRESHOLD		1
#define MEDIAN_HOTNESS_THRESHOLD	10
#define LARGE_HOTNESS_THRESHOLD		120

#define ESTIMATED_PART_INL_BB_CNT       2
#define ESTIMATED_PART_INL_STMT_CNT     5
//INLINING_TUNIN$
#define TINY_SIZE 10

INT Total_Prog_Size = 0;	// Size of the final program
INT Total_Inlined = 0;
INT Total_Not_Inlined = 0;
static UINT32 Max_Total_Prog_Size; // max. program size allowed
static INT Real_Orig_Prog_Weight; // Orig_Prog_Weight - dead code
static UINT32 non_aggr_callee_limit;
static UINT32 Real_Orig_WN_Count; // Orig_Prog_Weight - dead code

FILE *N_inlining = NULL;
FILE *Y_inlining = NULL;
FILE *e_weight = NULL;
FILE *Verbose_inlining = NULL;

#define BASETYPE TY_mtype

static OPCODE OPC_UNKNOWN = (OPCODE)0;

static
OPCODE Stid_Opcode [MTYPE_LAST + 1] = {
  OPC_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_I1STID,     /* MTYPE_I1 */
  OPC_I2STID,     /* MTYPE_I2 */
  OPC_I4STID,     /* MTYPE_I4 */
  OPC_I8STID,     /* MTYPE_I8 */
  OPC_U1STID,     /* MTYPE_U1 */
  OPC_U2STID,     /* MTYPE_U2 */
  OPC_U4STID,     /* MTYPE_U4 */
  OPC_U8STID,     /* MTYPE_U8 */
  OPC_F4STID,     /* MTYPE_F4 */
  OPC_F8STID,     /* MTYPE_F8 */
  OPC_F10STID,    /* MTYPE_F10 */
  OPC_F16STID,    /* MTYPE_F16 */
  OPC_UNKNOWN,    /* MTYPE_STR */
  OPC_FQSTID,     /* MTYPE_FQ */
  OPC_UNKNOWN,    /* MTYPE_M */
  OPC_C4STID,     /* MTYPE_C4 */
  OPC_C8STID,     /* MTYPE_C8 */
  OPC_CQSTID,     /* MTYPE_CQ */
  OPC_UNKNOWN     /* MTYPE_V */
#ifdef KEY
  ,OPC_BSSTID,    /* MTYPE_BS */
  OPC_A4STID,     /* MTYPE_A4 */
  OPC_A8STID,     /* MTYPE_A8 */
  OPC_C10STID,    /* MTYPE_C10 */
  OPC_UNKNOWN,    /* MTYPE_C16 */
  OPC_UNKNOWN,    /* MTYPE_I16 */
  OPC_UNKNOWN     /* MTYPE_U16 */
#ifdef TARG_X8664
  ,OPC_V16C4STID, /* MTYPE_V16C4 */
  OPC_V16C8STID,  /* MTYPE_V16C8 */
  OPC_V16I1STID,  /* MTYPE_V16I1 */
  OPC_V16I2STID,  /* MTYPE_V16I2 */
  OPC_V16I4STID,  /* MTYPE_V16I4 */
  OPC_V16I8STID,  /* MTYPE_V16I8 */
  OPC_V16F4STID,  /* MTYPE_V16F4 */
  OPC_V16F8STID,  /* MTYPE_V16F8 */
  OPC_V8I1STID,   /* MTYPE_V8I1 */
  OPC_V8I2STID,   /* MTYPE_V8I2 */
  OPC_V8I4STID,   /* MTYPE_V8I4 */
  OPC_V8I8STID,   /* MTYPE_V8I8 */
  OPC_V8F4STID,   /* MTYPE_V8F4 */
  OPC_M8I1STID,   /* MTYPE_M8I1 */
  OPC_M8I2STID,   /* MTYPE_M8I2 */
  OPC_M8I4STID,   /* MTYPE_M8I4 */
  OPC_M8F4STID,   /* MTYPE_M8F4 */
  OPC_V32C4STID,  /* MTYPE_V32C4 */
  OPC_V32C8STID,  /* MTYPE_V32C8 */
  OPC_V32I1STID,  /* MTYPE_V32I1 */
  OPC_V32I2STID,  /* MTYPE_V32I2 */
  OPC_V32I4STID,  /* MTYPE_V32I4 */
  OPC_V32I8STID,  /* MTYPE_V32I8 */
  OPC_V32F4STID,  /* MTYPE_V32F4 */
  OPC_V32F8STID,  /* MTYPE_V32F8 */
#endif // TARG_X8664
#endif // KEY
};

#ifdef TODO
// IPA Feedback - need inline/noinline identifiers
extern IPA_FEEDBACK_STRINGS * IPA_Fbk_Strings;
#endif

// ====================================================================
//
// Report_Reason / Report_Limit_Reason
//
// We're not inlining a call.  Report why.  For Report_Reason, the
// parameter reason is a simple string.  For Report_Limit_Reason, it
// is a printf format string for printing one (limit1) or two (limit1,
// limit2) integer limits.
//
// WARNING:  The DEMANGLE routine always returns the same static
// buffer, so the caller and callee names must not be live
// simultaneously.
//
// ====================================================================

void
Report_Reason (const IPA_NODE *callee, const IPA_NODE *caller,
	       const char *reason , const IPA_EDGE *edge)
{
  INT32 IPA_idx = 0;

  if ( ! ( INLINE_List_Actions || Trace_IPA || Trace_Perf ) ) return;

  char *callee_name = DEMANGLE (callee->Name());

  if ( INLINE_List_Actions ) {
    fprintf ( stderr, "%s not inlined into ", callee_name );
  }
  if ( Trace_IPA || Trace_Perf ) {
    fprintf ( TFile, "%s not inlined into ", callee_name );
  }

  char *caller_name = DEMANGLE (caller->Name());
  INT32 caller_index = caller->Node_Index();

  if ( INLINE_List_Actions ) {
    fprintf ( stderr, "%s", caller_name );
    if ( IPA_Skip_Report ) {
      fprintf ( stderr, " (%d)", caller_index );
    }
    fprintf ( stderr, ": %s    (edge# %d) \n", reason, edge->Edge_Index ()  );
  }
  if ( Trace_IPA || Trace_Perf ) {
    fprintf ( TFile, "%s", caller_name );
    if ( IPA_Skip_Report ) {
      fprintf ( TFile, " (%d)", caller_index );
    }
    fprintf ( TFile, ": %s    (edge# %d) \n", reason, edge->Edge_Index ()  );
    fflush ( TFile );
  }
if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING))
{
  	
 	fprintf(N_inlining, "Report_Reason:[%s] not inlined into [%s]  (edge# %d):\n", callee_name, caller_name,edge->Edge_Index () ); 
	fprintf ( N_inlining, "\t {reason: %s}\n", reason  );
}
#ifdef TODO
#ifndef _STANDALONE_INLINER
  if( IPA_Enable_Feedback ) {
    /* need pragma plus the reason */
    IPA_idx = IPA_Fbk_Strings->Emit_id_string( callee_name);
    if ( IPA_idx > 0 ) {
      fprintf(IPA_Feedback_prg_fd, 
              "#pragma noinline %s  /* why: %s */\n", callee_name, reason );
    }
  } /* if (IPA_Enable_Feedback) */
#endif  // _STANDALONE_INLINER
#endif // TODO
}

void
Report_Limit_Reason (const IPA_NODE *callee, const IPA_NODE *caller,
		     const IPA_EDGE *edge,
		     const char *reason, float limit1, float limit2)
{
  INT32 IPA_idx = 0;
  if ( ! ( INLINE_List_Actions || Trace_IPA || Trace_Perf ) ) return;

  char *callee_name = DEMANGLE (callee->Name());

  if ( INLINE_List_Actions ) {
    fprintf ( stderr, "%s not inlined into ", callee_name );
  }
  if ( Trace_IPA || Trace_Perf ) {
    fprintf ( TFile, "%s not inlined into ", callee_name );
  }
  char *caller_name = DEMANGLE (caller->Name());
  INT32 caller_index = caller->Node_Index();

  if ( INLINE_List_Actions ) {
    fprintf ( stderr, "%s", caller_name );
    if ( IPA_Skip_Report ) {
      fprintf ( stderr, " (%d): ", caller_index );
    } else {
      fprintf ( stderr, ": " );
    }
    fprintf ( stderr, reason, limit1, limit2 );
    fprintf ( stderr, "   (edge# %d)\n" , edge->Edge_Index () );
  }
  if ( Trace_IPA || Trace_Perf ) {
    fprintf ( TFile, "%s", caller_name );
    if ( IPA_Skip_Report ) {
      fprintf ( TFile, " (%d): ", caller_index );
    } else {
      fprintf ( TFile, ": " );
    }
    fprintf ( TFile, reason, limit1, limit2 );
    fprintf ( TFile, "   (edge# %d)\n", edge->Edge_Index ()  );
    fflush ( TFile );
  }
if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING))
{
	fprintf ( N_inlining, "Report_Limit_Reason: [%s] not inlIned into [%s]  (edge# %d):\n", callee_name, caller_name, edge->Edge_Index () );
	fprintf(N_inlining, "\t{reason: " );
	fprintf ( N_inlining, reason, limit1, limit2 );
	fprintf(N_inlining, "}\n");
}
#ifdef TODO
#ifndef _STANDALONE_INLINER
  if( IPA_Enable_Feedback ) {
    /* need pragma plus the reason */
    IPA_idx = IPA_Fbk_Strings->Emit_id_string( callee_name);
    if ( IPA_idx > 0 ) {
      fprintf(IPA_Feedback_prg_fd, 
              "#pragma noinline %s  /* why: ", callee_name );
      fprintf(IPA_Feedback_prg_fd, reason, limit1, limit2 );
      fprintf(IPA_Feedback_prg_fd, " */\n"); 

    }
  } /* if( IPA_Enable_Feedback ) */
#endif  // _STANDALONE_INLINER
#endif // TODO
}

/* the combined weight after inlining two procedures */
/* we need to subtract the bb count and call count by 1 to reflect the
   removal of a call.  We add the number of copy stmt created for copying
   the formal parameters */
UINT32
Get_combined_weight (PU_SIZE s1, PU_SIZE s2, IPA_NODE *callee)
{
    if (callee->Has_Varargs()) {
       /* If callee is a varargs, we will only do partial inlining
          and hence we estimate a much smaller size increase.
          TODO:
          We want to adjust the size estimate for all partially
          inlined functions. But since the partial inlining candidates
          and paths are not determined until the IPO phase, this
          adjustment is not easy here. Once we move the partial
          inlining candidate selection to the IPL phase and the decision
          to the ipa_inline phase, we can solve this size adjustment
          problem.
        */
       s1.Inc_PU_Size (ESTIMATED_PART_INL_BB_CNT,
                       ESTIMATED_PART_INL_STMT_CNT, 0);
       return s1.Weight ();
    }

    s1 += s2;
    /* 1 less bb and 1 less callfrom removing the call, add copy stmt for
       formals */ 
#ifdef KEY
    s1.Inc_PU_Size (-1, 0, -1);
#else
    s1.Inc_PU_Size (-1, callee->Num_Formals(), -1);
#endif
    return s1.Weight ();
}

UINT32
Get_combined_olimit (PU_SIZE s1, PU_SIZE s2, IPA_NODE *callee)
{
    if (callee->Has_Varargs()) {
       /* If callee is a varargs, we will only do partial inlining
          and hence we estimate a much smaller size increase.
          TODO:
          We want to adjust the size estimate for all partially
          inlined functions. But since the partial inlining candidates
          and paths are not determined until the IPO phase, this
          adjustment is not easy here. Once we move the partial
          inlining candidate selection to the IPL phase and the decision
          to the ipa_inline phase, we can solve this size adjustment
          problem.
        */
       s1.Inc_PU_Size (ESTIMATED_PART_INL_BB_CNT,
                       ESTIMATED_PART_INL_STMT_CNT, 0);
       return s1.Weight ();
    }

    s1 += s2;
    /* 1 less bb and 1 less callfrom removing the call, add copy stmt for
       formals */ 
#ifdef KEY
    s1.Inc_PU_Size (-1, 0, -1);
#else
    s1.Inc_PU_Size (-1, callee->Num_Formals(), -1);
#endif
    return s1.Olimit ();
}



#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

typedef AUX_IPA_NODE<UINT32> INLINE_COUNTER_ARRAY;

// For each PU, keep track of the number of calls (to this PU) that are
// inlined.  If all calls to this PU are inlined, the PU might be deleted.
static INLINE_COUNTER_ARRAY* inline_count;

static inline BOOL
All_Calls_Inlined (const IPA_NODE* node, const IPA_CALL_GRAPH* cg)
{
    Is_True (IPA_Enable_DFE, ("Expecting -IPA:dfe=on"));

    return cg->Num_In_Edges (node) == (*inline_count)[node];
}

#endif // (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

static void
Init_inline_parameters (void)
{
    UINT64 bloat_size;
    UINT64 current_size = Real_Orig_Prog_Weight =
	MIN (Total_Prog_Size, Orig_Prog_Weight);

	Real_Orig_WN_Count = Orig_Prog_WN_Count; //INLINING_TUNING

#ifdef KEY
    if (INLINE_Ignore_Bloat && ! IPA_Bloat_Factor_Set)
      IPA_Bloat_Factor = UINT32_MAX;
#endif // KEY

    bloat_size = current_size * (UINT64) IPA_Bloat_Factor;

    if (bloat_size > UINT32_MAX || IPA_Bloat_Factor == UINT32_MAX)
	Max_Total_Prog_Size = UINT32_MAX; // possible mult overflow
    else
	Max_Total_Prog_Size = current_size + bloat_size / 100;

    non_aggr_callee_limit = IPA_PU_Minimum_Size + (IPA_PU_Minimum_Size / 2);

    //Adaptive inlining here, pengzhao
    if( IPA_Min_Hotness == 10) //DEFAULT_MIN_HOTNESS
    {
      if(Real_Orig_Prog_Weight< MINI_APPLICATION)
      {
	    IPA_Min_Hotness = MINI_HOTNESS_THRESHOLD;	
      }else if(Real_Orig_Prog_Weight< LARGE_APPLICATION)
      {
	    IPA_Min_Hotness = MEDIAN_HOTNESS_THRESHOLD;	
      }else
      {
	    IPA_Min_Hotness = LARGE_HOTNESS_THRESHOLD;	
      }
    }

    if (Total_cycle_count.Known() && (Trace_IPA || Trace_Perf|| Get_Trace(TP_IPA, IPA_TRACE_TUNING_NEW))) {
	fprintf (TFile, "\tTotal number of calls = ");
        Total_call_freq.Print(TFile);
	fprintf (TFile, "\n\tTotal cycle count = ");
        Total_cycle_count.Print(TFile);
	fprintf (TFile, "\n");
// INLINING_TUNING^
	fprintf(TFile, "Total cycle_count_2 = "); 
		Total_cycle_count_2.Print(TFile); 
	fprintf(TFile, "\nTotal WN_count = %d",Orig_Prog_WN_Count); 
	fprintf (TFile, "\n");
//INLINING_TUNING$
	fprintf(TFile, "\t Real_Orig_Prog_Weight=%d\n",Real_Orig_Prog_Weight);
	fprintf(TFile, "\t current_size=%lld\n",current_size);
	fprintf(TFile, "\t Max_Total_Prog_Size=%d\n",Max_Total_Prog_Size);
    }

    if (Trace_IPA || Get_Trace(TP_IPA, IPA_TRACE_TUNING_NEW)) {
	fprintf(TFile, "Bloat factor = %u%% \n",  IPA_Bloat_Factor);
	fprintf(TFile, "PU Limit = %u \n", IPA_PU_Limit);
	fprintf(TFile, "PU Hard Limit = %u\n", IPA_PU_Hard_Limit);
	fprintf(TFile, "Depth Level = %u \n", IPA_Max_Depth);
	if (IPA_Bloat_Factor_Set)
	    fprintf(TFile, "Bloat Factor set = TRUE \n");
	else
	    fprintf(TFile, "Bloat Factor set = FALSE \n");

	if (IPA_PU_Limit_Set)
	    fprintf(TFile, "PU Limit Set = TRUE \n");
	else
	    fprintf(TFile, "PU Limit Set = FALSE \n");

	fprintf(TFile, "IPA_PU_Minimum_Size = %d\n",IPA_PU_Minimum_Size );
	fprintf(TFile, "non_aggr_callee_limit = %d\n",non_aggr_callee_limit );
	fprintf(TFile, "IPA_Min_Hotness = %d\n",IPA_Min_Hotness );
    }

} // Init_inline_parameters


#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
static void
Update_Total_Prog_Size (const IPA_NODE *caller, IPA_NODE *callee,
			const IPA_CALL_GRAPH *cg)
{
    ++((*inline_count)[callee]);

    if ((IPA_Enable_Cloning && caller->Is_Clone_Candidate()) ||
        callee->Has_Varargs()) {
        /* If callee has var args, this is indempotent partial
           inlining and the callee will be kept.
         */
	callee->Set_Undeletable ();
	return;
    }
    if (
#ifdef KEY
	OPT_Cyg_Instrument == 0 &&  // Bug 750; TODO: Relax restriction
#endif
	! callee->Is_Undeletable () &&
	! callee->Should_Be_Skipped() &&
	All_Calls_Inlined (callee, cg) &&
	! callee->Is_Externally_Callable ()) {

	callee->Set_Deletable ();
	Total_Prog_Size -= callee->Weight();
    }
} // Update_Total_Prog_Size
#endif // (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))


float
compute_hotness (IPA_EDGE *edge, IPA_NODE *callee, INT callee_size)
{
    FB_FREQ cycle_ratio = (edge->Get_frequency () / callee->Get_frequency () *
                           callee->Get_cycle_count ()) / Total_cycle_count;
    float cycle_ratio_float = cycle_ratio.Value();
    float size_ratio = (float) callee_size / (float) Real_Orig_Prog_Weight;
    float result_float = (cycle_ratio_float / size_ratio * 100.0);
    return (result_float);
}

UINT32 // KEY
Effective_weight (const IPA_NODE* node)  {
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (IPA_Use_Effective_Size && node->Has_frequency ()) {
	SUMMARY_FEEDBACK *fb = node->Get_feedback ();
	return PU_Weight (fb->Get_effective_bb_count (),
			  fb->Get_effective_stmt_count (),
			  node->PU_Size().Call_Count ());
    } else if (node->Has_Varargs()) { 
        /* If callee is a varargs, we will only do partial inlining
           and hence we estimate a much smaller size increase.
           TODO:
           We want to adjust the size estimate for all partially
           inlined functions. But since the partial inlining candidates
           and paths are not determined until the IPO phase, this
           adjustment is not easy here. Once we move the partial 
           inlining candidate selection to the IPL phase and the decision
           to the ipa_inline phase, we can solve this size adjustment
           problem.
         */
        return PU_Weight(ESTIMATED_PART_INL_BB_CNT, 
                         ESTIMATED_PART_INL_STMT_CNT, 0);
    } else
#endif // _STANDALONE_INLINER
	return node->Weight ();
}

#ifdef KEY
void inline_do_it (IPA_EDGE * ed, IPA_NODE * caller, IPA_NODE * callee,
                   const IPA_CALL_GRAPH * cg)
{
    UINT32 caller_weight = caller->Weight();
    UINT32 combined_weight = Get_combined_weight (caller->PU_Size(),
                                                  callee->PU_Size(),
						  callee);
    if (Trace_IPA || Trace_Perf) {
        UINT32 callee_weight = Effective_weight (callee);
	fprintf (stderr, "%s inlined into %s\n", callee->Name(),caller->Name());
	fprintf (TFile, "%s inlined into ", DEMANGLE (callee->Name()));
	fprintf (TFile, "%s (size: %d + %d = %d)   (edge# %d) \n", DEMANGLE (caller->Name()), callee_weight, caller_weight, combined_weight, ed->Edge_Index());
    }
    
#ifdef TODO
    if( IPA_Enable_Feedback ) {
	/* check for cross-file inlining */
	if( callee->File_Index() !=  caller->File_Index()) {
	    (callee->Get_fbk_ptr() )->Set_Cross_File_Fnd();
	}
    }
#endif
    
    Total_Prog_Size += (combined_weight - caller_weight);
    caller->UpdateSize (callee, ed);
    
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (IPA_Enable_DFE)
	Update_Total_Prog_Size (caller, callee, cg);
#endif // _STANDALONE_INLINER
    
    if (callee->Summary_Proc()->Has_var_dim_array()) {  // propagate the bit up
	caller->Summary_Proc()->Set_has_var_dim_array();
    }
}

// Return true if a callee (actually any callee) can be inlined into
// "node" without checking for additional constraints.
static BOOL
trivially_ok_to_inline (const IPA_NODE * node, const IPA_CALL_GRAPH * cg)
{
  Is_True (node->Summary_Proc()->Get_bb_count() > 0,
           ("How can there be a call without any BB?"));

  // An approximate estimate to decide if the only thing "node" does is
  // call another function -- a popular category of functions in C++.
  return node->Summary_Proc()->Get_callsite_count() == 1 &&
         cg->Num_Out_Edges (node) == 1 &&
         node->Summary_Proc()->Get_bb_count() == 1;
}
#endif // KEY

#include<string>
#include<sstream>

static BOOL
check_size_and_freq (IPA_EDGE *ed, IPA_NODE *caller,
		     IPA_NODE *callee, const IPA_CALL_GRAPH *cg)
{
    BOOL inline_it = FALSE;
    using namespace std;
    std::string reason_st, tmp_decision_st, tmp_reason_st;
    reason_st = "{reason: ";
    INT32 IPA_idx = 0;
    UINT32 caller_weight = caller->Weight ();
    UINT32 callee_weight = Effective_weight (callee);
    UINT32 combined_weight = Get_combined_weight (caller->PU_Size(), callee->PU_Size(), callee);

    if (PU_is_operator(callee->Get_PU()) && ed->Num_Actuals() <= 2)
      combined_weight = 1;

#ifdef KEY
    float hotness = callee->Get_feedback() == NULL ? 0.0 :
      compute_hotness (ed, callee, callee_weight);       
#else
    float  hotness = compute_hotness (ed, callee, callee_weight); 
#endif

    //pengzhao
    if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
	
	SUMMARY_FEEDBACK *fb = callee->Get_feedback();
	INT e_bb_cnt, e_stmt_cnt;
	e_bb_cnt= e_stmt_cnt = -1;
	
	if(callee->Has_frequency ()) {
	    e_bb_cnt = (fb==NULL)? unsigned(-1) : fb->Get_effective_bb_count ();
	    e_stmt_cnt = (fb==NULL)? unsigned(-1) : fb->Get_effective_stmt_count ();
	}
	fprintf(e_weight, "%-8d%-8d%-8d%-8d%-8d%-8d%-8d%s \n", e_bb_cnt, e_stmt_cnt, callee->PU_Size().Call_Count (),callee_weight, callee->PU_Size().Bb_count(), callee->PU_Size().Stmt_count(),callee->Weight() , callee->Name() );
    }

     if ( caller->Summary_Proc()->Is_Never_Invoked() == FALSE && 
         callee->Summary_Proc()->Is_Never_Invoked() == FALSE) {

        if (callee_weight <= TINY_SIZE
#ifdef KEY
	    || (callee_weight < INLINE_Callee_Limit && PU_is_marked_inline(callee->Get_PU()))
	    || trivially_ok_to_inline (caller, cg) // ok to inline into caller?
#endif
	   )
	{
#ifdef KEY
	    // don't use goto
	    inline_do_it(ed, caller, callee, cg);
	    return TRUE;
#else
            goto inline_do_it;
#endif
        }             
     } 
    
    if (IPA_Force_Depth_Set) {
	if (!callee->Has_Noinline_Attrib() && cg->Node_Depth(callee) <= IPA_Force_Depth
#ifdef TODO
	&& !ed->IsICall()
#endif
	) {
	    if ( Trace_IPA || Trace_Perf ) {
		fprintf ( TFile, "%s inlined into ", DEMANGLE (callee->Name()) );
		fprintf (TFile, "%s:  because of force depth = (%d)\n", DEMANGLE (caller->Name()), IPA_Force_Depth);
	    }
	    
	    //pengzhao
	    if (Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
		inline_it = TRUE;
                ostringstream ed_ind;
                ed_ind << ed->Edge_Index();
                tmp_decision_st = string ("*[") + string(DEMANGLE(callee->Name())) + string("]") +
                                  string("will be Inlined into [") + string (DEMANGLE(caller->Name())) + string("] (edge=") +
                ed_ind.str() + string(")");
                ostringstream fd_;
                fd_ << IPA_Force_Depth;
                tmp_reason_st = string(" because of force depth = (") + fd_.str() + string (")");
                reason_st = reason_st + tmp_reason_st;
	    }
	    
	    if ( INLINE_List_Actions ) {
                fprintf ( stderr, "%s (size %d) inlined into ", DEMANGLE (callee->Name()), callee_weight );
                fprintf ( stderr, "%s (combined size %d): because of force depth =  (%d)\n", DEMANGLE (caller->Name()), combined_weight, IPA_Force_Depth );
		Total_Prog_Size += (combined_weight - caller_weight);
		caller->UpdateSize (callee, ed);
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
		if (IPA_Enable_DFE)
		    Update_Total_Prog_Size (caller, callee, cg);
#endif // _STANDALONE_INLINER
	    }
	    ed->Set_Must_Inline_Attrib();
	    
	    if (callee->Summary_Proc()->Has_var_dim_array()) {  // propagate the bit up
		caller->Summary_Proc()->Set_has_var_dim_array();
	    }
	    return TRUE;
	}
    }
    
    // We must inline (regardless of size considerations) if edge is
    // set inline (by a pragma) or if callee is set inline (by being on a
    // must list, or by default)
    if (!ed->Has_Must_Inline_Attrib() && !callee->Has_Must_Inline_Attrib() && !INLINE_All
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
	// callee has __inline set
	&& (!callee->Summary_Proc()->Is_must_inline())
#endif // _STANDALONE_INLINER
    ) {
	
	if (Total_Prog_Size >= Max_Total_Prog_Size) {
#ifndef KEY
	    static BOOL reported = FALSE;
	    
	    if ( ! reported ) {
#endif // !KEY
		if ( Trace_IPA || Trace_Perf ) {
		    fprintf ( TFile, "Inlining stopped because total " "program size limit exceeded\n" );
		}
#ifdef KEY
                Report_Reason (callee, caller, 
		               "Total program size limit exceeded", ed);
                if ( INLINE_List_Actions ) {
                    fprintf ( stderr, "Total program size (%d) exceeds the limit (%d)\n", Total_Prog_Size, Max_Total_Prog_Size);
                }
#else
		if ( INLINE_List_Actions ) {
		    fprintf ( stderr, "Inlining stopped because total " "program size limit exceeded\n" );
		}
		reported = TRUE;
	    }
#endif
	    return FALSE;
	}// if (Total_Prog_Size >= Max_Total_Prog_Size) 
	
	INT loopnest = ed->Summary_Callsite ()->Get_loopnest ();
	
	if (Trace_IPA) {
	    fprintf (TFile, "\tcaller: %s (%u) loopnest = %u", DEMANGLE (caller->Name()), caller_weight, loopnest); 
	    fprintf (TFile, ", callee: %s ", DEMANGLE (callee->Name()));
	    if (callee->PU_Size().Call_Count () == 0)
		fprintf (TFile, "(leaf) ");
	    fprintf (TFile, "(%u)\n", callee_weight);
	}
	
	if (ed->Has_frequency ()) {
#ifdef KEY
	    if(ed->Get_frequency ().Value() == 0.0f)
#else
	    if(ed->Get_frequency ()._value == 0.0f)
#endif
	    {
		ed->Set_reason_id(32);
		Report_Reason (callee, caller, "Edge is never invoked", ed);
		return FALSE;
	    }
	}
	
	if ( caller->Summary_Proc()->Is_Never_Invoked() ) {// there is no fdbk info for this edge
	    ed->Set_reason_id(32);
	    Report_Reason (callee, caller, "Edge is never invoked", ed);
	    return FALSE;
	}
	
	if ( callee->Summary_Proc()->Is_Never_Invoked() ) {// there is no fdbk info for this edge
	    ed->Set_reason_id(32);
	    Report_Reason (callee, caller, "Edge is never invoked", ed);
	    return FALSE;
	}

	if (callee_weight >= non_aggr_callee_limit && 
	    hotness < (float)IPA_Min_Hotness &&
	    ed->Summary_Callsite ()->Is_in_case_clause ()) {
	    ed->Set_reason_id(34);
	    Report_Reason (callee, caller, "Infrequent callee in switch statements", ed);
	    return FALSE;
	}

        if (callee_weight > IPA_PU_Minimum_Size) {
            if (combined_weight > IPA_PU_Limit) {
		ed->Set_reason_id(26);
		ed->Set_reason_data((float)combined_weight);
		Report_Limit_Reason (callee, caller, ed, "combined size(%f) exceeds -IPA:plimit=%f", 
	                             combined_weight,IPA_PU_Limit);
		return FALSE;
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
	    } 
	    
	    if (ed->Has_frequency () && callee->Has_frequency () && ed->Get_frequency().Known() && callee->Get_frequency ().Known()) {
		float min_hotness = (float)IPA_Min_Hotness;
		
		//following codes deal with the PUs that are invoked 
		//infrequently but contain very hot codes (i.e. loops etc)
		float density = (float) callee->Get_cycle_count().Value() / ((float)callee_weight * (float)callee->Get_frequency().Value());
		
		// if there is only one callsite, we will inline it anyway
		if ( cg->Num_In_Edges(callee) > 1) {
		    if(hotness < min_hotness) {
                        ed->Set_reason_id(27);
                        ed->Set_reason_data(hotness);
                        Report_Limit_Reason (callee, caller, ed, "hotness (%f) < -IPA:min_hotness (%.1f)", hotness, min_hotness);

                        // Add the vector registration here
                        // if we have more budget, we will lower the hotness threshhold
                        return FALSE;
                    }
                    if(density > IPA_Max_Density) {
                        ed->Set_reason_id(33);
                        ed->Set_reason_data(density);
			// KEY: use %f for IPA_Max_Density
                        Report_Limit_Reason (callee, caller, ed, "Density (%f) > Max_density (%f)", density, (float)IPA_Max_Density);
                        return FALSE;
		    }
		}else{
		    if(hotness < (min_hotness/2)) {
			ed->Set_reason_id(27);
			ed->Set_reason_data(hotness);
			Report_Limit_Reason (callee, caller, ed, "hotness (%f) < -IPA:min_hotness (%.1f)", hotness, min_hotness);
			return FALSE;
		    }
		}
	    } else if ( callee->Summary_Proc()->Is_Never_Invoked() ) { // there is no fdbk info for this edge
		ed->Set_reason_id(32);
		Report_Reason (callee, caller, "Edge is never invoked", ed);
		return FALSE;
#endif // _STANDALONE_INLINER
	    }else{ // 1.
		if (callee_weight > IPA_Small_Callee_Limit && cg->Num_In_Edges(callee) > 1) {
                    /* We try to screen out callees that are too large, but
                     * accept those that have only one caller:
                     */
                    if (loopnest == 0 || callee->PU_Size().Call_Count () > 0) {
                        ed->Set_reason_id(28);
                        ed->Set_reason_data((float)callee_weight);
                        Report_Limit_Reason (callee, caller, ed, "callee size" " (%f) > -IPA:callee_limit=%f", callee_weight, IPA_Small_Callee_Limit ); 
                        return FALSE;
                    }
                }

		if (!INLINE_Aggressive && loopnest == 0 && 
                    callee->PU_Size().Call_Count() > 0   && 
                    callee_weight > non_aggr_callee_limit) {
                    /* Less aggressive inlining: don't inline unless it is
                     * either small, leaf, or called from a loop. 
                     */
                    ed->Set_reason_id(29);
                    ed->Set_reason_data((float)callee_weight);
                    Report_Limit_Reason (callee, caller, ed, "callee_size (%.1f) > -INLINE:aggressive=off callee limit (%.1f)", callee_weight, non_aggr_callee_limit );
                    return FALSE;
                }
	    }// 1.
	} else {
	    if (combined_weight > IPA_PU_Hard_Limit) {
		ed->Set_reason_id(30);
		ed->Set_reason_data((float)combined_weight);
		Report_Limit_Reason ( callee, caller, ed, "small, but size (%f) " "exceeds hard function size limit (%f)", combined_weight, IPA_PU_Hard_Limit );
		return FALSE;
	    } else {
		// inlining since pu size is less than minimum pu size
		
#ifdef TODO
		if (ed->IsICall () && (IPA_Enable_Cloning && callee->Is_Clone_Candidate()))
		// until cprop handles indirect call, we can never
		// inline a cloned PU
		    callee->Clear_Clone_Candidate ();
#endif
		
		if ( Trace_IPA || Trace_Perf ) {
		    fprintf ( TFile, "%s inlined into ", DEMANGLE (callee->Name()) );
		    fprintf (TFile, "%s: forced because of small size (%d)  (edge# %d)\n", DEMANGLE (caller->Name()), callee_weight, ed->Edge_Index() );
		}
		
		//pengzhao
		if (Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
		    inline_it = TRUE;
                    ostringstream ed_ind;
                    ed_ind << ed->Edge_Index();
                    tmp_decision_st = string ("*[") + string(DEMANGLE(callee->Name())) + string("]") +
                                      string("will be Inlined into [") + string (DEMANGLE(caller->Name())) + string("] (edge=") +
                    ed_ind.str() + string(")");
                    ostringstream cw;
                    cw << callee_weight;
                    tmp_reason_st = string(" forced because of small size (") + cw.str() + string (")");
                    reason_st = reason_st + tmp_reason_st;
		}
		
		if ( INLINE_List_Actions ) {
                    fprintf ( stderr, "%s (size %d) inlined into ", DEMANGLE (callee->Name()), callee_weight );
                    fprintf ( stderr, "%s (combined size %d): forced because of small size (%d)  (edge# %d)\n", 
                              DEMANGLE (caller->Name()), combined_weight, callee_weight, ed->Edge_Index() );
		}
	    }
	}
	
	if (Get_combined_olimit (caller->PU_Size(), callee->PU_Size(), callee) > Olimit) {
	    ed->Set_reason_id(31);
	    ed->Set_reason_data((float)Get_combined_olimit (caller->PU_Size(), callee->PU_Size(), callee));
	    Report_Limit_Reason (callee, caller, ed, "Olimit (%f) exceeds -OPT:Olimit=%f", Get_combined_olimit (caller->PU_Size(), callee->PU_Size(), callee), Olimit );
	    return FALSE;
	}

#ifdef KEY
	if (IPA_Enable_Branch_Heuristic)
	{
	    // ********** REMOVE THIS ************* 
	    if (getenv ("branch"))
	    	IPA_Min_Branch_Prob = atof (getenv("branch"));
	    float branch_p = ed->Summary_Callsite()->Get_probability();
	    if (branch_p >= 0 && branch_p < IPA_Min_Branch_Prob)
	    {
	    	fprintf (stderr, "%s not inlined into %s because branch probability %f < minimum probability %f\n", callee->Name(), caller->Name(), branch_p, IPA_Min_Branch_Prob);
		return FALSE;
	    }
	}
#endif
    }//
    
#ifdef TODO
    if (ed->IsICall () && (IPA_Enable_Cloning && callee->Is_Clone_Candidate())) {
	// if we decide to clone, we cannot inline
	Report_Reason (callee, caller, "cannot inline indirect call to cloned PU", ed);
	return FALSE;
    }
#endif
    
    /* Finally, we decide to inline this call */
#ifdef KEY
    inline_do_it (ed, caller, callee, cg);
#else
    inline_do_it: if (Trace_IPA || Trace_Perf) {
	fprintf (stderr, "%s inlined into %s\n", callee->Name(),caller->Name());
	fprintf (TFile, "%s inlined into ", DEMANGLE (callee->Name()));
	fprintf (TFile, "%s (size: %d + %d = %d)   (edge# %d) \n",
                 DEMANGLE (caller->Name()), callee_weight, caller_weight, combined_weight, ed->Edge_Index());
    }
    
    //pengzhao
    if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
	if (inline_it==FALSE) {
            ostringstream ed_ind;
            ed_ind << ed->Edge_Index();
            tmp_decision_st = string ("*[") + string(DEMANGLE(callee->Name())) + string("]") +
                              string("will be Inlined into [") + string (DEMANGLE(caller->Name())) + string("] (edge=") +
                              ed_ind.str() + string(")");
        }
        ostringstream alis;
        alis << callee_weight << "+" << caller_weight << "=" << combined_weight << ")\n";
        tmp_reason_st = string("  and the limits donot filter it out},(size") + alis.str();
        reason_st = reason_st + tmp_reason_st;
	fprintf(Y_inlining, tmp_decision_st.c_str());
	fprintf(Y_inlining, reason_st.c_str());
    }
    
#ifdef TODO
    if( IPA_Enable_Feedback ) {
	/* check for cross-file inlining */
	if( callee->File_Index() !=  caller->File_Index()) {
	    (callee->Get_fbk_ptr() )->Set_Cross_File_Fnd();
	}
    }
#endif
    
    Total_Prog_Size += (combined_weight - caller_weight);
    caller->UpdateSize (callee, ed);
    
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (IPA_Enable_DFE)
	Update_Total_Prog_Size (caller, callee, cg);
#endif // _STANDALONE_INLINER
    
    if (callee->Summary_Proc()->Has_var_dim_array()) {  // propagate the bit up
	caller->Summary_Proc()->Set_has_var_dim_array();
    }
#endif // KEY
    return TRUE;
} // check-size-and-freq

//--------------------------------------------------------------------------
// check if return types are okay
//--------------------------------------------------------------------------
static BOOL
return_types_are_compatible (IPA_NODE* callee_node, IPA_EDGE *ed)
{
    if (ed->Summary_Callsite()->Get_return_type() == MTYPE_V)
        return TRUE;                    // caller ignoring the return type

    ST* callee = callee_node->Func_ST();

    Is_True (ST_sym_class (callee) == CLASS_FUNC,
             ("Expecting a function ST"));

    TY_IDX ty_idx = ST_pu_type (callee);
    TY& ty = Ty_Table[ty_idx];

    Is_True (TY_kind (ty) == KIND_FUNCTION, ("Expecting a function ST")
);

    TY_IDX ret_ty_idx = TYLIST_type (Tylist_Table[TY_tylist (ty)]);

    if (ret_ty_idx == 0)
        return FALSE;

    TY& ret_ty = Ty_Table[ret_ty_idx];

    if (TY_kind (ret_ty) == KIND_VOID)
        return FALSE;
    else {
        TYPE_ID callee_mtype = BASETYPE(ret_ty);
        TYPE_ID caller_mtype = ed->Summary_Callsite()->Get_return_type();

        // check if types are the same or the return type is
        // a structure return

        if ((callee_mtype == caller_mtype) || (callee_mtype == MTYPE_M)
)
            return TRUE;
    }

    return FALSE;
} // return_types_are_compatible

static TY_IDX
base_type_of_array(TY_IDX array_type)
{
    if (TY_kind(TY_AR_etype(array_type)) != KIND_ARRAY)
	return (TY_AR_etype(array_type));
    else
	return (base_type_of_array(TY_AR_etype(array_type)));
}

//--------------------------------------------------------------------------
// check if the types are compatible. If they are then we are fine,
// else we don't inline
//--------------------------------------------------------------------------
static BOOL
types_are_compatible (TY_IDX ty_actual, TY_IDX ty_formal, BOOL lang)
{

    // if it is not a value parameter then check to see
    // if it is of type KIND_SCALAR, if true then return
    // FALSE

    TYPE_ID formal_element_size, actual_element_size;

    if ((ty_actual == 0) || (ty_formal == 0))
	return FALSE;		// No type info

    if (lang) {
        BOOL formal_is_array, actual_is_array;
    
        if (TY_kind(ty_formal) == KIND_POINTER) {
    	    ty_formal = TY_pointed(ty_formal);
    	    formal_is_array = (TY_kind(ty_formal) == KIND_ARRAY);
	}
        else
    	    formal_is_array = (TY_kind(ty_formal) == KIND_ARRAY);
    
        if (TY_kind(ty_actual) == KIND_POINTER) {
    	    ty_actual = TY_pointed(ty_actual);
    	    actual_is_array = (TY_kind(ty_actual) == KIND_ARRAY);
        }
        else
    	    actual_is_array = (TY_kind(ty_actual) == KIND_ARRAY);
    
#ifdef KEY
	UINT64 formal_type_size, actual_type_size;

	if (INLINE_Check_Compatibility == RELAXED_CHECK)
	{
	  formal_type_size = TY_size (ty_formal);
	  actual_type_size = TY_size (ty_actual);

	  if (!actual_is_array && formal_is_array && 
	      (!formal_type_size /* variable-sized formal array */ ||
	      actual_type_size == formal_type_size))
	  {
	    // check if actual is a complex and formal is array of 2 floats
	    TY_IDX formal = base_type_of_array (ty_formal);
	    TYPE_ID actual_type = BASETYPE (ty_actual);
	    TYPE_ID formal_type = BASETYPE (formal);
	    if (
#if defined(TARG_IA64)
		(actual_type == MTYPE_C10 && formal_type == MTYPE_F10) ||
#endif
		(actual_type == MTYPE_C8 && formal_type == MTYPE_F8) ||
		(actual_type == MTYPE_C4 && formal_type == MTYPE_F4))
	      return TRUE;

	    // Actual can be complex, and formal can be array of 1 formal or
	    // of variable size where size should be 1.
	    if (
#if defined(TARG_IA64)
		(actual_type == MTYPE_C10 && formal_type == MTYPE_C10) ||
#endif
		(actual_type == MTYPE_C8 && formal_type == MTYPE_C8) ||
		(actual_type == MTYPE_C4 && formal_type == MTYPE_C4))
	      return TRUE;
	  }
	}
#endif // KEY

        // PV 374125, don't inline in this case
        // where one of the parameters is an array and the
        // other is a scalar
        if ((!actual_is_array && formal_is_array))
    	    return FALSE;
    
        if (actual_is_array) {
	    ty_actual = base_type_of_array(ty_actual);
    	    actual_element_size = BASETYPE(ty_actual);
	}
        else 
    	    actual_element_size = BASETYPE(ty_actual);
    
        if (formal_is_array)  {
	    ty_formal = base_type_of_array(ty_formal);
    	    formal_element_size = BASETYPE(ty_formal);
	}
        else 
    	    formal_element_size = BASETYPE(ty_formal);
        
        if (formal_element_size == actual_element_size)
    	    return TRUE;

#ifdef KEY
	if (INLINE_Check_Compatibility == RELAXED_CHECK)
	{
	  if (actual_is_array && formal_is_array &&
	      (
#if defined(TARG_IA64)
	       (actual_element_size==MTYPE_C10 &&
		formal_element_size==MTYPE_F10) ||
#endif
	       (actual_element_size==MTYPE_C8 &&
		formal_element_size==MTYPE_F8) ||
	       (actual_element_size==MTYPE_C4 && 
	        formal_element_size==MTYPE_F4)))
	  {
	    if (!formal_type_size || !actual_type_size ||
	        formal_type_size == actual_type_size)
	      return TRUE;
	  }
	  // the other way
	  if (actual_is_array && formal_is_array &&
	      (
#if defined(TARG_IA64)
	       (actual_element_size==MTYPE_F10 && 
	        formal_element_size==MTYPE_C10) ||
#endif
	       (actual_element_size==MTYPE_F8 && 
	        formal_element_size==MTYPE_C8) ||
	       (actual_element_size==MTYPE_F4 && 
	        formal_element_size==MTYPE_C4)))
	  {
	    if (!formal_type_size || !actual_type_size ||
	        formal_type_size == actual_type_size)
	      return TRUE;
	  }
	}
#endif // KEY

    }
	
        TYPE_ID desc = BASETYPE(ty_formal);
	if ((desc == 0) && (TY_kind (ty_formal) == KIND_FUNCTION)) {
    	    TY_IDX ret_ty_idx = TYLIST_type (Tylist_Table[TY_tylist (ty_formal)]);

    	    if (ret_ty_idx == 0)
        	return FALSE;
	    else
		desc = BASETYPE(ret_ty_idx);
	}

	if (desc == 0)
	    return FALSE;	// Don't know what the basetype is

        OPCODE stid  = Stid_Opcode[desc];

        if (desc == MTYPE_M)
	    /* we just check the size of the formal and actual */
	    return (TY_size(ty_formal) == TY_size(ty_actual));
    
        if (stid == OPC_UNKNOWN)
	    return FALSE;

	TYPE_ID rtype = BASETYPE(ty_actual);
	TYPE_ID ltype = OPCODE_desc(stid);
#ifdef KEY
        if (INLINE_Type_Mismatch || IPO_Types_Are_Compatible(ltype, rtype))
#else
        if (IPO_Types_Are_Compatible(ltype, rtype))
#endif
	    return TRUE;

    return FALSE;
}

//--------------------------------------------------------------------------
// check if return types are okay
//--------------------------------------------------------------------------
static BOOL
param_types_are_compatible (IPA_NODE* caller_node, IPA_NODE* callee_node, IPA_EDGE *ed)
{
#ifdef KEY
    // num_formals is actually the lesser of the # of actual parameters and
    // # of formal parameters.
    INT num_formals = ed->Num_Actuals() < callee_node->Num_Formals() ? 
    			ed->Num_Actuals() : callee_node->Num_Formals();
#else
    INT num_formals = callee_node->Num_Formals();
#endif

    if (!num_formals) // No types to check
	return TRUE;

    SUMMARY_FORMAL* callee_formal = IPA_get_formal_array(callee_node);
    SUMMARY_ACTUAL* call_actual = IPA_get_actual_array(caller_node);
    SUMMARY_SYMBOL* caller_symbols = IPA_get_symbol_array(caller_node);
    SUMMARY_SYMBOL* callee_symbols = IPA_get_symbol_array(callee_node);

    SUMMARY_ACTUAL *actuals = &call_actual[ed->Summary_Callsite()->Get_actual_index()];
    SUMMARY_FORMAL *formals = &callee_formal[callee_node->Summary_Proc()->Get_formal_index()];

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    const IPC_GLOBAL_IDX_MAP* callee_idx_maps = IP_FILE_HDR_idx_maps(callee_node->File_Header());
    const IPC_GLOBAL_IDX_MAP* caller_idx_maps = IP_FILE_HDR_idx_maps(caller_node->File_Header());
    Is_True(callee_idx_maps && caller_idx_maps, ("idx_maps for caller and callee are not set up\n"));
#endif // _STANDALONE_INLINER

    BOOL lang = ((callee_node->Summary_Proc()->Get_lang() == LANG_F77) || 
    		(callee_node->Summary_Proc()->Get_lang() == LANG_F90));


    for (INT i=0; i<num_formals; ++i) {
	TY_IDX ty_formal = formals[i].Get_ty();
	TY_IDX ty_actual = actuals[i].Get_ty();

	if (!IPA_Enable_Inline_Char_Array) {
            if (((TY_kind(ty_formal) == KIND_SCALAR) && 
			(formals[i].Is_ref_parm() || lang)) &&
	    		actuals[i].Is_value_parm())
	        return FALSE;   // formal is decl. as a scalar formal reference parm and 
			        // actual is passed-by-value, don't match
	}

	if (!IPA_Enable_Inline_Var_Dim_Array && formals[i].Is_var_dim_array())
	    return FALSE;	// Don't inline var-dim array

        // turn off inlining when the formal is of
        // sclass: scalar_formal_ref and its kind is STRUCT
        // We can only inline scalar or array FORMAL_REFS
        if (formals[i].Is_ref_parm() && (TY_kind(ty_formal) == KIND_STRUCT)) {
	    if (!IPA_Enable_Inline_Struct) 
                return FALSE;
	    else {
		if (TY_kind(ty_actual) == KIND_POINTER) {
    	    	    if (TY_kind(TY_pointed(ty_actual)) == KIND_ARRAY) 
		  	if (!IPA_Enable_Inline_Struct_Array_Actual)
			    return FALSE;
                }
                else {
    	            if (TY_kind(ty_actual) == KIND_ARRAY)
		  	if (!IPA_Enable_Inline_Struct_Array_Actual)
			    return FALSE;
	        }
	    }
	}

	SUMMARY_SYMBOL* s = callee_symbols + formals[i].Get_symbol_index();

	if (actuals[i].Get_symbol_index () >= 0) {
            SUMMARY_SYMBOL* caller_sym =
            	caller_symbols + actuals[i].Get_symbol_index ();

            if (s->Is_addr_f90_target () != caller_sym->Is_addr_f90_target ())
            	return FALSE;
	} else if (s->Is_addr_f90_target ())
	    return FALSE;

	if (IPA_Enable_Inline_Optional_Arg && s->Is_optional() &&
		(ty_actual == 0))  // Skip over optional argument
	    continue;

	if (!types_are_compatible(ty_actual, ty_formal, lang))
	    return FALSE;
    }
    return TRUE;
}

void
IPA_NODE::UpdateSize (IPA_NODE *callee, IPA_EDGE *ed)
{
    if (callee->Has_Varargs()) {
       /* If callee is a varargs, we will only do partial inlining
          and hence we estimate a much smaller size increase.
          TODO:     
          We want to adjust the size estimate for all partially
          inlined functions. But since the partial inlining candidates
          and paths are not determined until the IPO phase, this
          adjustment is not easy here. Once we move the partial
          inlining candidate selection to the IPL phase and the decision
          to the ipa_inline phase, we can solve this size adjustment
          problem.
          TODO:
          To adjust the size in the feedback case.
        */
       _pu_size.Inc_PU_Size (ESTIMATED_PART_INL_BB_CNT,
                             ESTIMATED_PART_INL_STMT_CNT, 0);
       return;
    } 

    _pu_size += callee->PU_Size();
#ifdef KEY
    _pu_size.Inc_PU_Size (-1, 0, -1);
#else
    _pu_size.Inc_PU_Size (-1, callee->Num_Formals(), -1);
#endif

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    if (IPA_Use_Effective_Size && Has_frequency() &&
	callee->Has_frequency () && ed->Summary_Callsite()->Get_frequency_count().Known()) {
	SUMMARY_FEEDBACK *fb = Get_feedback ();
	SUMMARY_FEEDBACK *callee_fb = callee->Get_feedback ();
	fb->Inc_effective_bb_count (callee_fb->Get_effective_bb_count () - 1);
	fb->Inc_effective_stmt_count (callee_fb->Get_effective_stmt_count () + 
				      callee->Num_Formals());
	fb->Inc_cycle_count ((ed->Get_frequency () / callee->Get_frequency ()) * callee_fb->Get_cycle_count ());
    }
#endif // _STANDALONE_INLINER
    
} // IPA_NODE::UpdateSize


//--------------------------------------------------------------
// now update the call graph, i.e. simply increment the inline
// count, set the edge to no_inline, adjust the total program
// size
//--------------------------------------------------------------
void
Update_Call_Graph (IPA_NODE *n, IPA_EDGE *edge)
{
    /* by removing a call, we decrease the number of basic block by 1 */
    PU_SIZE size = n->PU_Size ();
    size.Inc_PU_Size (-1, 0, -1);
    n->Set_PU_Size (size);

    size.Set_PU_Size (1, 0, 1);
    Total_Prog_Size -= size.Weight ();

    n->Summary_Proc()->Decr_call_count ();
    n->Summary_Proc()->Decr_callsite_count ();
    // Don't update Total_Succ as the edge is still present.

    // mark the edge to indicate that caller has been updated.
    edge->Set_Updated_Caller();


} // Update_Call_Graph 


/*-------------------------------------------------------------*/
/* check to see if the callee, being procedure with nested PU, */
/* can be inlined only if all its nested PU are inlined        */
/*-------------------------------------------------------------*/
static BOOL
no_inline_pu_with_nested_pus(IPA_NODE* caller, IPA_GRAPH* cg)
{
    const PU_Info* pu = caller->PU_Info ();
    if (pu == NULL) 			// alt.entry
	return TRUE;
    for (pu = PU_Info_child (pu); pu; pu = PU_Info_next (pu)) {

	const AUX_PU& aux_pu =
	    Aux_Pu_Table [ST_pu (St_Table [PU_Info_proc_sym (pu)])];
	const IPA_NODE* child = cg->Node_User (AUX_PU_node (aux_pu));
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
	if (child && (!(child->Has_Inline_Attrib() || 
                  child->Has_Must_Inline_Attrib())))
#else // _STANDALONE_INLINER
	if (child && !child->Is_Deletable ())
#endif // _STANDALONE_INLINER
	    return TRUE;
    }
    return FALSE;
}

#ifdef KEY
#include "ir_bread.h"		// for WN_get_section_base
#include "sys/elf_whirl.h"	// for WT_COMP_FLAGS
// Fills in the vector with options
static void
get_command_options (IPA_NODE * node, vector<char *> &v)
{
    PU_IDX pu = ST_pu (node->Func_ST ());
    const IP_FILE_HDR& hdr = *AUX_PU_file_hdr (Aux_Pu_Table [pu]);

    char * base_addr = (char *) 
    	WN_get_section_base (IP_FILE_HDR_input_map_addr (hdr), WT_COMP_FLAGS);
    if (base_addr == (char *) -1)
    	ErrMsg (EC_IR_Scn_Read, "command line", IP_FILE_HDR_file_name (hdr));

    Elf64_Word argc = *((Elf64_Word *) base_addr);
    Elf64_Word * args = (Elf64_Word *) (base_addr + sizeof (Elf64_Word));

    v.reserve (sizeof (char *) * (argc - 1));
    for (INT i=1; i<argc; ++i)
    	v.push_back (base_addr + args[i]);
}

static BOOL
different_options (IPA_NODE * caller, IPA_NODE * callee)
{
    vector<char *> caller_opt;
    get_command_options (caller, caller_opt);

    vector<char *> callee_opt;
    get_command_options (callee, callee_opt);

    if (caller_opt.size() != callee_opt.size())
    	return TRUE;

    sort (caller_opt.begin(), caller_opt.end(), option_cmp());
    sort (callee_opt.begin(), callee_opt.end(), option_cmp());

    UINT size = caller_opt.size();

    for (UINT i=0; i<size; ++i)
    	if (strcmp (caller_opt[i], callee_opt[i]))
	  return TRUE;

    return FALSE;
}

#if !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
// Return TRUE if a call to this PU is suitable for pure-call optimization
static bool check_node (IPA_NODE * node)
{
    WN * func = node->Whirl_Tree(FALSE);
    Is_True (WN_operator(func) == OPR_FUNC_ENTRY, ("Unexpected WN node"));

    WN * body = WN_func_body (func);
    WN * first = WN_first (body);
    Is_True (first, ("No code in function body"));

    WN * last = WN_last (body);

    if (WN_operator (first) == OPR_PRAGMA)
      first = WN_next (first);
    Is_True (first, ("No code in function body"));

    Is_True (WN_operator (first) != OPR_PRAGMA, ("Unexpected pragma"));

    // returns an expression
    if (first == last && WN_operator (first) == OPR_RETURN_VAL)
      return FALSE;

    // returns void
    if (WN_operator (last) == OPR_RETURN)
      return FALSE;

    // no control-flow statements?
    for (; first != last; first = WN_next (first))
    {
       if (OPERATOR_is_scf (WN_operator (first)) ||
           OPERATOR_is_non_scf (WN_operator (first)))
         return TRUE;
    }

    return FALSE;
}

// Return true if a fortran reference parameter is used as a do-loop index
// and the corresponding actual is an array element. We should be able to
// inline in such scenarios. One option is to use something like COPY_IN
// and COPY_OUT immediately when the temporary is written to.
// Bugs 110, 9925
static BOOL
formal_is_loop_index (IPA_NODE * caller_node, IPA_NODE * callee_node, IPA_EDGE * ed)
{
    // num_formals is actually the lesser of the # of actual parameters and
    // # of formal parameters.
    INT num_formals = ed->Num_Actuals() < callee_node->Num_Formals() ? 
    			ed->Num_Actuals() : callee_node->Num_Formals();

    if (!num_formals) // Nothing to check
	return FALSE;

    BOOL lang = ((callee_node->Summary_Proc()->Get_lang() == LANG_F77) || 
    		(callee_node->Summary_Proc()->Get_lang() == LANG_F90));

    if (!lang)
	return FALSE;

    SUMMARY_FORMAL* callee_formal = IPA_get_formal_array(callee_node);
    SUMMARY_ACTUAL* call_actual = IPA_get_actual_array(caller_node);

    SUMMARY_ACTUAL *actuals = &call_actual[ed->Summary_Callsite()->Get_actual_index()];
    SUMMARY_FORMAL *formals = &callee_formal[callee_node->Summary_Proc()->Get_formal_index()];

    for (INT i=0; i<num_formals; ++i) {
      if (formals[i].Is_loop_index() && actuals[i].Get_pass_type() == PASS_ARRAY)
        return TRUE;
    }
    return FALSE;
}
#endif // ! _STANDALONE_INLINER && ! _LIGHTWEIGHT_INLINER
#endif // KEY

/*-------------------------------------------------------------*/
/* check to see if we should be inlining                       */
/*-------------------------------------------------------------*/
static BOOL
do_inline (IPA_EDGE *ed, IPA_NODE *caller,
	   IPA_NODE *callee, const IPA_CALL_GRAPH *cg)
{
    BOOL result = TRUE;
    const char *reason = 0;

#ifdef KEY
    if (cg->Graph()->Is_Recursive_Edge (ed->Edge_Index ())) {
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
	if (caller == callee)
	{
	// we may inline it, but that doesn't mean we can delete it.
	    caller->Set_Undeletable ();
	    caller->Set_Recursive ();
	}
	BOOL set_recursive_in_edge = TRUE;
	if (ed->Has_frequency () && callee->Has_frequency () &&
		 ed->Get_frequency().Known() && callee->Get_frequency ().Known()) {
	    if (compute_hotness (ed, callee, Effective_weight (callee))
		< IPA_Min_Hotness)
		set_recursive_in_edge = FALSE;
	}

	if (set_recursive_in_edge)
#endif //  _STANDALONE_INLINER
	    callee->Set_Recursive_In_Edge();
    }
#endif

    if (callee->Should_Be_Skipped()) {
	reason = "callee is skipped";
	ed->Set_reason_id (0);
	result = FALSE;
    }
    else if (ed->Has_Noinline_Attrib()) {
	reason = "edge is skipped";
	ed->Set_reason_id (1);
	result = FALSE;
    }
    else if (IPA_Enable_DCE && ed->Is_Deletable ()) {
        // call deleted by DCE
	reason = "call deleted by DCE";
	ed->Set_reason_id (2);
	result = FALSE;
    }
    else if (!IPA_Enable_Inline_Nested_PU && caller->Is_Nested_PU ()) {
        // Check for nested PU
	result = FALSE;
	reason = "caller is a nested procedure";
	ed->Set_reason_id (3);
    } else if ( PU_uplevel (callee->Get_PU ()) &&
		((!IPA_Enable_Inline_Nested_PU) ||
		no_inline_pu_with_nested_pus(callee, cg->Graph ()))) {
	if (callee->Has_Must_Inline_Attrib()) {
	    callee->Clear_Must_Inline_Attrib ();
	    reason = "callee has nested procedure(s) so ignore user MUST inline request";
	ed->Set_reason_id (4);
	}
	else 
	    reason = "callee has nested procedure(s)";
	ed->Set_reason_id (5);
	callee->Set_Noinline_Attrib ();
	result = FALSE;
#ifdef KEY
// Do recursive inlining only under ipa
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
    } else if (cg->Graph()->Is_Recursive_Edge (ed->Edge_Index ()) &&
    	       (caller != callee || !INLINE_Recursive)) {
#else
    } else if (cg->Graph()->Is_Recursive_Edge (ed->Edge_Index ())) {
#endif
#else	// for KEY
    } else if (cg->Graph()->Is_Recursive_Edge (ed->Edge_Index ())) {
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
	BOOL set_recursive_in_edge = TRUE;
	if (ed->Has_frequency () && callee->Has_frequency () &&
		 ed->Get_frequency().Known() && callee->Get_frequency ().Known()) {
	    if (compute_hotness (ed, callee, Effective_weight (callee))
		< IPA_Min_Hotness)
		set_recursive_in_edge = FALSE;
	}

	if (set_recursive_in_edge)
#endif //  _STANDALONE_INLINER
	    callee->Set_Recursive_In_Edge ();
#endif	// KEY
	result = FALSE;
	reason = "callee is recursive";
	ed->Set_reason_id (6);
    } else if (callee->Has_Varargs() && 
               !IPA_Enable_Partial_Inline) {
        /*
             Because WNs have not been read to memory at this
             stage, we cannot scan the body of the callee
             to determine a partial inlining candidate here.
               The plan is to move the scanning for partial
             inlining candidates to the IPL stage to address
             this phase ordering issue.
         */
	result = FALSE;
	reason = "callee is varargs";
	ed->Set_reason_id (7);

        if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
	   fprintf (Y_inlining, "In do_inline(): finding a var args\n");
        }
    } else if (callee->Summary_Proc()->Is_alt_entry() ||
	       callee->Summary_Proc()->Has_alt_entry() || 
	       caller->Summary_Proc()->Is_alt_entry()) {
	result = FALSE;
	reason = "function with alternate entry point";
	ed->Set_reason_id (8);
    }
#ifdef KEY
    else if (!INLINE_Param_Mismatch 
    	     && ed->Num_Actuals() < callee->Num_Formals())
#else
    else if (ed->Num_Actuals() < callee->Num_Formals())
#endif
    {
	result = FALSE;
	reason = "number of parameters mismatched";
	ed->Set_reason_id (9);
    } else if (callee->Summary_Proc()->Has_formal_pragma()) {
	result = FALSE;
	reason = "callee has pragmas which are associated with formals";
	ed->Set_reason_id (10);
    } else if (callee->Summary_Proc()->Has_mp_needs_lno()) {
	result = FALSE;
	reason = "callee has flag that suggested that it should be MP'ed";
	ed->Set_reason_id (11);
    } else if (callee->Summary_Proc()->Has_noinline_parallel_pragma()) {
	result = FALSE;
	reason = "callee has parallel pragmas that suggest turning off inlining";
	ed->Set_reason_id (12);
    } else if ((caller->Summary_Proc()->Has_parallel_pragma() ||
	       caller->Summary_Proc()->Has_parallel_region_pragma()) &&
	       callee->Summary_Proc()->Has_var_dim_array()) {
	result = FALSE;
	reason = "callee has VLAs and caller has parallel_pragma"; 
	ed->Set_reason_id (13);
    } else if (caller->Summary_Proc()->Has_parallel_region_pragma() &&
	       callee->Summary_Proc()->Has_pdo_pragma()) {
	result = FALSE;
	reason = "callee has PDO pramgas and caller has parallel_pragma"; 
	ed->Set_reason_id (14);
    } else if (ed->Summary_Callsite()->Is_no_inline())  {

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
        // check for pragmas and command line options before setting this
  	// call to no inline
	if ( !ed->Has_Must_Inline_Attrib() && !callee->Has_Must_Inline_Attrib()) {
#endif // _STANDALONE_INLINER 

            result = FALSE;
	    reason = "callsite pragma requested not to inline";
	ed->Set_reason_id (15);
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
	}
#endif // _STANDALONE_INLINER 

    } else if (ed->Summary_Callsite()->Is_must_inline() &&
	     !callee->Has_Noinline_Attrib())  {
        // Pragmas override commandline options
        // set the MustInline bit so that we inline regardless
        // of size
        ed->Set_Must_Inline_Attrib();

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
    } else if (callee->Summary_Proc()->Is_exc_inline() && !INLINE_Exceptions) {
#else // _STANDALONE_INLINER
    } else if (callee->Summary_Proc()->Is_exc_inline() && !IPA_Enable_Exc) {
#endif // _STANDALONE_INLINER
	result = FALSE;
	reason = "exception handling function";
	ed->Set_reason_id (16);
    } else if (callee->Summary_Proc()->Is_exc_inline() &&
	     callee->Summary_Proc()->Has_pstatic()) {
	result = FALSE;
	reason = "exception handling code with pstatics";
	ed->Set_reason_id (17);
    } else if ((UINT) cg->Node_Depth(callee) > IPA_Max_Depth) {
	result = FALSE;
	reason = "depth in call graph exceeds specified maximum";
	ed->Set_reason_id (18);
    } else if (!ed->Has_Must_Inline_Attrib() &&
	     (callee->Has_Noinline_Attrib() ||
	      (callee->Summary_Proc()->Is_no_inline() && result) ||
	      (!callee->Has_Must_Inline_Attrib() && INLINE_None ))) {
	result = FALSE;
	reason = "user requested not to inline";
	ed->Set_reason_id (19);
#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
    // if an inline function has local statics the front end marks the function
    // pre-emptible (in an attempt to not inline it) and weak
    // The inliner doesn't inline this fn and should emit a message
    // that distinguishes it from the case where a function was not inlined
    // because it was NOT marked inline
    } else if ( ( (!callee->Summary_Proc()->Is_may_inline() &&
	    !callee->Summary_Proc()->Is_must_inline()) && 
	    !INLINE_Preemptible ) && 
	    ( !callee->Has_Must_Inline_Attrib() ) && 
	    !ed->Summary_Callsite()->Is_must_inline() && 
	    !ed->Has_Must_Inline_Attrib()) {
	result = FALSE;
	if ( callee->Summary_Proc()->Has_fstatic()) 
	{
            reason = "function has local fstatics and is set preemptible";
	        ed->Set_reason_id (20);
	}
	else
	{
            reason = "function is preemptible and has not been set to mustinline";
	        ed->Set_reason_id (21);
	}

#endif // _STANDALONE_INLINER
    }
    else if (!return_types_are_compatible(callee, ed)) {
	reason = "incompatible return types";
	        ed->Set_reason_id (22);
	result = FALSE;
    }
    else if (INLINE_Check_Compatibility != AGGRESSIVE && // KEY
             !param_types_are_compatible(caller, callee, ed)) {
	reason = "incompatible parameter types";
	        ed->Set_reason_id (23);
	result = FALSE;
    } 
#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    else if (Opt_Options_Inconsistent && 
    	     caller->File_Id() != callee->File_Id()) {
    // The caller and callee come from different files, check if they
    // are to be compiled with different options
    	if (different_options (caller, callee)) {
	    result = FALSE;
	    reason = "optimization options are different for caller and callee";
	    ed->Set_reason_id (34);
	}
    }
    else if (IPA_Enable_Pure_Call_Opt && 
             !callee->Summary_Proc()->Has_side_effect() &&
	     !callee->Summary_Proc()->Is_must_inline() &&
	     !ed->Has_Must_Inline_Attrib() &&
	     !callee->Has_Must_Inline_Attrib() &&
	     // Check several heuristics to determine if it is a good
	     // candidate.
	     check_node (callee)) {
	    result = FALSE;
	    reason = "Trying to do pure-call-optimization for this callsite";
	    ed->Set_reason_id (35);
    }
    // This can only arise with ipa
    else if (callee->Is_Lang_CXX() && !caller->Is_Lang_CXX() &&
             PU_has_exc_scopes (callee->Get_PU())) {
            result = FALSE;
            reason = "not inlining C++ with exceptions into non-C++";
            ed->Set_reason_id (36);
    }
    else if (formal_is_loop_index(caller, callee, ed)) {
            result = FALSE;
            reason = "formal parameter is a loop index";
            ed->Set_reason_id (37);
    }
#endif // KEY && !_STANDALONE_INLINER && !_LIGHTWEIGHT_INLINER
#ifdef KEY
    else if (PU_is_nested_func(callee->Get_PU()) ||
             PU_uplevel(callee->Get_PU())) {
            result = FALSE;
            reason = "not inlining nested functions";
            ed->Set_reason_id (38);
    } 
#endif
    else if (PU_has_attr_noreturn(callee->Get_PU()) &&
             callee->PU_Size().Stmt_count() > TINY_SIZE ) {
            result = FALSE;
            reason = "not inlining non-tiny noreturn functions";
            ed->Set_reason_id (39);
    }
#ifdef KEY
    else if (PU_has_apply_args(callee->Get_PU())) {
            result = FALSE;
            reason = "not inlining __builtin_apply_args functions";
            ed->Set_reason_id(40);
    }
#endif
    // The following else-if must be last
    else if (!IPA_Enable_Lang) {
	if ((callee->Summary_Proc()->Get_lang() == LANG_F77) || 
	    (caller->Summary_Proc()->Get_lang() == LANG_F77)) {
	    if ((callee->Summary_Proc()->Get_lang() != LANG_F77) || 
		(caller->Summary_Proc()->Get_lang() != LANG_F77)) {
		result = FALSE;
		reason = "not inlining across language boundaries";
	        ed->Set_reason_id (24);
	    }
	}
        else if ((callee->Summary_Proc()->Get_lang() == LANG_F90) || 
                 (caller->Summary_Proc()->Get_lang() == LANG_F90)) {
	    if ((callee->Summary_Proc()->Get_lang() != LANG_F90) || 
		(caller->Summary_Proc()->Get_lang() != LANG_F90)) {
		result = FALSE;
		reason = "not inlining across language boundaries";
	        ed->Set_reason_id (25);
	    }
	}
    }

    if ( result == FALSE ) {
	Report_Reason ( callee, caller, reason , ed);
	return FALSE;
    } 

    return check_size_and_freq (ed, caller, callee, cg);
} // do_inline



// invocation cost for each IPA_EDGE during inline analysis
typedef AUX_IPA_EDGE<INT32> INVOCATION_COST;

// Assign to each call a "cost", which is used to determine the priority of
// inlining.  There are three factors, loopnest of the call, number of calls
// from the callee (0 means leaf), and size of the callee.  Also, we isolated
// out several boundary cases which we give higher priority:
//
// 1) loopnest > 0 && call_count == 0
// 2) loopnest == 0 && call_count == 0
// 3) loopnest > 0 && call_count > 0
// 4) loopnest == 0 && call_count > 0
//
// In the first 3 cases, the value of call_count does not matter,
// all we care is > or == 0.  We just sort by size.  In the 4th cases, we sort
// first by call_count, and then by the size.
static INT32
Estimated_Invocation_Cost (IPA_EDGE* edge, const IPA_CALL_GRAPH* cg)
{
    IPA_NODE* callee = cg->Callee (edge);

    INT loopnest = edge->Summary_Callsite ()->Get_loopnest ();

#if (defined(_STANDALONE_INLINER) || defined(_LIGHTWEIGHT_INLINER))
    INT32 cost = callee->Weight ();
#else
    INT32 cost = Effective_weight (callee);

    if (edge->Has_frequency () && callee->Has_frequency () &&
	    edge->Get_frequency().Known() && callee->Get_frequency ().Known()) {
	// if feedback information is available, ignore the heuristics and
	// use the "hotness" of the callee instead
	return INT32_MAX - (INT32)compute_hotness (edge, callee, cost);
    }
#endif // _STANDALONE_INLINER

    if (loopnest < 100)
	/* assume we never have loopnest > 100 */
	cost += ((100 - loopnest) << 11);
    
    if (callee->PU_Size().Call_Count () != 0) {
	if (loopnest > 0)
	    cost += (1 << 22);
	else
	    cost += (callee->PU_Size().Call_Count () << 22);
    }

    return cost;
    
} // Estimated_Invocation_Cost


// comparision function object for sorting the callsites
struct INVOCATION_COST_COMP
{
    const INVOCATION_COST& cost_vector;

    INVOCATION_COST_COMP (const INVOCATION_COST& c) : cost_vector (c) {}

    BOOL operator() (IPA_EDGE_INDEX e1, IPA_EDGE_INDEX e2) const {
	return cost_vector[e1] < cost_vector[e2];
    }
};

// For each node, create a list of call sites and sort them based on the
// cost function defined in Estimated_Invocation_Cost so that more
// desirable callees are inlined first.   
typedef vector<IPA_EDGE_INDEX> EDGE_INDEX_VECTOR;

void
Get_Sorted_Callsite_List (IPA_NODE *n, IPA_CALL_GRAPH *cg,
			  INVOCATION_COST& cost_vector,
			  EDGE_INDEX_VECTOR& callsite_list)
{
    if (cg->Num_Out_Edges(n) == 0)
	return;

    Is_True (callsite_list.empty (), ("Uninitialized callsite list"));

    IPA_SUCC_ITER edge_iter (cg, n);
    for (edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next ()) {
	IPA_EDGE *edge = edge_iter.Current_Edge ();

	if (edge) {
          IPA_NODE* callee = cg->Callee (edge);
          BOOL small_callee =  callee->Weight() <= IPA_PU_Minimum_Size;
          if ( small_callee || 
              !(edge->Is_Orig_Devirtualized() && !IPA_Inline_Original_VF) && 
              !(edge->Is_New_Devirtualized() && !IPA_Inline_New_VF)) {
	    IPA_EDGE_INDEX idx = edge->Array_Index ();
	    cost_vector[idx] = Estimated_Invocation_Cost (edge, cg);
	    callsite_list.push_back (idx);
	  }
	}
    }   

    sort (callsite_list.begin (), callsite_list.end (),
	  INVOCATION_COST_COMP (cost_vector));
} // Get_Sorted_Callsite_List


// decide if the given call could be deleted or inlined
// This function has effects when IPA_Enable_DCE is true.
// A deletable edge is marked so and the call graph and 
// call counts are updated.  So the client should make
// sure that this method is called only once on any edge.
// In future, we can separate this functionality from
// Analyze_call().  
static void
Analyze_call (IPA_NODE* caller, IPA_EDGE* edge, const IPA_CALL_GRAPH* cg)
{
    IPA_NODE* callee = cg->Callee (edge);

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
	    
    if (IPA_Enable_DCE) {
	// Do dead call elimination analysis

        if (edge->Has_Updated_Caller())
	{
	  // this edge is deleted and caller is updated, so return.
          // this will allow invoking Analyze_call() on an edge multiple
          // times.
          return;
	}
		
	if (!callee->Summary_Proc()->Has_pragma_side_effect() && // KEY
	    ((edge->Is_Deletable () || // set by const. propagation

	      (
#ifdef KEY
	       callee->Summary_Proc()->Get_callsite_count() == 0 &&
#else
	       cg->Node_Depth (callee) == 0 &&
#endif
	       !callee->Has_Direct_Mod_Ref() &&
	       !callee->Summary_Proc()->Is_alt_entry () &&
	       !callee->Summary_Proc()->Has_alt_entry () &&
	       !caller->Summary_Proc()->Is_alt_entry () &&
	       return_types_are_compatible (callee, edge))))) {

	    // KEY
	    Is_True (callee->Summary_Proc()->Get_callsite_count() ||
	             !callee->Summary_Proc()->Get_call_count(),
		     ("Callsite and call counts don't match"));

	    edge->Set_Deletable();
	    if (Trace_IPA || Trace_Perf) {
		fprintf (TFile, "%s called from ",
			 DEMANGLE (callee->Name()));
		fprintf(TFile, "%s deleted\n",
			DEMANGLE (caller->Name())); 
	    }

	    Update_Call_Graph (caller,edge);
			
	    if (IPA_Enable_DFE)
		Update_Total_Prog_Size (caller, callee, cg);

	    return;		// edge deleted, skip the inline analysis
	}
    }

    if (! IPA_Enable_Inline)
	return;
    
#endif // _STANDALONE_INLINER

    if (do_inline (edge, caller, callee, cg)) {
	    
        if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
            fprintf(Verbose_inlining,"YYY\t(%p)%-20s -------<%p>--------> (%p)%-20s\n",caller,caller->Name(),edge,callee,callee->Name());
	}

	edge->Set_Inline_Attrib ();
	Total_Inlined++;

    } else {
	edge->Clear_All_Inline_Attrib ();

        if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
            fprintf(Verbose_inlining,"NNN\t(%p)%-20s -------<%p>--------> (%p)%-20s\n",caller,caller->Name(),edge,callee,callee->Name());
	}
	if (callee->Has_Inline_Attrib())
	    callee->Clear_Inline_Attrib ();
	Total_Not_Inlined++;
    }
} // Analyze_call


/*-------------------------------------------------------------------------*/
/* Solve the interprocedural analysis phase of inlining.                   */
/*-------------------------------------------------------------------------*/

#ifdef KEY

// invocation freq for each IPA_EDGE during inline analysis
typedef AUX_IPA_EDGE<float> INVOCATION_FREQ;

// comparision function object for sorting the callsites
struct INVOCATION_FREQ_COMP
{
  const INVOCATION_FREQ& freq_vector;

  INVOCATION_FREQ_COMP( const INVOCATION_FREQ& c ) : freq_vector (c) {}

  BOOL operator() (IPA_EDGE_INDEX e1, IPA_EDGE_INDEX e2) const {
    return freq_vector[e1] > freq_vector[e2];
  }
};


void Perform_Inline_Analysis2( IPA_CALL_GRAPH* cg, MEM_POOL* pool )
{
  INVOCATION_FREQ freq_vector( cg, pool );
    
  if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
    Verbose_inlining = fopen ("Verbose_inlining.log", "w");
    N_inlining = fopen ("N_inlining.log", "w");
    Y_inlining = fopen ("Y_inlining.log", "a+");
    e_weight = fopen ("callee_wght.log","w");
  }

  Init_inline_parameters ();

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
  if( IPA_Enable_DFE ){
    inline_count = CXX_NEW (INLINE_COUNTER_ARRAY (cg, pool), pool);
  }
#endif // !_STANDALONE_INLINER

  EDGE_INDEX_VECTOR callsite_list;

  IPA_NODE_ITER cg_iter( cg, PREORDER, pool );

  /* traverse all nodes at PREORDER */
  for( cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next() ){
    IPA_NODE* caller = cg_iter.Current();

    if( caller == NULL ||
	caller->Should_Be_Skipped() ||
	cg->Num_Out_Edges(caller) == 0 ){
      Total_Not_Inlined++;
      continue;
    }

    IPA_SUCC_ITER edge_iter( cg, caller );

    for( edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next() ){
      IPA_EDGE *edge = edge_iter.Current_Edge ();

      if( edge != NULL &&
	  edge->Has_frequency() ){
	const FB_FREQ freq = edge->Get_frequency();

	if( freq.Known() &&
	    freq.Value() > 0 ){
	  const IPA_EDGE_INDEX idx = edge->Array_Index();
	  freq_vector[idx] = freq.Value();
	  callsite_list.push_back (idx);
	}
      }
    }   
  }    

  sort( callsite_list.begin(),
	callsite_list.end(),
	INVOCATION_FREQ_COMP( freq_vector ) );

  for( EDGE_INDEX_VECTOR::iterator first = callsite_list.begin();
       first != callsite_list.end();
       first++ ){
    IPA_EDGE* edge = cg->Edge(*first);
    Analyze_call( cg->Caller(edge), edge, cg );
  }

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))
  if( IPA_Enable_DFE ){
    CXX_DELETE (inline_count, pool);
    inline_count = NULL;
  }
#endif  // !_STANDALONE_INLINER

  if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
    fclose(e_weight);
    fclose(N_inlining);
    fclose(Verbose_inlining);
  }

  //return Perform_Inline_Analysis( cg, pool );
}
#endif // KEY


static
void analyze_calls_in_caller(IPA_NODE *caller, IPA_CALL_GRAPH *cg, INVOCATION_COST& cost_vector)
{ 
   if (caller == NULL)
     return;

   if (caller->Should_Be_Skipped()) { 
	    Total_Inlined++;
            return; 
   }
   EDGE_INDEX_VECTOR callsite_list;  	 
   callsite_list.clear ();
   Get_Sorted_Callsite_List (caller, cg, cost_vector, callsite_list);
   EDGE_INDEX_VECTOR::const_iterator last = callsite_list.end ();
   for (EDGE_INDEX_VECTOR::iterator first = callsite_list.begin ();
	   first != last; ++first) {
      IPA_EDGE *edge = cg->Edge(*first); 
      if (edge->Has_Inline_Attrib()) continue; 
      Analyze_call (caller, cg->Edge (*first), cg);
   }
}

void
Perform_Inline_Analysis (IPA_CALL_GRAPH* cg, MEM_POOL* pool)
{

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

    if (IPA_Enable_DFE)
	inline_count = CXX_NEW (INLINE_COUNTER_ARRAY (cg, pool), pool);

#endif // _STANDALONE_INLINER
 
   INVOCATION_COST cost_vector (cg, pool);
   if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
        Verbose_inlining = fopen ("Verbose_inlining.log", "w");
	N_inlining = fopen ("N_inlining.log", "w");
	Y_inlining = fopen ("Y_inlining.log", "a+");
	e_weight = fopen ("callee_wght.log","w");
    }
    Init_inline_parameters ();
    Inline_Analyzer analyzer(cg, pool, cost_vector);
    analyzer.analyze();
    if ( INLINE_List_Actions ) {
      analyzer.report();
    }

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

#ifdef TODO
    /* Feedback needs inline counters, so use before cleaned up */
    if( IPA_Enable_Feedback ) {
	fprintf(IPA_Feedback_prg_fd,"\nINLINING SUCCESS INFO\n\n");
    	IPA_NODE_ITER cg_iter (cg, PREORDER);
	/* process INLINE nodes for #pragma inline info */
    	for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
	    IPA_NODE* n = (IPA_NODE*)cg_iter.Current();
	    if (n == NULL) continue;
	    if ((*inline_count)[caller]) {
		/* check for cross-file inlining */
		if((caller->Get_fbk_ptr() )->Get_Cross_File_Fnd() ) {
		    fprintf( IPA_Feedback_prg_fd, "#pragma inline %s /* Cross-file - %d inlined - %d calls */\n",
			     DEMANGLE (caller->Name()),
			     (*inline_count)[caller],
			     cg->Num_In_Edges(n));
		} else {
		    fprintf(IPA_Feedback_prg_fd, "#pragma inline %s /* %d inlined - %d calls */\n",
			    DEMANGLE (caller->Name()),
			    (*inline_count)[caller],
			    cg->Num_In_Edges(n));
		}
	    }
	}
    } /* if (IPA_Enable_Feedback) */
#endif // TODO

    if (IPA_Enable_DFE) {
	CXX_DELETE (inline_count, pool);
	inline_count = 0;
    }
    
#endif  // _STANDALONE_INLINER

    if(Get_Trace ( TP_IPA, IPA_TRACE_TUNING)) {
        fclose(e_weight);
        fclose(N_inlining);
        fclose(Verbose_inlining);
    }
} // Perform_Inline_Analysis

BOOL
Inline_Analyzer::all_calls_inlined(IPA_NODE *node)
{ 
  if (the_cg->Num_In_Edges(node)==0) return FALSE; 
  IPA_PRED_ITER edge_iter( the_cg, node );
  for( edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next() ){
    IPA_EDGE *edge = edge_iter.Current_Edge ();
    if (( edge != NULL) && edge->Has_Inline_Attrib()) continue;
    return FALSE; 
  }
  return TRUE; 
} 

void 
Inline_Analyzer::update_budget(IPA_EDGE *edge)
{ 
  NODE_INDEX_VECTOR modified;
  IPA_NODE *callee = the_cg->Callee(edge);

  nodes_affected_by_inlining(edge,modified);

  NODE_INDEX_VECTOR::const_iterator last = modified.end();
  INT32 count = 0;
  for (NODE_INDEX_VECTOR::iterator first = modified.begin();
      first != last ; ++first) {
     IPA_NODE *node = the_cg->Node(*first);
     if (all_calls_inlined(node)) continue;
     count++;
  }

  if (all_calls_inlined(callee)) count--;
  budget(budget()-count*Effective_weight(callee));
}

Inline_Analyzer::Inline_Analyzer(IPA_CALL_GRAPH *cg, MEM_POOL *pool, INVOCATION_COST &cost)
{ 
  the_cg = cg;
  the_pool = pool;
  cost_vector = &cost;
  call_in_loop = (BOOL *) MEM_POOL_Alloc(pool, cg->Node_Size()*sizeof(BOOL));
  int i;
  for (i = 0; i < cg->Node_Size(); i++)
    call_in_loop[i]=FALSE;
}

BOOL
Inline_Analyzer::visited(std::map<IPA_NODE_INDEX,BOOL>&vec,IPA_NODE *node)
{
  if (vec.find(node->Array_Index()) == vec.end())
    return FALSE;
  return TRUE; 
} 

void
Inline_Analyzer::nodes_affected_by_inlining(IPA_EDGE *edge, NODE_INDEX_VECTOR &modified)
{ 
  std::map<IPA_NODE_INDEX, BOOL> visited;
  help_find_nodes_affected_by_inlining(edge,modified, visited);
} 

void
Inline_Analyzer::help_find_nodes_affected_by_inlining(IPA_EDGE *edge, 
                                                      NODE_INDEX_VECTOR &modified,
                                                      std::map<IPA_NODE_INDEX,BOOL> &visited_vector)
{ 
  // caller is definitely affected. 
  visited_vector[the_cg->Caller(edge)->Array_Index()] = TRUE;
  modified.push_back(the_cg->Caller(edge)->Array_Index());
  IPA_PRED_ITER edge_iter (the_cg, the_cg->Caller(edge));
  for (edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next ()) {
    IPA_EDGE *edge2 = edge_iter.Current_Edge ();
    if (edge2) {
      if (the_cg->Graph()->Is_Recursive_Edge (edge2->Edge_Index ())) continue; 
      if (edge2->Has_Inline_Attrib())
      { 
        if (!visited(visited_vector, the_cg->Caller(edge2)))
	{ 
          help_find_nodes_affected_by_inlining(edge2,modified,visited_vector);
	}
      }
    }
  }
}

void 
Inline_Analyzer::compute_statistics
(
  INT32&avg_wt, 
  INT32&call_count,
  INT32&avg_leaf_wt,
  INT32 &max_wt, 
  INT32&avg_fanout
)
{ 

  INT32 total_wt = 0;
  INT32 leaf_count = 0;
  INT32 total_fanout = 0;
  INT32 total_leaf_wt = 0;
  max_wt = 0;
  int i;
  int size = 0;
  for (i = 0; i < the_cg->Node_Size(); i++)
  {
    if (!call_in_loop[i]) continue;
    size++;
    IPA_NODE *node= the_cg->Node(i);

    // ignore leaves.
    INT32 fan_out = the_cg->Num_Out_Edges(node);
    if (fan_out == 0)
    { 
      leaf_count++;
      INT32 node_wt = Effective_weight(node);
      total_leaf_wt += node_wt;
      continue;
    }
    total_fanout += fan_out;
    INT32 node_wt = Effective_weight(node);
    total_wt += node_wt;
    if (max_wt < node_wt)
     max_wt = node_wt;
  }

  INT32 non_leaf_count = (size-leaf_count); 
  call_count = size;
  if (non_leaf_count == 0)
  { 
    avg_wt = 0;
    avg_fanout = 0;
    avg_leaf_wt = 0;
    return; 
  }
  else
  { 
    avg_wt = total_wt/non_leaf_count;
    avg_fanout = total_fanout/non_leaf_count;
    if (leaf_count > 0)
      avg_leaf_wt = total_leaf_wt/leaf_count; 
    else
      avg_leaf_wt = 0;
  }
} 

void
Inline_Analyzer::analyze()
{ 
  if (!INLINE_First_Inline_Calls_In_Loops
      || INLINE_Aggressive)
  {
     IPA_NODE_ITER cg_iter (the_cg, LEVELORDER, the_pool);
     /* traverse all nodes at levelorder */
     for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
	 IPA_NODE* caller = cg_iter.Current();
         if (caller==NULL)  continue;
         analyze_calls_in_caller(caller, the_cg, *cost_vector);
     }
     return;
  }

  mark_calls_in_loop();

  INT32 avg_wt = 0;
  INT32 max_wt = 0; 
  INT32 avg_leaf_wt = 0;
  INT32 avg_fanout = 0;
  INT32 call_count = 0;

  // compute the following statistics on the calls in loops. 
  //  o average size 
  //  o the max function size of function called in a loop.
  //  o average fanout of a call. 
  // use that as a guideline for inlining decisions. 
  //   plimit = avg_size + avg_fanout*avg_size;

  compute_statistics(avg_wt, call_count, avg_leaf_wt, max_wt, avg_fanout);

  IPA_NODE_ITER cg_iter (the_cg, LEVELORDER, the_pool);

  if (avg_wt != 0)
  {
    // readjust the non aggressive callee limit
    // based on the avgerage size of a function 
    // called from a loop

    INT32 orig2 = non_aggr_callee_limit;

    if (avg_wt < orig2)
       non_aggr_callee_limit = 2*orig2; 
    else
       non_aggr_callee_limit = 2*orig2 + 100;

    // use a 50% inline budget for calls in loops
    budget(avg_wt*call_count/2);

    if ( INLINE_List_Actions ) {
      fprintf(stderr, "call stats: avg_wt=%d, avg_fanout=%d, small_pu=%d, IPA_PU_Limit=%d\n",avg_wt,avg_fanout,non_aggr_callee_limit,IPA_PU_Limit);
    }


    /* traverse all nodes at levelorder */
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
       IPA_NODE* caller = cg_iter.Current();
       if ( (caller==NULL) ||
            !call_in_loop[caller->Array_Index()])
         continue;
       inline_calls_in_caller(caller);
       if (budget() < 0) break;
    }

    // reset the parameters to original values.
    non_aggr_callee_limit = orig2;
  }

  /* traverse all nodes at levelorder */
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
	IPA_NODE* caller = cg_iter.Current();
        if (caller==NULL) continue;
        analyze_calls_in_caller(caller, the_cg, *cost_vector);
  }

}

void
Inline_Analyzer::inline_calls_in_caller
(
  IPA_NODE *call 
)
{
  if ( INLINE_List_Actions ) {
  fprintf(stderr, "Current budget is %d\n", budget());
  fprintf(stderr, "Picking node %s with weight %d for inlining \n",call->Name(),Effective_weight(call));
  }
  analyze_calls_in_caller(call, the_cg, *cost_vector);

  IPA_SUCC_ITER edge_iter(the_cg, call);
  for( edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next() ){
  IPA_EDGE *edge = edge_iter.Current_Edge ();
  if (( edge != NULL)
      && (edge->Has_Inline_Attrib()))
  { 
    update_budget(edge); 
  }
  }
}

static
char *reason(IPA_EDGE *edge)
{ 
  INT32 reason_id = edge->reason_id();
  switch (reason_id)
  { 
    case 0:
	return "callee is skipped";
    case 1:
        return "edge is skipped";
    case 2:
        return "call deleted by DCE";
    case 3:
	return "caller is a nested procedure";
    case 4:
        return "callee has nested procedure(s) so ignore user MUST inline request";
    case 5:
        return "callee has nested procedure(s)";
    case 6:
        return "callee is recursive";
    case 7:
	return "callee is varargs";
    case 8:
	return "function with alternate entry point";
    case 9:
        return "number of parameters mismatched";
    case 10:
	return "callee has pragmas which are associated with formals";
    case 11:
	return "callee has flag that suggested that it should be MP'ed";
    case 12:
	return "callee has parallel pragmas that suggest turning off inlining";
    case 13:
	return "callee has VLAs and caller has parallel_pragma"; 
    case 14:
	return "callee has PDO pramgas and caller has parallel_pragma"; 
    case 15:
	    return "callsite pragma requested not to inline";
    case 16:
	return "exception handling function";
    case 17:
	return "exception handling code with pstatics";

    case 18:
	return "depth in call graph exceeds specified maximum";
    case 19:
	return "user requested not to inline";
    case 20:
        return "function has local fstatics and is set preemptible";
    case 21:
        return "function is preemptible and has not been set to mustinline";
    case 22:
	return "incompatible return types";
    case 24:
    case 25:
	return "not inlining across language boundaries";
    case 26:
      return "combined size exceeds -IPA:plimit";
    case 27:
        return "hotness  < -IPA:min_hotness ";
    case 28:
      return "callee size > -IPA:callee_limit";
    case 29:
      return "callee_size > -INLINE:aggressive=off callee limit";
    case 30:
      return "small, but size but exceeds hard function size limit";
    case 31:
      return "Olimit exceeds -OPT:Olimit";
    case 32:
      return       "Edge is never invoked";
    case 33:
      return "Density  > Max_density ";
    case 34:
      return "optimization options are different for caller and callee";
  case 35:
	    return "Trying to do pure-call-optimization for this callsite";
  case 36:
            return "not inlining C++ with exceptions into non-C++";
  case 37:
            return "formal parameter is a loop index";
  case 38:
            return "not inlining nested functions";
  case 39:
            return "not inlining non-tiny noreturn functions";
  case 40:
            return "not inlining __builtin_apply_args functions";
  default:
    return "unknown reason";
  } 
} 



void 
Inline_Analyzer::mark_calls_in_loop()
{
  INT32 i;
  NODE_INDEX_VECTOR worklist;

  IPA_NODE_ITER cg_iter (the_cg, LEVELORDER, the_pool);

  /* traverse all nodes at levelorder */
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
    IPA_NODE* caller = cg_iter.Current();
    if (caller == NULL) continue; 
    IPA_SUCC_ITER edge_iter(the_cg, caller);
    for( edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next() ){
      IPA_EDGE *edge = edge_iter.Current_Edge ();
      if( edge != NULL)
      { 
	INT loopnest = edge->Summary_Callsite ()->Get_loopnest ();
        IPA_NODE *callee = the_cg->Callee(edge);
        if (loopnest > 0) 
        {
          if (!call_in_loop[callee->Array_Index()])
          {
            call_in_loop[callee->Array_Index()]=TRUE;
            worklist.push_back(callee->Array_Index());
	  }
	}
      }   
    }
  }

  while (!worklist.empty())
  { 

    IPA_NODE_INDEX caller = worklist.back();
    // remove it from the work list. 
    worklist.pop_back();
    IPA_SUCC_ITER edge_iter (the_cg, the_cg->Node(caller));
    for (edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next ()) {
    IPA_EDGE *edge = edge_iter.Current_Edge ();
    if (edge != NULL)
    {
      IPA_NODE_INDEX callee = the_cg->Callee(edge)->Array_Index();
      if (!call_in_loop[callee])
      {
        call_in_loop[callee]=TRUE;
        worklist.push_back(callee);
      }
    }
    }
  }
}


void
Inline_Analyzer::report()
{

   // For each of the calls in loops, find the number of edges
   // not inlined and report. 
   /* traverse all nodes at levelorder */

   IPA_NODE_ITER cg_iter( the_cg, PREORDER, the_pool );
   for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
       IPA_NODE* caller = cg_iter.Current();
       if (caller==NULL) continue; 
       INT32 inlined_edges=0;
       IPA_SUCC_ITER edge_iter (the_cg, caller);
       INT32 total = the_cg->Num_Out_Edges(caller);
       BOOL in_loop = call_in_loop[caller->Array_Index()];
       if (the_cg->Num_Out_Edges(caller) == 0) continue; 
       fprintf(stderr, "caller=%s %s weight=%d fully inlined = %s\n",caller->Name(), in_loop ? "*" : "", Effective_weight(caller), all_calls_inlined(caller) ? "yes" : "no");
       for (edge_iter.First (); !edge_iter.Is_Empty (); edge_iter.Next ()) {
          IPA_EDGE *edge = edge_iter.Current_Edge ();
          if (edge != NULL)
          {
	     if (edge->Has_Inline_Attrib())
             { 
             fprintf(stderr, "   edge %d : inlined %s weight(%d)\n",edge->Edge_Index(), the_cg->Callee(edge)->Name(), Effective_weight(the_cg->Callee(edge)));
	     } 
             else
             { 
             fprintf(stderr, "   edge %d : didnot %s weight(%d): reason=%s\n",edge->Edge_Index(), the_cg->Callee(edge)->Name(), Effective_weight(the_cg->Callee(edge)), reason(edge));
	     } 
          }
       }
    }

}
