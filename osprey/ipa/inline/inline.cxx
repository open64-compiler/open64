/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: inline.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/inline/inline.cxx,v $
// Revision history:
//  20-Apr-95 - Original Version
//
// Description:
//
// Standalone (intra-file) inliner.
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#include <stdio.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <cmplrs/host.h>
#ifndef _LIGHTWEIGHT_INLINER
#include <dem.h>		    // demangle()
#endif
#define USE_STANDARD_TYPES	// override unwanted defines in defs.h
#include "defs.h"

#include "wn.h"			    // WN
#include "wn_map.h"		    // WN_MAP, WN_MAP_TAB
#include "stab.h"		    // ST, TY
#include "config.h"             // WHIRL_Comma_Rcomma_On
#include "config_ipa.h"		// IPA_Enable_*, IPA_Max_Depth, ...
#include "config_opt.h"         // OPT_Cyg_Instrument
#include "dwarf_DST_mem.h"	// DST_TYPE, DST_IDX
#include "pu_info.h"		// PU_Info
#include "ir_bread.h"		// Read_*_Info
#include "ir_bwrite.h"	    // Write_*_Info
#include "tracing.h"		// TFile, Get_Trace
#include "glob.h"		    /* for [Tlog|Irb]_File_Name and Tim_File */
#include "ipo_tlog_utils.h"     // for tlog info
#include "erglob.h"		// Include the error tables
#include "privatize_common.h"   // for Rename_Privatized_COMMON()

#include "ip_graph.h"       // NODE_INDEX, NODE_ITER
#include "ipc_file.h"       // IP_FILE_HDR
#include "ipc_option.h"     // Inline_[Must|Never]_Strtab
#include "ipc_symtab.h"     // Process_Global_Symtab
#include "ipc_bread.h"      // IP_READ_pu_infos and IP_READ_pu
#include "ipl_summary.h"    // for summary info structures
#include "ipl_summarize.h"  // for generation of summary info
#include "inline_summarize.h"
#include "ipa_cg.h"         // IPA_NODE,  Orig_Prog_Weight

#include "ipa_inline.h"     // Perform_Inline_Analysis
#include "ipo_inline.h"     // Init_inline(), ...
#include "inline.h"         // extern "C" Inliner()
#include "inline_utils.h"   // Setup_Inliner_File_Header
#include "ipc_symtab_merge.h"
#include "ipa_nested_pu.h" // Build_Nested_Pu_Relations
#include "privatize_common.h"   // for Rename_Privatized_COMMON
#include "inline_split_common.h"
#ifdef KEY
#include "ipo_inline_util.h"	// for Get_enclosing_region
#include "ipa_trace.h"
#endif

#include "mempool.h"         // MEM_Trace

#ifdef IPA_DEBUG
#include "ir_reader.h"      // IR_reader_init
#include "wn_simp.h"        // WN_Simplifier_Enable
#endif

#include "timelib.h"

// temporary placeholder until feedback is fixed
// BOOL FB_PU_Has_Feedback = FALSE;

#ifndef KEY
#define BZERO(b, len) \
bzero((void *)(b), (int)(len))
/* copied for now from ld3264/ld_defs.h */
#endif /* KEY */

const UINT32 inliner_main_file_index = 0;
SCOPE** Inliner_Aux_Pu_Table;	// for mapping PUs to SCOPE

static MEM_POOL scope_mpool;
static MEM_POOL inline_mpool;

static WN*  pu;
static BOOL is_cplusplus;
#ifndef _LIGHTWEIGHT_INLINER
static char dem_name[MAXDBUF];
static char dem_name2[MAXDBUF];
#endif
/* These totals are just like Total_Inlined and Total_Not_Inlined for IPA */
static int ITotal_Inlined = 0; 
static int ITotal_Not_Inlined = 0;

typedef enum phases
{
  PHASE_INPUT = 0,
  PHASE_SUMMARIZE = 1,
  PHASE_ANALYZE = 2,
  PHASE_DFE = 3,
  PHASE_COMMON_SPLIT = 4,
  PHASE_OUTPUT = 5,
  PHASE_INLINING = 6,
  PHASE_CALL_GRAPH = 7,
} PHASES;

#define LAST_PHASE	8

static double result_times[LAST_PHASE] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };

MEM_POOL *PU_pool;

SUMMARY *Summary;

extern MEM_POOL Ipo_mem_pool;
extern WN_MAP Parent_Map;
extern BOOL Debug_On;
extern BOOL Verbose;
BOOL Trace_CopyProp = FALSE; // allow tracing of copy propogation
BOOL Trace_Inline = FALSE; // allow tracing of inliner 
#ifdef _LIGHTWEIGHT_INLINER
SCOPE* Orig_Scope_tab;
#endif // _LIGHTWEIGHT_INLINER


extern IP_FILE_HDR_TABLE IP_File_header;  // array of IP_FILE_HDR, which
                                   // holds per file information

#ifdef _LIGHTWEIGHT_INLINER
AUX_PU_TAB Aux_Pu_Table;
#endif // _LIGHTWEIGHT_INLINER

static void
Perform_inline (IPA_NODE* caller_node, IPA_NODE* callee_node, 
        	IPA_EDGE* edge);
static void 
inline_init(IP_FILE_HDR& hdr);

DYN_ARRAY<char*>* Ipl_Symbol_Names = NULL;
DYN_ARRAY<char*>* Ipl_Function_Names = NULL;

static void
Read_PU(PU_Info *current_pu);

struct INLINE_COUNTER
{
    UINT32 Num_Calls_Inlined;		// when this count equals to the
					// total number of calls to this
					// node, the out-of-line copy can
					// be deleted.
    UINT32 Num_Calls_Processed;		// when this count equals to the
					// total number of calls to this
					// node, we are done with it.
};

typedef AUX_IPA_NODE<INLINE_COUNTER> INLINE_COUNTERS;

static INLINE_COUNTERS* inline_counters;

static void 
set_timer(void)
{
    if (INLINE_Get_Time_Info) {
	clear_timer(1);
	start_timer(1);
    }
}

static void 
get_timer(PHASES p)
{
    if (INLINE_Get_Time_Info) {
	stop_timer(1);
	result_times[p] += get_timer_time(1);
    }
}

static void 
time_summary(void)
{

    if (INLINE_Get_Time_Info) {

	double total_result_times = 0.0;
	FILE* fp = stderr;

   	for (int i = 0; i < LAST_PHASE; ++i)
	    total_result_times += result_times[i];

    	if (Trace_Inline)
	    fp = TFile;

	fprintf(fp, "Time summary information from the inliner:\n\n");
        fprintf(fp, "  PHASES:\t\t\tabsolute time\n");
        fprintf(fp, "  -------------------------------------\n");
        fprintf(fp, "  INPUT\t\t\t\t%5.2fsec\n", result_times[PHASE_INPUT]);
        fprintf(fp, "  SUMMARIZE\t\t\t%5.2fsec\n", result_times[PHASE_SUMMARIZE]);
        fprintf(fp, "  ANALYZE\t\t\t%5.2fsec\n", result_times[PHASE_ANALYZE]);
        fprintf(fp, "  DFE\t\t\t\t%5.2fsec\n", result_times[PHASE_DFE]);
        fprintf(fp, "  COMMON_SPLIT\t\t\t%5.2fsec\n", result_times[PHASE_COMMON_SPLIT]);
        fprintf(fp, "  OUTPUT\t\t\t%5.2fsec\n", result_times[PHASE_OUTPUT]);
        fprintf(fp, "  INLINING\t\t\t%5.2fsec\n", result_times[PHASE_INLINING]);
        fprintf(fp, "  CALL_GRAPH\t\t\t%5.2fsec\n", result_times[PHASE_CALL_GRAPH]);
        fprintf(fp, "  ---------------------------------------------------------------\n");
        fprintf(fp, "  Total time:\t\t\t%5.2fsec\n", total_result_times);

    }
    
}

static void
write_pu_info(PU_Info *pu)
{
	set_timer();

        Write_PU_Info (pu);

	get_timer(PHASE_OUTPUT);
}

static inline BOOL
All_Calls_Processed (const IPA_NODE* node, const IPA_CALL_GRAPH* cg)
{

    UINT32 num_p = (*inline_counters)[node].Num_Calls_Processed;

    return cg->Num_In_Edges (node) == num_p;
}

static inline BOOL
All_Calls_Inlined (const IPA_NODE* node, const IPA_CALL_GRAPH* cg)
{
    INT num_edges = cg->Num_In_Edges (node);
#ifdef _LIGHTWEIGHT_INLINER
    if (INLINE_Inlined_Pu_Call_Graph && (num_edges > 1)) {
	if (Pred_Is_Root(node))
	    num_edges -= 1;
    }
#endif //  _LIGHTWEIGHT_INLINER
    UINT32 num_i = (*inline_counters)[node].Num_Calls_Inlined;
    return num_edges == num_i;
}


//----------------------------------------------------------------
// local computation
//----------------------------------------------------------------
static void
IP_local_inline(SUMMARY* summary, BOOL compute_summary)
{
    Summary = summary;
    Trace_Inline = Get_Trace ( TP_INLINE, 2 );

#ifndef _LIGHTWEIGHT_INLINER
    // Tracing of copy prop also
    if (INLINE_Enable_Copy_Prop) {
      Trace_CopyProp = Get_Trace ( TP_INLINE, 1 );

      if (Trace_CopyProp) {
	demangle(ST_name(WN_st(pu)),dem_name);
	fprintf (TFile, "\nComputing summary info for function: %s \n",dem_name);
      }
    }
#endif // _LIGHTWEIGHT_INLINER

    Init_Aux_Symbol_Info (CURRENT_SYMTAB);
    Recompute_Addr_Taken (pu, Summary);
    if (compute_summary) {

	set_timer();

        Summary->Process_procedure (pu);

	get_timer(PHASE_SUMMARIZE);

    }
}



//======================================================================
// don't inline under these conditions even for the standalone inliner
//======================================================================
static BOOL
Is_do_inline (EDGE_INDEX ed, IPA_EDGE_INDEX ipa_ed)
{

  IPA_NODE& callee = *(IPA_Call_Graph->Callee(ed));
  IPA_NODE& caller = *(IPA_Call_Graph->Caller(ed));
  IPA_EDGE& edge   = *(IPA_Call_Graph->Edge(ipa_ed));

  if (&caller == &callee)
      return FALSE;			// recursive

  if (callee.Has_Varargs ())
      return FALSE;			// varargs

  if (IPA_Call_Graph->Graph()->Is_Recursive_Edge (edge.Edge_Index ()))
      return FALSE;

#ifndef _LIGHTWEIGHT_INLINER
  if (!edge.Has_Noinline_Attrib() && 
        (edge.Has_Must_Inline_Attrib() || edge.Has_Inline_Attrib())) {
#else // _LIGHTWEIGHT_INLINER
  if (!edge.Has_Noinline_Attrib() &&
	(edge.Has_Must_Inline_Attrib() || edge.Has_Inline_Attrib() ||
	 callee.Has_Must_Inline_Attrib() || callee.Has_Inline_Attrib())) {
#endif // _LIGHTWEIGHT_INLINER

      if ( INLINE_List_Actions ) {
#ifndef _LIGHTWEIGHT_INLINER
          if (callee.Summary_Proc()->Get_lang() == LANG_CPLUS) {
              demangle (callee.Name(), dem_name);
      	      demangle(caller.Name(), dem_name2);
      	      fprintf ( stderr, "Inlining %s into %s (edge# %d)\n",
                  dem_name, dem_name2, ed );      
      	      if ( Tracing_Enabled ) {
    		  fprintf ( TFile, "%s inlined into %s:",
         	      dem_name, dem_name2 );
        	  fflush ( TFile );
      	      }
          }
          else {
    	      fprintf ( stderr, "Inlining %s into %s (edge# %d)\n",
         	  callee.Name(), caller.Name(), ed );      
          }
#else
          fprintf ( stderr, "Inlining %s into %s (edge# %d)\n",
                    callee.Name(), caller.Name(), ed );
#endif
      }
      if ( Tracing_Enabled ) {
          fprintf ( TFile, "%s inlined into %s:",
           	callee.Name(), caller.Name() );
          fflush ( TFile );
      }

      return TRUE;
  }
  return FALSE;
}



//----------------------------------------------------------------
// walk the call graph and perform inlining
//----------------------------------------------------------------

static void
Write_inline_pu(IPA_NODE* node)
{
    Current_Map_Tab = PU_Info_maptab(node->PU_Info());
 
    if (!node->Is_Deletable ()) {
	Scope_tab = node->Scope();
#ifdef Is_True
	CURRENT_SYMTAB = node->Lexical_Level();
        WN_verifier(node->Whirl_Tree());
#endif

        write_pu_info (node->PU_Info());

    }
    node->Set_Processed();
#ifdef _LIGHTWEIGHT_INLINER
    if (INLINE_Use_Malloc_Mempool && INLINE_Free_Malloc_Mempool)
        node->Free_inlined_list();
#endif // _LIGHTWEIGHT_INLINER

    Parent_Map = node->Parent_Map();
    MEM_POOL_Pop(node->Mem_Pool());
    MEM_POOL_Delete(node->Mem_Pool());

}

static void
Write_inline_succ_pu()
{
    IPA_NODE_ITER cg_iter (POSTORDER, Malloc_Mem_Pool);

    for (cg_iter.First (); !cg_iter.Is_Empty(); cg_iter.Next ()) {
	IPA_NODE* node = cg_iter.Current ();

	if (node && !node->Is_Non_Local() && !node->Is_Processed ())
	    Write_inline_pu (node);
    }
}

static void
Write_callee(IPA_NODE* callee, BOOL non_local, BOOL inline_performed)
{
    if (All_Calls_Processed (callee, IPA_Call_Graph)) {
      if (IPA_Enable_DFE
#ifdef KEY
	  && OPT_Cyg_Instrument == 0  // Bug 750; TODO: Relax restriction
#endif
	  && All_Calls_Inlined (callee, IPA_Call_Graph)
	  && !callee->Is_Externally_Callable ()
#ifdef KEY
	    && ! PU_no_delete(Pu_Table[ST_pu(callee->Func_ST())])
#endif
	    ) {
	    callee->Set_Deletable();
	    /* mark this ST not_used */
            Set_ST_is_not_used(callee->Func_ST());
	}

#ifndef _LIGHTWEIGHT_INLINER
        if (non_local) { // callee is from a different(non-local) file
	    callee->Set_Deletable();
	    Set_ST_sclass(callee->Func_ST(), SCLASS_EXTERN);
	    if (inline_performed)
	        Set_ST_is_not_used(callee->Func_ST());
   	}

#endif // _LIGHTWEIGHT_INLINER

        if (!INLINE_Keep_PU_Order || callee->Is_Deletable()) 
	    /* if node is deletable, Write_inline_pu will
             * release the memory used by the pu without writing 
	     */
	    Write_inline_pu(callee);

    }
}

static void
Write_caller(IPA_NODE* node)
{
    if (!INLINE_Keep_PU_Order) {
	if (All_Calls_Processed (node, IPA_Call_Graph)) 
	    Write_inline_pu(node);
    }
}

static void
Inline_callees_into_caller(IPA_NODE* caller)
{
    caller->Scope ();

    IPA_SUCC_ITER succ_iter (caller->Node_Index());

    
    for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {

	set_timer();

        IPA_EDGE* e = succ_iter.Current_Edge();
	EDGE_INDEX call = e->Edge_Index();
		
 	e->Set_Whirl_Node(e->Summary_Callsite()->Get_wn());

	IPA_NODE* callee = IPA_Call_Graph->Callee(call);

	Current_Map_Tab = PU_Info_maptab(caller->PU_Info());
#ifdef KEY
// Current_Map_Tab is set to the proper value above, and is required in
// Get_enclosing_region(). Parent_Map is set in the function
	Get_enclosing_region (caller, e);
#endif

	BOOL inline_performed = FALSE;
	if (Is_do_inline(call, e->Array_Index()) && !caller->Is_Deletable()) {

	    MEM_POOL_Popper pool (&Ipo_mem_pool);
            if ( Get_Trace ( TKIND_ALLOC, TP_IPA ) ) {
                fprintf ( TFile,
                    "\n%s%s\tMemory allocation information before inlining\n%s%s\n",
                                 DBar, DBar, DBar, DBar );
                MEM_Trace ();
            }

	    callee->Scope ();

	    Perform_inline(caller, callee, e);

            if ( Get_Trace ( TKIND_ALLOC, TP_IPA ) ) {
                fprintf ( TFile,
                                 "\n%s%s\tMemory allocation information "
                                 "after inlining\n%s%s\n",
                                 DBar, DBar, DBar, DBar );

                MEM_Trace ();
            }

	    ++((*inline_counters)[callee].Num_Calls_Inlined);

            if (Verbose) {
                fprintf (stderr, "   %s [i] ", callee->Name());
#ifdef TRACE_CALL_GRAPH
                fprintf (stderr, "   icnt[%d, 0x%x] ", (*inline_counters)[callee].Num_Calls_Inlined, &((*inline_counters)[callee].Num_Calls_Inlined));
#endif // TRACE_CALL_GRAPH
                if (callee->Is_Deletable())
                    fputs ("[d] ", stderr);
                fputs("\n", stderr);
                fflush (stderr);
            }
	    ITotal_Inlined++;
	    inline_performed = TRUE;
	}
	else
	    ITotal_Not_Inlined++;

	++((*inline_counters)[callee].Num_Calls_Processed);

	get_timer(PHASE_INLINING);

#ifdef Is_True_On
	Scope_tab = callee->Scope();
        Verify_SYMTAB (callee->Lexical_Level());
#endif 

#ifdef _LIGHTWEIGHT_INLINER
	if (!INLINE_Inlined_Pu_Call_Graph)
#endif // _LIGHTWEIGHT_INLINER
	    Write_callee(callee, (&caller->File_Header() != &callee->File_Header()), inline_performed);

    }

    IPA_SUCC_ITER succ_iter2 (caller->Node_Index());

    for (succ_iter2.First(); !succ_iter2.Is_Empty(); succ_iter2.Next()) {
	    
	IPA_EDGE* edge = succ_iter2.Current_Edge();
	IPA_NODE* callee = IPA_Call_Graph->Callee(edge);

	callee->Clear_Cloned_Symtab();
    }

#ifdef Is_True_On
    Scope_tab = caller->Scope();
    Verify_SYMTAB (GLOBAL_SYMTAB);
    Verify_SYMTAB (caller->Lexical_Level());
#endif

#ifdef _LIGHTWEIGHT_INLINER
    if (!INLINE_Inlined_Pu_Call_Graph)
#endif // _LIGHTWEIGHT_INLINER
        Write_caller(caller);

}


static void
Perform_inlining()
{

    Init_inline();

    IPA_NODE_ITER cg_iter (POSTORDER, Malloc_Mem_Pool);

    MEM_POOL_Constructor ipo_pool (&Ipo_mem_pool, "Ipo_mem_pool", 0);
    
    if (INLINE_Enable_Auto_Inlining) {
	MEM_POOL_Constructor pool (&inline_mpool, "inline_mpool", TRUE);

	set_timer();

        Perform_Inline_Analysis(IPA_Call_Graph, &inline_mpool);

	get_timer(PHASE_ANALYZE);

        inline_counters = CXX_NEW (INLINE_COUNTERS (IPA_Call_Graph, &Ipo_mem_pool),
			       &Ipo_mem_pool); 

    }
    else if (INLINE_Inlined_Pu_Call_Graph)
        inline_counters = CXX_NEW (INLINE_COUNTERS (IPA_Call_Graph, Malloc_Mem_Pool),
			       Malloc_Mem_Pool); 
    else 
        inline_counters = CXX_NEW (INLINE_COUNTERS (IPA_Call_Graph, &Ipo_mem_pool),
			       &Ipo_mem_pool); 


    for (cg_iter.First (); !cg_iter.Is_Empty(); cg_iter.Next ()) {

    	IPA_NODE* caller = cg_iter.Current ();

        if (caller == NULL) {
	    if ( ! INLINE_Keep_PU_Order )
#ifdef _LIGHTWEIGHT_INLINER
    		if (!INLINE_Inlined_Pu_Call_Graph)  
#endif // _LIGHTWEIGHT_INLINER
		    Write_inline_succ_pu();
 	} else {
	    Inline_callees_into_caller(caller);
#ifdef DEBUG_SYMTAB
        Scope_tab = caller->Scope();
	Print_local_symtab(TFile, Scope_tab[caller->Lexical_Level()]);
        fflush(TFile);
#endif // DEBUG_SYMTAB
	}
    }

    if (Verbose)
        fputc ('\n', stderr);

    if ( INLINE_List_Actions ) {
        fprintf ( stderr, "Total number of edges = %d\n", IPA_Call_Graph->Edge_Size() );
    }

}




//----------------------------------------------------------------
// perform the inlining
//----------------------------------------------------------------

static void
Perform_inline (IPA_NODE* caller_node, IPA_NODE* callee_node, 
        	IPA_EDGE* edge)
{

  IPO_INLINE ip_inline(caller_node, callee_node, edge);

  ip_inline.Process();
}



static INT
Inliner_Read_PUs (IP_FILE_HDR& file_header, PU_Info *pu_tree, INT num_PU)
{
    PU_Info *current_pu;

    MEM_POOL rename_pool;
    MEM_POOL_Initialize(&rename_pool, "Rename_Pool", TRUE);

    for (current_pu = pu_tree;
	 current_pu != NULL;
	 current_pu = PU_Info_next (current_pu)) {

	IP_PROC_INFO *proci = &(IP_FILE_HDR_proc_info(file_header)[num_PU]);
	Set_IP_PROC_INFO_pu_info(*proci, current_pu);
	Set_IP_PROC_INFO_state(*proci, IPA_ORIG);

#ifdef _LIGHTWEIGHT_INLINER
	if (INLINE_Inlined_Pu_Call_Graph && !PU_Info_child(current_pu)) {
	    // Only process those PUs that are tagged inline   
	    // However, if this PU has a child (nested PU), we might as well
	    // put this node into the call graph to avoid reading dependency
	    // problem.
	    const PU& this_pu = Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(current_pu)])];
	    if (!PU_is_inline_function(this_pu)) {
#ifdef TRACE_CALL_GRAPH
	        printf ("NO inline tag -----------  %s\n", ST_name(PU_Info_proc_sym(current_pu)));
#endif // TRACE_CALL_GRAPH
		continue;
	    }
#ifdef TRACE_CALL_GRAPH
	    printf ("HAS inline tag -----------  %s\n", ST_name(PU_Info_proc_sym(current_pu)));
#endif // TRACE_CALL_GRAPH
	}
#endif // _LIGHTWEIGHT_INLINER

	Read_PU (current_pu);

	pu = PU_Info_tree_ptr(current_pu);

#ifdef _LIGHTWEIGHT_INLINER

        if (INLINE_Inlined_Pu_Call_Graph2 && !PU_Info_child(current_pu)) {
            // We try to put all callers that call inlined functions into
            // the Call Graph at this time, no later as in the case of
            // INLINE_Inlined_Pu_Call_Graph
	    const PU& this_pu = Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(current_pu)])];

            if (!Need_To_Inline_Callee(pu) && !PU_is_inline_function(this_pu)) {
                IP_local_inline ((SUMMARY *)IP_FILE_HDR_summary(file_header), FALSE);
                write_pu_info (current_pu);

                continue;
            }
#ifdef TRACE_CALL_GRAPH
            else
                printf ("HAS inline tag or HAS call to inline function  -----------  %s\n", ST_name(PU_Info_proc_sym(current_pu)));
#endif // TRACE_CALL_GRAPH
        }

#endif // _LIGHTWEIGHT_INLINER


	// For standalone inliner, need Current_PU_Info to detect we are
	// eg in a constructor. This variable we get for free from pu_info.h
	Current_PU_Info = current_pu;

        // fix PV 437716 : privatized COMMONs referenced in inlined routines
	const PU& this_pu = Get_Current_PU ();
        if (PU_has_mp(this_pu)) {
            MEM_POOL_Push(&rename_pool);
            RENAMING_STACK rename_common_stack(&rename_pool);
            rename_common_stack.Push(new RENAMING_SCOPE(NULL, &rename_pool));

            Rename_Privatized_COMMON(pu, &rename_common_stack);
            MEM_POOL_Pop(&rename_pool);
        }


	SCOPE *new_scope_tab = (SCOPE *) MEM_POOL_Alloc (Malloc_Mem_Pool,
							 (CURRENT_SYMTAB+1) * sizeof(SCOPE));

	SYMTAB_IDX i;
	for (i = 0; i <= CURRENT_SYMTAB; ++i) {
	    new_scope_tab[i] = Scope_tab[i];
	}

	Inliner_Aux_Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(current_pu)])] = new_scope_tab;

	IP_local_inline ((SUMMARY *)IP_FILE_HDR_summary(file_header), TRUE);

	num_PU += 1;

	if (PU_Info_child(current_pu)) {
	    num_PU = Inliner_Read_PUs(file_header, PU_Info_child(current_pu), num_PU);
	    Set_IP_FILE_HDR_has_nested_pu(file_header);
	}
	// Report an error if the current PU has feedback sections
	if ((current_pu) && (PU_Info_state (current_pu, WT_FREQ) == Subsect_InMem))
	    ErrMsg( EC_FB_File_Fmt, "Standalone Inliner", "Use -IPA with feedback files");


    }
    return num_PU;
}


static PU_Info *
Inliner_Write_PUs (PU_Info *pu_tree, INT *p_num_PU)
{
    PU_Info *current_pu;
    PU_Info *prev_pu = NULL;

    for (current_pu = pu_tree; current_pu != NULL; current_pu = PU_Info_next (current_pu)) {

        Current_Map_Tab = PU_Info_maptab(current_pu);

        BOOL deletable = FALSE;
        IPA_NODE* node = NULL;
        NODE_INDEX cg_node =
	    AUX_PU_node (Aux_Pu_Table[ST_pu (St_Table[PU_Info_proc_sym(current_pu)])]);
     	if (cg_node != INVALID_NODE_INDEX) 
            node = (IPA_Call_Graph->Graph()->Node_User(cg_node));

	if (node) {
#ifdef _LIGHTWEIGHT_INLINER
	    const PU& this_pu = Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(current_pu)])];
            if ((INLINE_Inlined_Pu_Call_Graph || INLINE_Inlined_Pu_Call_Graph) && PU_is_inline_function(this_pu))
#endif // _LIGHTWEIGHT_INLINER
	    {
	      if (IPA_Enable_DFE
#ifdef KEY
		  && ! OPT_Cyg_Instrument
#endif
		  && All_Calls_Inlined (node, IPA_Call_Graph)
		  && !node->Is_Externally_Callable ()
#ifdef KEY
		  && ! PU_no_delete(Pu_Table[ST_pu(node->Func_ST())])
#endif
		  ) {

                    node->Set_Deletable();
                    /* mark this ST not_used */
                    Set_ST_is_not_used(node->Func_ST());
	        }
	    }
            deletable = node->Is_Deletable();
        }


        if (IPA_Enable_DFE && deletable) {
            if (prev_pu) {
                PU_Info_next(prev_pu) = PU_Info_next(current_pu);
            } 
	    else {
                pu_tree = PU_Info_next(current_pu);
            }
        } 
	else {

	// Only write out PUs if they have not been written in the Perform_inlining phase

#ifdef _LIGHTWEIGHT_INLINER
	    if (INLINE_Inlined_Pu_Call_Graph && node) {
	        Scope_tab = node->Scope();
	        write_pu_info (current_pu);
	    }
	    else {
                if ( INLINE_Keep_PU_Order && node ) {
	            Scope_tab = node->Scope();
                    write_pu_info (current_pu);
		}
	    }
#else // _LIGHTWEIGHT_INLINER
            if ( INLINE_Keep_PU_Order )
                write_pu_info (current_pu);
#endif // _LIGHTWEIGHT_INLINER

            prev_pu = current_pu;
        }

        if (PU_Info_child(current_pu)) {
            PU_Info_child(current_pu) =
            Inliner_Write_PUs(PU_Info_child(current_pu), p_num_PU);
        }
        *p_num_PU += 1;
    }

    return pu_tree;
}

#ifdef _LIGHTWEIGHT_INLINER

static void
Process_Uninlinable_PU (const IP_FILE_HDR& file_header, PU_Info *current_pu, int& num_PU)
{
    
    Read_PU (current_pu);

    pu = PU_Info_tree_ptr(current_pu);

    if (num_PU == 0 || !Need_To_Inline_Callee(pu)) {
	IP_local_inline ((SUMMARY *)IP_FILE_HDR_summary(file_header), FALSE);
	write_pu_info (current_pu);
    }
    else {
	// Summarize and do inline process for this PU
	IP_PROC_INFO *proci = &(IP_FILE_HDR_proc_info(file_header)[num_PU]);
	Set_IP_PROC_INFO_pu_info(*proci, current_pu);
	Set_IP_PROC_INFO_state(*proci, IPA_ORIG);

	    
	Current_PU_Info = current_pu;

	SCOPE *new_scope_tab = (SCOPE *) MEM_POOL_Alloc (Malloc_Mem_Pool,
					(CURRENT_SYMTAB+1) * sizeof(SCOPE));

	SYMTAB_IDX i;
	for (i = 0; i <= CURRENT_SYMTAB; ++i) {
	    new_scope_tab[i] = Scope_tab[i];
	}

	Inliner_Aux_Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(current_pu)])] = new_scope_tab;

	IP_local_inline ((SUMMARY *)IP_FILE_HDR_summary(file_header), TRUE);

        INT proc_idx = ((SUMMARY *)IP_FILE_HDR_summary(file_header))->Get_procedure_idx();

	// Add this PU to the call_graph
 	NODE_INDEX index = INVALID_NODE_INDEX;
        IPA_NODE* caller =  Add_One_Node((IP_FILE_HDR&)file_header, inliner_main_file_index, proc_idx, index);

	Add_Edges_For_Node((IP_FILE_HDR&)file_header, num_PU, NULL, NULL);


	Inline_callees_into_caller(caller);

#ifdef DEBUG_SYMTAB
        Scope_tab = caller->Scope();
	Print_local_symtab(TFile, Scope_tab[caller->Lexical_Level()]);
        fflush(TFile);
#endif // DEBUG_SYMTAB


	num_PU += 1;
    }

}  // Process_Uninlinable_PU


//----------------------------------------------------------------
// Needs to go through the PU list to see if any of the PU
// calls a node in the Call-graph that needs to be inlined
//----------------------------------------------------------------

static PU_Info *
Process_Remaining_PUs (const IP_FILE_HDR& file_header, PU_Info *pu_tree, int& num_PU)
{
    PU_Info *current_pu;

    for (current_pu = pu_tree; current_pu != NULL; current_pu = PU_Info_next (current_pu)) {
        Current_Map_Tab = PU_Info_maptab(current_pu);
	const PU& this_pu = Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(current_pu)])];
        NODE_INDEX cg_node =
	    AUX_PU_node (Aux_Pu_Table[ST_pu (St_Table[PU_Info_proc_sym(current_pu)])]);
     	if (cg_node == INVALID_NODE_INDEX) {
		// && (!PU_is_inline_function(this_pu)) {
	// Only process those PU that has not been put into IPA_Call_Graph
	    Process_Uninlinable_PU(file_header, current_pu, num_PU);
	}

        if (PU_Info_child(current_pu)) {
            PU_Info_child(current_pu) =
                Process_Remaining_PUs(file_header, PU_Info_child(current_pu), num_PU);
        }
    }

    return pu_tree;
}

#else // _LIGHTWEIGHT_INLINER

//----------------------------------------------------------------
// the name of the input and output files
//----------------------------------------------------------------
void 
Process_Nonlocal_File(char* input_name, void* handle)
{
    PU_Info *pu_tree;
    INT32 PU_count, num_PU;

    if (handle == NULL)
        handle = Open_Input_Info (input_name);
   
    /* initialize the file header */
    IP_FILE_HDR& file_header = Setup_Inliner_File_Header (input_name, (char *)handle);

    const MEM_POOL* mpool = IP_FILE_HDR_mem_pool(file_header);
    MEM_POOL_Push((MEM_POOL *)(mpool));

    Process_Global_Symtab (handle, file_header);
    IP_READ_pu_infos(file_header);
    IPA_update_summary_st_idx(file_header);
}

#endif // _LIGHTWEIGHT_INLINER

//----------------------------------------------------------------
// the name of the input and output files
//----------------------------------------------------------------
static void 
Process_Local_File(char* input_name, void *handle, INT& num_PU)
{
    PU_Info *pu_tree;
    INT32 PU_count;

    if (handle == NULL)
        handle = Open_Input_Info (input_name);
   
    /* initialize the file header */
    IP_FILE_HDR& file_header = Setup_Inliner_File_Header (input_name, (char *)handle);

    const MEM_POOL* mpool = IP_FILE_HDR_mem_pool(file_header);
    MEM_POOL_Push((MEM_POOL *)(mpool));

    pu_tree = Read_Global_Info (&PU_count);
    inline_init(file_header);

    Initialize_Special_Global_Symbols ();

#ifdef _LIGHTWEIGHT_INLINER

    const AUX_PU aux_pu;
    Aux_Pu_Table.Insert (aux_pu);

#else // _LIGHTWEIGHT_INLINER

    Initialize_Auxiliary_Tables();

#endif // _LIGHTWEIGHT_INLINER

    Set_IP_FILE_HDR_pu_list(file_header,  pu_tree);
    Set_IP_FILE_HDR_num_procs(file_header,  PU_count);

    INT size_proc = 0;
    if (PU_count != 0) {
        // reserve room for cloned procs to minimize reallocating.
        size_proc = PU_count < 32 ? PU_count + 8 : PU_count + PU_count / 4;
    }

    Set_IP_FILE_HDR_proc_info(file_header,
	(IP_PROC_INFO*) MEM_POOL_Alloc (Malloc_Mem_Pool, (sizeof(IP_PROC_INFO)*size_proc)));
    Set_IP_FILE_HDR_max_size (file_header, size_proc);

    Set_IP_FILE_HDR_dst(file_header,  Current_DST);

    UINT size = sizeof(SCOPE*) * PU_Table_Size ();

    if (!Inliner_Aux_Pu_Table)
        Inliner_Aux_Pu_Table =  (SCOPE**) MEM_POOL_Alloc (&scope_mpool, size);
    else
        Inliner_Aux_Pu_Table =  (SCOPE**) MEM_POOL_Realloc (&scope_mpool, Inliner_Aux_Pu_Table, size, size*2);

    num_PU = Inliner_Read_PUs(file_header, pu_tree, 0);

    ((SUMMARY *)IP_FILE_HDR_summary(file_header))->Set_global_addr_taken_attrib ();

    Set_IP_FILE_HDR_file_info(file_header, File_info);

    UINT32 aux_idx;
    for (INT i = 1; i < PU_Table_Size (); ++i) {
        AUX_PU& aux_pu = Aux_Pu_Table.New_entry (aux_idx);
	aux_pu.construct();
    }

}


//----------------------------------------------------------------
// the name of the input and output files
//----------------------------------------------------------------
BOOL 
Inliner(char* input_name, char* output_name)
{
    void *handle;
    PU_Info *pu_tree;
    INT32 PU_count, num_PU;

    MEM_POOL_Push(&MEM_src_pool);
    MEM_POOL_Push(&MEM_pu_pool); 
    MEM_POOL_Push(&MEM_local_pool); 

   
    Initialize_Symbol_Tables(FALSE);

    New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);

    MEM_POOL_Initialize(&scope_mpool, "scope_mpool", TRUE);
    MEM_POOL_Push(&scope_mpool);

#ifdef _LIGHTWEIGHT_INLINER
    Orig_Scope_tab = Scope_tab;

    if (!INLINE_Inlined_Pu_Call_Graph)
#endif // _LIGHTWEIGHT_INLINER
	(void) Open_Output_Info (output_name);

    Process_Local_File(input_name, NULL, num_PU);

#ifndef _LIGHTWEIGHT_INLINER

    Process_Non_Local_Files();
 
    Process_Non_Local_Libraries();

    if (INLINE_Enable_Split_Common) {
	set_timer();

	INLINE_Split_Common ();
	
	get_timer(PHASE_COMMON_SPLIT);
    }

#else  //  _LIGHTWEIGHT_INLINER
    if (INLINE_Inlined_Pu_Call_Graph  && (num_PU == 0)) {

        Free_Input_Info ();

	set_timer();

	Copy_Input_Info_To_Output(input_name, output_name);

	get_timer(PHASE_OUTPUT);

        time_summary();

	return FALSE;
    }

    if (INLINE_Inlined_Pu_Call_Graph)
	(void) Open_Output_Info (output_name);

    INLINE_Enable_Copy_Prop = FALSE;

#endif //  _LIGHTWEIGHT_INLINER

    // handle multiple definitions of common blocks
    Fix_Aliased_Commons ();


#ifdef IPA_DEBUG
    if (!Verbose)
      WN_Simplifier_Enable(FALSE);
#endif

    Set_ipa_tlog_phase(PHASE_INLINER);
	
    set_timer();

    Build_Call_Graph();

    if (Get_Trace (TP_INLINE, IPA_TRACE_TUNING))
    {
      FILE *tmp_call_graph = fopen("cg_dump.log", "w");

      if(tmp_call_graph != NULL)
      {
        fprintf(tmp_call_graph, "\t+++++++++++++++++++++++++++++++++++++++\n");
        IPA_Call_Graph->Print(tmp_call_graph);
        fprintf(tmp_call_graph, "\t+++++++++++++++++++++++++++++++++++++++\n");
      }
      fclose(tmp_call_graph);
    }

    get_timer(PHASE_CALL_GRAPH);

#ifdef _LIGHTWEIGHT_INLINER
    if (!(INLINE_Inlined_Pu_Call_Graph || INLINE_Inlined_Pu_Call_Graph2))
#endif // _LIGHTWEIGHT_INLINER
    {
        MEM_POOL_Pop(&scope_mpool);
        MEM_POOL_Delete(&scope_mpool);
    }

#ifdef TRACE_CALL_GRAPH

    FILE* f;
    f = fopen("/usr/tmp/call.graph", "a+");
    if (IPA_Call_Graph)
        IPA_Call_Graph->Print(f);
    fclose(f);
#endif

    for (UINT i = 0; i < IP_File_header.size(); ++i) {
        if (IP_FILE_HDR_has_nested_pu(IP_File_header[i])) {
            Build_Nested_Pu_Relations();
	    break;
	}
    }

    if ( Get_Trace ( TKIND_ALLOC, TP_IPA ) ) {
      fprintf ( TFile,
           "\n%s%s\tMemory allocation information after Build_call_graph\n%s%s\n",
           DBar, DBar, DBar, DBar );
      MEM_Trace ();
    }
//  TKL added initialization of Total_Prog_Size (similar to ipa_main.cxx)
    Total_Prog_Size = Orig_Prog_Weight;
    if (Trace_Inline)
       fprintf(TFile, "\nTotal Prog Size is %d\n",Total_Prog_Size); 

    if (IPA_Enable_DFE) {
#ifdef _LIGHTWEIGHT_INLINER
 	if (!(INLINE_Inlined_Pu_Call_Graph || INLINE_Inlined_Pu_Call_Graph2)) 
#endif // _LIGHTWEIGHT_INLINER
	    {
	    set_timer();

	    Total_Prog_Size = Orig_Prog_Weight - Eliminate_Dead_Func(FALSE);

	    get_timer(PHASE_DFE);

	    }
    }

#ifdef TRACE_CALL_GRAPH

    f = fopen("/usr/tmp/call.graph2", "a+");
    if (IPA_Call_Graph)
        IPA_Call_Graph->Print(f);
    fclose(f);
#endif

    Perform_inlining();

#ifdef _LIGHTWEIGHT_INLINER
    if (INLINE_Inlined_Pu_Call_Graph || INLINE_Inlined_Pu_Call_Graph2) {
        // Now needs to go through the PU list to see if any of the PU
        // calls a node that needs to be inlined

	if (INLINE_Inlined_Pu_Call_Graph) {
            (void)Process_Remaining_PUs(IP_File_header[inliner_main_file_index], IP_FILE_HDR_pu_list(IP_File_header[inliner_main_file_index]), num_PU);

            ((SUMMARY *)IP_FILE_HDR_summary(IP_File_header[inliner_main_file_index]))->Set_global_addr_taken_attrib ();
	}

        MEM_POOL_Pop(&scope_mpool);
        MEM_POOL_Delete(&scope_mpool);
    }

#endif  // _LIGHTWEIGHT_INLINER

    Current_DST = IP_FILE_HDR_dst(IP_File_header[inliner_main_file_index]);
    num_PU = 0;
    pu_tree = Inliner_Write_PUs(IP_FILE_HDR_pu_list(IP_File_header[inliner_main_file_index]), &num_PU);

    set_timer();

    Write_Global_Info(pu_tree);

    get_timer(PHASE_OUTPUT);

    Free_Input_Info ();

    time_summary();

    return TRUE;
}


//---------------------------------------------------------------------
//---------------------------------------------------------------------
static void
Read_PU(PU_Info *current_pu)
{
    MEM_POOL *m;


    m = (MEM_POOL*) MEM_POOL_Alloc(Malloc_Mem_Pool, sizeof(MEM_POOL));
    MEM_POOL_Initialize (m, NULL, 1);
    MEM_POOL_Push (m);

#ifdef _LIGHTWEIGHT_INLINER
    Scope_tab = Orig_Scope_tab;
#endif // _LIGHTWEIGHT_INLINER

    set_timer();

    Read_Local_Info (m, current_pu);

    get_timer(PHASE_INPUT);

    PU_pool = m;
}

//---------------------------------------------------------------------
// initialize the pu array
//---------------------------------------------------------------------
static void inline_init(IP_FILE_HDR& hdr)
{
    SUMMARY *summary;
    summary = CXX_NEW (SUMMARY(Malloc_Mem_Pool), Malloc_Mem_Pool);
    
    Set_IP_FILE_HDR_summary(hdr, (char *)summary);
}



