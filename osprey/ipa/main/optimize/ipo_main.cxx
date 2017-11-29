/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
// Module: ipo_main.cxx
//
// Revision history:
//  08-Nov-94 - Original Version
//
// Description:  Interprocedural Optimization Driver
//
// This phase reads in the IR files and perform tranformations
// on it based on the results of the analysis phase. 
// The first set of optimizations will be inlining and 
// constant propagation
//
// ====================================================================
// ====================================================================

// The main clause of the #ifdef contains a stub version of this file:
// just enough so that we can build and run ipa.  The else clause contains
// the full (old symtab) version of the file.


#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#include <signal.h>
#include <alloca.h>
#include <cmplrs/rcodes.h>		// for RC_SYSTEM_ERROR
#include <float.h>
#include <ext/hash_set>
#include <queue>

#include "defs.h"
#include "erglob.h"
#include "cxx_memory.h"
#include "glob.h"			// for Ipa_File_Name
#include "wn.h"
#include "wn_simp.h"
#include "symtab.h"
#include "pu_info.h"			// For struct pu_info
#include "ipc_file.h"			// IP_FILE_HDR
#include "ipc_compile.h"		// for ipacom_*
#include "ipc_bread.h"			// Binary read.
#include "ipc_bwrite.h"			// Binary write.
#include "ipo_defs.h"
#include <cmplrs/host.h>		// for typedef string
#include "ld_ipa_option.h"		// for "keep" macro
#include "ipa_option.h"			// std. ipa options
#include "ipo_tlog_utils.h"		// for Set_ipa_tlog_phase
#include "ipa_main.h"			// for analysis phase driver
#include "ipa_cg.h"			// call graph related stuff
#include "ipo_const.h"			// for cprop routines
#include "ipc_symtab_merge.h"		// for AUX_ST
#include "ipo_main.h"
#include "ipc_weak.h"
#include "ipo_pad.h"
#include "ipo_split.h"
#include "ipo_dce.h"			// for dead call elimination
#include "ipo_inline.h"			// for inlining
#include "ipo_alias.h"			// for Mark_readonly_param
#include "ipa_lno_write.h"
#include "cgb.h"                        // CG_BROWSER, CGB_Initialize
#include "cgb_ipa.h"                    // CGB_IPA_{Initialize|Terminate}
#include "ipc_pic.h"
#include "ipo_alias_class.h"
#include "ir_bread.h"			// For second-pass WHIRL file
#include "ir_bwrite.h"			// I/O for alias class
#include "ipa_be_write.h"               // for nystrom alias summary
#include "be_symtab.h" 
#include "ipa_nystrom_alias_analyzer.h"

#include "ipc_option.h"

#include "ipa_devirtual.h"
#ifdef KEY
#include "ipa_builtins.h"
#include "ipo_parent.h"
#endif

#ifndef KEY
#include "inline_script_parser.h"
#else
extern void IPO_WN_Update_For_Struct_Opt (IPA_NODE *);
extern void IPO_WN_Update_For_Complete_Structure_Relayout_Legality(IPA_NODE *);
extern void IPO_WN_Update_For_Array_Remapping_Legality(IPA_NODE *, int, int *);
extern void IPO_Identify_Single_Define_To_HeapAlloced_GlobalVar(WN *wn);
#endif  /* KEY */
#include "ipa_reorder.h" //IPO_Modify_WN_for_field_reorder ()

extern "C" void add_to_tmp_file_list (char*);
#pragma weak add_to_tmp_file_list

extern MEM_POOL Ipo_mem_pool;
extern WN_MAP Parent_Map;

extern char * preopt_path;       // declared in ld/option.c
extern int preopt_opened;        // declared in ld/option.c
extern FILE *Y_inlining;         // declared in analyze/ipa_inline.cxx
static BOOL have_open_input_file = FALSE;
static FILE *fin;
struct reg_feedback {
    char _func_name[120];
    INT32 _stacked_callee_used;
    INT32 _stacked_caller_used;
    float _cost[96];
    reg_feedback() : _stacked_callee_used(0),
             _stacked_caller_used(0)
    {
        for (INT32 i = 0; i < 96; i++) {
            _cost[i] = 0;
        }
    }
};

typedef reg_feedback* REG_FB_POINTER;
 
namespace {
  struct eqs {
    bool operator()(char* s1, char* s2) const
      { return strcmp(s1, s2) == 0; }
  };
  typedef __gnu_cxx::hash_map<char*, REG_FB_POINTER, __gnu_cxx::hash<char*>, eqs> REG_FB_MAP;
};

static REG_FB_MAP REG_FB_INFO_TABLE;

struct reg_budget {
    INT32 _budget;
    IPA_NODE *_node;
    float _self_recursive_freq;
    reg_budget() : _budget(0),_node(NULL),
    _self_recursive_freq(0) {}
};

typedef struct reg_budget* REG_BUDGET_POINTER;
namespace {
  struct eq {
    bool operator()(char* s1, char* s2) const
      { return strcmp(s1, s2) == 0; }
  };
  typedef __gnu_cxx::hash_map<char*, REG_BUDGET_POINTER, __gnu_cxx::hash<char*>, eq> REG_BUDGET_MAP;
};

static REG_BUDGET_MAP REG_BUDGET_TABLE;
 
struct reg_list {
    IPA_NODE *_node;
    float     _spill_cost;
    struct reg_list *_next;
    struct reg_list *_prev;
    reg_list() : _node(NULL),_spill_cost(0),
                 _next(NULL),_prev(NULL) {
    }
};

INT IPO_Total_Inlined = 0;
static float REC_OVERFLOW_COST = 20;
static float OVERFLOW_COST = 20;
BOOL one_got;

static MEM_POOL Recycle_mem_pool;
MEM_POOL IPA_LNO_mem_pool; 
static IPA_LNO_WRITE_SUMMARY* IPA_LNO_Summary = NULL; 

#ifdef KEY
// For recursive inlining, ie. when caller == callee, we process the entire
// node before inlining the recursive edge(s). Store the node and edge in
// a vector of this struct for inlining later.
struct inline_info {
    IPA_NODE *_node;
    IPA_EDGE *_edge;
    inline_info (IPA_NODE *n, IPA_EDGE *e) : _node(n), _edge(e) {}
};
vector<inline_info> inline_list;
#endif

//----------------------------------------------------------------------
// Aux. info for keeping track of the transformation process:
//----------------------------------------------------------------------
typedef AUX_IPA_NODE<UINT32> NUM_CALLS_PROCESSED;
static NUM_CALLS_PROCESSED* Num_In_Calls_Processed;
static NUM_CALLS_PROCESSED* Num_Out_Calls_Processed;

// Scratch storage for identifying global-as-local candidates.
typedef enum {
  NONE = 0,
  INNERMOST_DOM = 1,
} DEF_FLAG;
#define MAX_GLOBAL_AS_LOCAL 20 // maximum number of global arrays to track.
static WN * Global_Defs[MAX_GLOBAL_AS_LOCAL];
static DEF_FLAG Global_Flags[MAX_GLOBAL_AS_LOCAL];
static ST * Global_Sts[MAX_GLOBAL_AS_LOCAL];
static INT Global_As_Local_Ndx = -1;
typedef HASH_TABLE<void *, UINT64> GLOBAL_AS_LOCAL_HASH;
static GLOBAL_AS_LOCAL_HASH * Global_As_Local_Hash;
static WN_MAP PU_Parent_Map;
static WN_MAP_TAB * PU_Map_Tab;
static MEM_POOL Temp_pool;

//FILE *inlining_result ;

static inline BOOL
All_Calls_Processed (const IPA_NODE* node, const IPA_CALL_GRAPH* cg)
{

    return ((cg->Num_In_Edges (node) == (*Num_In_Calls_Processed)[node]) &&
	    (cg->Num_Out_Edges (node) == (*Num_Out_Calls_Processed)[node]));
} // All_Callers_Processed


static void
Init_Num_Calls_Processed (const IPA_CALL_GRAPH* cg, MEM_POOL* pool)
{
    Num_In_Calls_Processed = CXX_NEW (NUM_CALLS_PROCESSED (cg, pool), pool);
    Num_Out_Calls_Processed = CXX_NEW (NUM_CALLS_PROCESSED (cg, pool), pool);

    // For the top-level nodes, there is a edge from the dummy root node
    // that we will never processed.  So we increment the Calls_Processed
    // count first so that All_Calls_Processed() will return the correct
    // result. 
    IPA_GRAPH* graph = cg->Graph ();
    NODE_ITER viter (graph, cg->Root ());
    for (NODE_INDEX child = viter.First_Succ ();
	 child != -1;
	 child = viter.Next_Succ ())
	++(*Num_In_Calls_Processed)[graph->Node_User (child)];

} // Init_Num_Calls_Processed

// check if a PU has been deleted
static BOOL
PU_Deleted (const IPA_GRAPH* cg, NODE_INDEX idx, const IP_FILE_HDR* fhdr)
{
    const IPA_NODE* node = cg->Node_User (idx);

    if (node == NULL)
	return TRUE;

#ifdef KEY
    // For builtins, Proc_Info_Index returns -1.  proc_info[-1] is illegal.
    if (node->Is_Builtin())
      return FALSE;
#endif

    const IP_PROC_INFO* proc_info = IP_FILE_HDR_proc_info (*fhdr);

    return IP_PROC_INFO_state (proc_info[node->Proc_Info_Index ()]) ==
	IPA_DELETED;
} // PU_Deleted
    
#ifdef KEY
static BOOL
PU_Written (const IPA_GRAPH* cg, const IPA_NODE* node, const IP_FILE_HDR* fhdr)
{
    if (node == NULL)
	return TRUE;

    const IP_PROC_INFO* proc_info = IP_FILE_HDR_proc_info (*fhdr);

    return IP_PROC_INFO_state (proc_info[node->Proc_Info_Index ()]) ==
	IPA_WRITTEN;
} // PU_Written
#endif // KEY

// this is basically a post-order iteration of the call graph, with the
// exception that any nested procedure is always processed before its
// parent, even when the parent does not directly call it (and thus not
// represented in the call graph).
typedef vector<IPA_NODE*> IPA_NODE_VECTOR;

static void
Trans_Order_Walk (IPA_NODE_VECTOR& vect, mBOOL* visited, IPA_GRAPH* cg,
		  NODE_INDEX root) 
{
    visited[root] = TRUE;
    NODE_ITER vitr (cg, root);
    NODE_INDEX child;

    for (child = vitr.First_Succ (); 
         child != -1;
	 child = vitr.Next_Succ ())
	if (!visited[child])
	    Trans_Order_Walk (vect, visited, cg, child);

    IPA_NODE* node = cg->Node_User (root);

    if (node == NULL)
	return;

#ifdef KEY
    if (node->Is_Builtin())
      return;
#endif

    if (node->Summary_Proc ()->Is_alt_entry ()) {
	IPA_SUCC_ITER succ_iter (node);
	for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {
	    IPA_EDGE *edge = succ_iter.Current_Edge ();
	    if (!edge->Is_Processed ()) {
		IPA_NODE* callee = IPA_Call_Graph->Callee (edge);
		++(*Num_In_Calls_Processed)[callee];
		++(*Num_Out_Calls_Processed)[node];
		edge->Set_Processed ();
	    }
	}
    } else if (PU_uplevel (node->Get_PU ())) {
	// if this pu contains nested children, make sure if they have all
	// been processed
	const PU_Info* pu = node->PU_Info ();
	for (pu = PU_Info_child (pu); pu; pu = PU_Info_next (pu)) {
	    const AUX_PU& aux_pu =
		Aux_Pu_Table [ST_pu (St_Table [PU_Info_proc_sym (pu)])];
	    child = AUX_PU_node (aux_pu);
	    if (!visited[child] &&
		!PU_Deleted (cg, child, AUX_PU_file_hdr (aux_pu)))
		Trans_Order_Walk (vect, visited, cg, child);
	}
    }

    vect.push_back (node);
} // Trans_Order_Walk 

#if defined(TARG_SL)
typedef queue<NODE_INDEX> NODE_INDEX_QUEUE;

// A breadth-first search of the call graph.
static void
Build_BFS_Order (IPA_NODE_VECTOR& vect, IPA_GRAPH* cg,
		  NODE_INDEX root) 
{
    mBOOL *visited = (mBOOL *) alloca (GRAPH_vmax (cg) * sizeof(mBOOL));
    bzero (visited, GRAPH_vmax (cg) * sizeof(mBOOL));
    vect.reserve (GRAPH_vcnt (cg));

    NODE_INDEX_QUEUE myqueue;
    myqueue.push(root);

    while (!myqueue.empty()) {

        NODE_INDEX cur = myqueue.front();
        myqueue.pop();

        if (!visited[cur]) {
            visited[cur] = TRUE;
            NODE_ITER vitr (cg, cur);
            NODE_INDEX child;
            for (child = vitr.First_Succ (); child != -1;
            	 child = vitr.Next_Succ ())
            {
	            if (!visited[child])
                    myqueue.push(child); 
            }
           
            IPA_NODE* node = cg->Node_User (cur);
            if (node == NULL)
                continue;
#ifdef KEY
            if (node->Is_Builtin())
                continue;
#endif
            //printf("BFS - Pushing node (%d): %s\n", node->Node_Index(), IPA_Node_Name(node));
            vect.push_back(node);
        }

    } // while
}
#endif // TARG_SL

static inline void
Build_Transformation_Order (IPA_NODE_VECTOR& vect, IPA_GRAPH* cg,
			    NODE_INDEX root)
{
    mBOOL *visited = (mBOOL *) alloca (GRAPH_vmax (cg) * sizeof(mBOOL));
    bzero (visited, GRAPH_vmax (cg) * sizeof(mBOOL));
    vect.reserve (GRAPH_vcnt (cg));
    Trans_Order_Walk (vect, visited, cg, root);

} // Build_Transformation_Order



/* rename the callsite to point to the cloned procedure */
void
Rename_Call_To_Cloned_PU (IPA_NODE *caller, 
                          IPA_NODE *callee,
			  IPA_EDGE *e, 
                          IPA_CALL_GRAPH *cg)
{
  IPA_NODE_CONTEXT context (caller);	// switch to the caller context

  cg->Map_Callsites (caller);           // map callsites to WN nodes

  WN* call = e->Whirl_Node();

  WN_st_idx (call) = ST_st_idx (callee->Func_ST()); 

} // Rename_Call_To_Cloned_PU

#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
static void Fixup_EHinfo_In_PU (IPA_NODE* node, WN * w = NULL)
{
  if (w && WN_operator(w) == OPR_REGION && WN_region_is_EH (w) &&
      WN_block_empty (WN_region_pragmas (w)))
  {
    int sym_size;
    SUMMARY_SYMBOL * sym_array = IPA_get_symbol_file_array (node->File_Header(), sym_size);
    Is_True (sym_array != NULL, ("Missing SUMMARY_SYMBOL section"));
    INITV_IDX blk = INITO_val (WN_ereg_supp (w));
    // ipl may create multiple copies of the same region, so keep track if
    // a region has been updated
    if (INITV_flags (Initv_Table[blk]) != INITVFLAGS_UPDATED)
    {
      Set_INITV_flags (blk, INITVFLAGS_UPDATED);
      INITV_IDX types = INITV_next (INITV_blk (blk));
      for (; types; types = INITV_next (types))
      {
        if (INITV_kind (types) != INITVKIND_VAL)
          continue;
        int index = TCON_uval (INITV_tc_val (types));
        if (index <= 0) continue;
        ST_IDX new_idx = sym_array[index].St_idx();
        INITV_IDX next = INITV_next (types);        // for backup
        INITV_Set_VAL (Initv_Table[types], Enter_tcon (
                       Host_To_Targ (MTYPE_U4, new_idx)), 1);
        Set_INITV_next (types, next);
      }
    }
  }

  if (w == NULL)
    w = node->Whirl_Tree (FALSE);

  if (!OPCODE_is_leaf (WN_opcode (w)))
  {
    if (WN_operator (w) == OPR_BLOCK)
    {
      WN * kid = WN_first (w);
      while (kid)
      {
        Fixup_EHinfo_In_PU (node, kid);
	kid = WN_next (kid);
      }
    }
    else
    {
      for (INT kidno=0; kidno<WN_kid_count(w); ++kidno)
        Fixup_EHinfo_In_PU (node, WN_kid(w, kidno));
    }
  }
}
#endif

static BOOL
Inline_Call (IPA_NODE *caller, IPA_NODE *callee, IPA_EDGE *edge,
	     IPA_CALL_GRAPH *cg)
{
    Is_True (caller->Is_Processed () && callee->Is_Processed (),
	     ("Node info not read before performing inlining"));

    IPA_NODE_CONTEXT context (caller);
    cg->Map_Callsites (caller);

    if (!Can_Inline_Call (caller, callee, edge))
	return FALSE;

#ifdef KEY
    Get_enclosing_region (caller, edge);
#if !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    // For C++, fix-up the summarized information in EH regions now, because
    // after inlining we won't know from which file each symbol came, making
    // it impossible to replace summary.
    if (!caller->EHinfo_Updated())
    {
      PU p = caller->Get_PU();
      if ((PU_src_lang (p) & PU_CXX_LANG) && PU_has_region (p))
        Fixup_EHinfo_In_PU (caller);
      caller->Set_EHinfo_Updated();
    }
    if (!callee->EHinfo_Updated())
    {
      PU p = callee->Get_PU();
      if ((PU_src_lang (p) & PU_CXX_LANG) && PU_has_region (p))
      {
        IPA_NODE_CONTEXT temp_context (callee);
        Fixup_EHinfo_In_PU (callee);
      }
      callee->Set_EHinfo_Updated();
    }
#endif // !_STANDALONE_INLINER && !_LIGHTWEIGHT_INLINER
#endif // KEY

#if Is_True_On
    if ( Get_Trace ( TKIND_ALLOC, TP_IPA) ) {
	fprintf ( TFile, "\n%s%s\tMemory allocation information before inlining\n%s%s\n",
		  DBar, DBar, DBar, DBar );
	MEM_Trace ();
    }
#endif

    IPO_INLINE ip_inline (caller, callee, edge); 
    ip_inline.Process ();

#if Is_True_On
    if ( Get_Trace ( TKIND_ALLOC, TP_IPA) ) {
	fprintf ( TFile, "\n%s%s\tMemory allocation information after inlining\n%s%s\n",
		  DBar, DBar, DBar, DBar );
	MEM_Trace ();
    }
#endif

    if ( Trace_IPA || Trace_Perf ) {
        if (edge->Has_Partial_Inline_Attrib()) {
           fprintf ( TFile, "%s partially inlined into ", DEMANGLE (callee->Name()) );
        } else {
           fprintf ( TFile, "%s inlined into ", DEMANGLE (callee->Name()) );
        }
	fprintf ( TFile, "%s (edge# %d)", DEMANGLE (caller->Name()), edge->Edge_Index () );
	if ( IPA_Skip_Report ) {
	    fprintf ( TFile, " (%d)\n", caller->Node_Index() );
	} else {
	    fprintf ( TFile, "\n" );
	}
    }
    if ( INLINE_List_Actions ) {
        if (edge->Has_Partial_Inline_Attrib()) {
           fprintf ( stderr, "%s partially inlined into ", DEMANGLE (callee->Name()) );
        } else {
           fprintf ( stderr, "%s inlined into ", DEMANGLE (callee->Name()) );
        }
	fprintf ( stderr, "%s (edge# %d)\n", DEMANGLE (caller->Name()), edge->Edge_Index () );
	if ( IPA_Skip_Report ) {
	    fprintf ( stderr, " (%d)\n", caller->Node_Index() );
	} else {
	    fprintf ( stderr, "\n" );
	}

	// Generate an inlining action log for verifying purpose
	// The file open/close operation could be placed in higher level to reduce time cost	
#ifdef Enable_ISP_Verify
	FILE *inline_action = fopen(inline_action_log, "a+");
	char *caller_key, *callee_key;
    	
	// Retrieve call site source line number		
	WN* call_wn = edge->Whirl_Node();
	USRCPOS callsite_srcpos;
	USRCPOS_srcpos(callsite_srcpos) = WN_Get_Linenum (call_wn);
	char callsite_linestr[1024];
	sprintf(callsite_linestr, "%d", USRCPOS_linenum(callsite_srcpos));
	
	// Retrieve caller/callee function names and the definition file names
	char *caller_file = (char *) alloca(strlen(caller->File_Header().file_name)+1);
	strcpy(caller_file, caller->File_Header().file_name);
	char *callee_file = (char *) alloca(strlen(callee->File_Header().file_name)+1);
	strcpy(callee_file, callee->File_Header().file_name);
	char *caller_func = (char *) alloca(strlen(DEMANGLE (caller->Name()))+1);
	strcpy(caller_func, DEMANGLE (caller->Name()));
	char *callee_func = (char *) alloca(strlen(DEMANGLE (callee->Name()))+1);
	strcpy(callee_func, DEMANGLE (callee->Name()));	
	
	// Filter out surffix in file/function names	
	ISP_Fix_Filename(caller_file);
	ISP_Fix_Filename(callee_file);
	ISP_Fix_Filename(caller_func);
	ISP_Fix_Filename(callee_func);
	
	// Assemble the caller key for inquiry into inlining record
	caller_key = (char *) alloca(strlen(caller_file)+strlen(caller_func)+2);
	strcpy(caller_key, "");
 	strcat(caller_key, caller_file);
  	strcat(caller_key, caller_func);	
  	
  	// Assemble the callee key for inquiry into inlining record
  	callee_key = (char *) alloca(strlen(callsite_linestr)+strlen(callee_file)+strlen(callee_func)+3);
   	strcpy(callee_key, "");
   	strcat(callee_key, callsite_linestr);
   	strcat(callee_key, callee_file);
	strcat(callee_key, callee_func);	
	
        if (edge->Has_Partial_Inline_Attrib()) {
           fprintf(inline_action, "[%s] partially inlined into [%s]\n", callee_key, caller_key);
        } else {
           fprintf(inline_action, "[%s] inlined into [%s]\n", callee_key, caller_key);
        }
	fclose(inline_action);
#endif
    }

    return TRUE;

} // Inline_Call

#ifdef KEY
extern void IPO_Process_Virtual_Functions (IPA_NODE *);
extern void IPO_Process_Icalls (IPA_NODE *);
extern void IPA_update_ehinfo_in_pu (IPA_NODE *);
#endif

static IPA_NODE *
IPO_Process_node (IPA_NODE* node, IPA_CALL_GRAPH* cg)
{
  if (Is_Skip_Equal(node->Name())) {
    if ( Trace_IPA || Trace_Perf ) 
	fprintf ( TFile, "%s is skipped \n", DEMANGLE (node->Name()) );
    return node;
  }

  if (node->Summary_Proc()->Is_alt_entry ()) {
    return node;
  }
  
  IP_READ_pu_infos (node->File_Header());

  IPA_NODE_CONTEXT context (node);	// switch to this node's context

  // Map WN to its new unique CGNodeId for the Nystrom alias analyzer
  if (Alias_Nystrom_Analyzer) {
    IPA_NystromAliasAnalyzer::aliasAnalyzer()->updateLocalSyms(node);
    IPA_NystromAliasAnalyzer::aliasAnalyzer()->
                              mapWNToUniqCallSiteCGNodeId(node);
  }

#ifdef KEY
  if (PU_src_lang (node->Get_PU()) & PU_CXX_LANG)
    IPA_update_ehinfo_in_pu (node);

  if (IPA_Enable_Icall_Opt && node->Has_Pending_Icalls()) {
    IPO_Process_Icalls (node);
  }
#endif
  if (IPA_Enable_Fast_Static_Analysis_VF && 
      node->Has_Pending_Virtual_Functions()) {
    IPO_Process_Virtual_Functions (node);
  }


  if (IPA_Enable_Padding) {
    IPO_Pad_Whirl (node);
  }

  if (IPA_Enable_Common_Const && node->Has_Propagated_Const()) {
    IPO_propagate_globals(node);
  }
  
#ifdef KEY
  if (IPA_Enable_Struct_Opt)
    IPO_WN_Update_For_Struct_Opt(node);
#endif

  if (OPT_Struct_Array_Copy >= 2)
    IPO_Identify_Single_Define_To_HeapAlloced_GlobalVar(
                                              WN_func_body(node->Whirl_Tree()));

  if(IPA_Enable_Reorder && reorder_candidate.size)
    IPO_Modify_WN_for_field_reorder(node) ;
  else // just for debug  field reorder
    Compare_whirl_tree(node);

  if (
#if defined(TARG_SL)
      !ld_ipa_opt[LD_IPA_IPISR].flag && 
#endif
      IPA_Enable_Cloning && node->Is_Clone_Candidate()) {

    IPA_NODE *cloned_node = cg->Create_Clone(node);
    cloned_node->Set_Propagated_Const ();

    if (Trace_IPA || Trace_Perf) {
      fprintf (TFile, "%s from file %s is cloned\n",
               DEMANGLE (node->Name()), node->Input_File_Name());
    }

    /* write out the original */
    node->Write_PU();
    
    node = cloned_node;
  }
    
  if (IPA_Enable_Cprop && node->Has_Propagated_Const()) {

      BOOL is_fortran = PU_f77_lang(node->Get_PU()) ||
	  PU_f90_lang(node->Get_PU());

      IPA_Propagate_Constants (node, (IPA_Enable_Cprop2 &&
				      !is_fortran &&
				      !node->Has_No_Aggr_Cprop()));
	
  }

  return node;
} // IPO_Process_node


static void
IPO_Process_edge (IPA_NODE* caller, IPA_NODE* callee, IPA_EDGE* edge,
		  IPA_CALL_GRAPH* cg)
{
    if (caller->Summary_Proc()->Is_alt_entry ()) {
	return;
    }

    /* In the analysis phase, we've already marked those edges not being
       inlined or dce'd as "processed", and incremented the succ_count and
       pred_count accordingly. */ 
	    
    BOOL action_taken = FALSE;

#ifdef TODO
    if (IPA_Enable_Array_Sections) {
	if (callee->Has_use_kill()) {
	    Mark_use_kill_param(caller, callee, edge);
	}
    }
#endif // TODO

    if (IPA_Enable_Addressing && IPA_Enable_Simple_Alias &&
	IPA_Enable_Readonly_Ref) {
	// must be called before constant parameters are removed, otherwise
	// the parameter positions might be wrong.
	if (edge->Has_Readonly_Param() || edge->Has_Pass_Not_Saved_Param())
	    Mark_readonly_param (caller, edge, cg);
    }

    if (edge->Has_Propagated_Const() && callee->Has_Propagated_Const()) {
      if (IPA_Enable_Cloning && callee->Is_Clone()) {
        Rename_Call_To_Cloned_PU (caller, callee, edge, cg);
      }
      if (IPA_Enable_Cprop2 && !callee->Has_No_Aggr_Cprop())
        Reset_param_list (caller, callee, edge, cg);
    }

    if (IPA_Enable_DCE && edge->Is_Deletable ())
	action_taken = Delete_Call (caller, callee, edge, cg);
    else if (IPA_Enable_Inline && edge->Has_Inline_Attrib () &&
		!callee->Has_Noinline_Attrib()) {
#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
      if (caller != callee) {
#endif
	MEM_POOL_Popper ipo_pool (&Ipo_mem_pool);
	action_taken = Inline_Call (caller, callee, edge, cg);
	if (action_taken) 
	    IPO_Total_Inlined++;
#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
      } else {
	// It is a recursive call, we cannot inline it until the node and all
	// its edges have been processed.
      	inline_info inline_later (caller, edge);
	inline_list.push_back (inline_later);
	return;
      }
#endif
    }
    
#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    static INT pure_call_opt_count = 0;
    if (!action_taken &&
         IPA_Enable_Pure_Call_Opt && 
	 !callee->Summary_Proc()->Has_side_effect()) 
    {
      IPA_NODE_CONTEXT context (caller);

      if (!edge->Whirl_Node())
        cg->Map_Callsites (caller);

      WN * ret_val, * ret_val_kid;

      WN * call = edge->Whirl_Node();
      ret_val = WN_next(call);

      if (ret_val)
      {
        ret_val_kid = WN_kid0 (ret_val);
	BOOL types_ok = WN_rtype (call) != MTYPE_M &&
	                WN_desc (call) == MTYPE_V;
        BOOL transform = (ret_val_kid && WN_operator (ret_val) == OPR_STID &&
            WN_operator (ret_val_kid) == OPR_LDID &&
	    WN_load_offset (ret_val_kid) == -1 &&
	    types_ok);
	if (transform) pure_call_opt_count++;
	if (transform &&
	    pure_call_opt_count > IPA_Pure_Call_skip_before)
	{ // actually do the transformation
	  WN * new_call = WN_Create (OPR_PURE_CALL_OP,
	                             WN_desc (ret_val), //WN_rtype (call)
				     MTYPE_V, // WN_desc (call)
				     WN_kid_count (call));
	  WN_st_idx (new_call) = WN_st_idx (call);
	  for (int i=0; i<WN_kid_count(call); ++i)
	    WN_kid(new_call,i) = WN_kid(call,i);
	  
	  IPA_WN_Delete (caller->Map_Table(), ret_val_kid);

	  WN_kid (ret_val, 0) = new_call;

	  // delete the call wn
	  if (WN_prev (call))
	    WN_next (WN_prev (call)) = ret_val;
	  else // find the parent block
	  {
	    WN * block = 
	      WN_Get_Parent (call, caller->Parent_Map(), caller->Map_Table());

	    Is_True (block, ("NULL parent of call node"));
	    Is_True (WN_operator(block) == OPR_BLOCK, 
	             ("Parent of stmt not a block"));

	    WN_first (block) = ret_val;
	  }
	  WN_prev (ret_val) = WN_prev (call);

	  IPA_WN_Delete (caller->Map_Table(), call);
	  // delete the edge
	  cg->Graph()->Delete_Edge (edge->Edge_Index());
	  return;
	}
      }
    }
#endif

    if (!action_taken) {
	/* when the type checking for dce and inlining is
	   moved to the analysis phase, we can optimize the
	   following to be done only if no dce and
	   inlining is done. */
	callee->Set_Undeletable();

//#ifdef TODO
	if (IPA_Enable_Cord) {
	    fprintf (Call_graph_file, "%s\t%s\t%f\t%f\t%f\n", caller->Name (),
		     callee->Name (), edge->Has_frequency () ?
#ifdef KEY
		     (edge->Get_frequency()).Value() : 0,
		     (caller->Get_frequency()).Value(),
		     (callee->Get_frequency()).Value()
#else
		     (edge->Get_frequency())._value : 0,
		     (caller->Get_frequency())._value,
		     (callee->Get_frequency())._value
#endif
		     );
	}
//#endif
    }
    

    ++(*Num_In_Calls_Processed)[callee];
    ++(*Num_Out_Calls_Processed)[caller];
#ifdef _DEBUG_CALL_GRAPH
    printf("Processed %s   -->   %s\n", caller->Name(), callee->Name());
#endif // _DEBUG_CALL_GRAPH
    edge->Set_Processed ();
    
} // IPO_Process_edge

extern void WN_free_input (void *handle, off_t mapped_size);

static inline void
Delete_Proc (IPA_NODE *node)
{
    Set_ST_is_not_used (node->Func_ST ());
    Delete_Function_In_File (node->File_Header(), node->Proc_Info_Index ());
    node->Un_Read_PU();
    /* Free the mmaped memory if all PUs in the file are released */
    if (node->File_Header().num_written == IP_FILE_HDR_num_procs(node->File_Header()))
      WN_free_input(IP_FILE_HDR_input_map_addr(node->File_Header()), node->File_Header().mapped_size);

} // Delete_Proc

// check whether 'st' has been seen in the array 'Global_Sts'.
// If so, return the index.
static INT Find_Global_As_Local_Ndx(ST * st)
{
  INT st_ndx = -1;
  for (int i = 0; i <= Global_As_Local_Ndx; i++) {
    if (Global_Sts[i] == st) {
      st_ndx = i;
      break;
    }
  }
  return st_ndx;
}

// Find containing store statement for 'wn'.
static WN * Find_Containing_Stmt(WN * wn)
{
  WN * wn_iter = wn;
  while (wn_iter) {
    OPERATOR opr = WN_operator(wn_iter);
    if (OPERATOR_is_store(opr))
      return wn_iter;
    wn_iter = (WN *) WN_MAP_Get(PU_Parent_Map, wn_iter);
  }
  return NULL;
}

// Obtain outermost nesting loop for 'wn'.
static WN * Outermost_Do_Loop(WN * wn)
{
  WN * wn_iter = wn;
  WN * outermost = NULL;

  while (wn_iter) {
    if (WN_operator(wn_iter) == OPR_DO_LOOP)
      outermost = wn_iter;
    wn_iter = (WN *) WN_MAP_Get(PU_Parent_Map, wn_iter);
  }

  return outermost;
}

// Check whether there exists a OPR_CALL or OPR_GOTO in 'wn'.
static BOOL Has_Call_Or_Goto(WN * wn)
{
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_DO_LOOP) {
    UINT64 val = Global_As_Local_Hash->Find((void *) wn);
    if (val > 0)
      return FALSE;
  }

  if ((opr == OPR_CALL) || (opr == OPR_GOTO))
    return TRUE;
  else if (opr == OPR_BLOCK) {
    for (WN * wn_iter = WN_first(wn); wn_iter; wn_iter = WN_next(wn_iter)) {
      if (Has_Call_Or_Goto(wn_iter))
	return TRUE;
    }
  }
  else {
    for (int i = 0; i < WN_kid_count(wn); i++) {
      if (Has_Call_Or_Goto(WN_kid(wn, i)))
	return TRUE;
    }
  }

  if (opr == OPR_DO_LOOP)
    Global_As_Local_Hash->Enter((void *) wn, 1);

  return FALSE;
}

// Categorize properties of 'def":
// "INNERMOST_DOM": 'def' is unconditionally evaluated in a loop-nest that is an
// immediate child of 'FUNC_ENTRY', the loop-nest is perfectly-nested and contains
// no calls and gotos.
// "NONE": default value.
static DEF_FLAG Get_Def_Flag(WN * def) 
{
  DEF_FLAG flag = NONE;

  WN * outermost = Outermost_Do_Loop(def);
  if (!outermost)
    return flag;

  WN * wn_p = (WN *) WN_MAP_Get(PU_Parent_Map, outermost);
  if (WN_operator(wn_p) != OPR_BLOCK)
    return flag;

  wn_p = (WN *) WN_MAP_Get(PU_Parent_Map, wn_p);
  if (WN_operator(wn_p) != OPR_FUNC_ENTRY)
    return flag;
  
  WN * def_p = (WN *) WN_MAP_Get(PU_Parent_Map, def);
  while (def_p && (def_p != outermost)) {
    OPERATOR opr = WN_operator(def_p);
    
    if (opr == OPR_DO_LOOP) {
      if (WN_next(def_p) || WN_prev(def_p))
	return flag;
    }
    else if (opr != OPR_BLOCK)
      return flag;

    def_p = (WN *) WN_MAP_Get(PU_Parent_Map, def_p);
  }
  
  if (Has_Call_Or_Goto(outermost))
    return flag;
  
  return INNERMOST_DOM;
}

// Check whether the evaluation of 'def' dominates the evaluation of 'use'.
// This includes dominance in both control flow and iteration space.
static BOOL Dominate(WN * def, WN * use, INT ndx)
{
  WN * use_p = Find_Containing_Stmt(use);

  if (!use_p)
    return FALSE;

  WN * def_p = (WN *) WN_MAP_Get(PU_Parent_Map, def);
  use_p = (WN *) WN_MAP_Get(PU_Parent_Map, use_p);
  BOOL same_scope = FALSE;

  while (use_p) {
    if (def_p == use_p) {
      same_scope = TRUE;
      break;
    }
    use_p = (WN *) WN_MAP_Get(PU_Parent_Map, use_p);
  }

  switch (Global_Flags[ndx]) {
  case INNERMOST_DOM:
    break;
  default:
    if (!same_scope)
      return FALSE;
    ;
  }

  // Check whether use's iteration space is a subset of def's iteration space.
  WN * array_def = WN_kid(def, 1);
  WN * array_use = WN_kid(use, 0);
  int dim = WN_num_dim(array_def);

  for (int i = 0; i < dim; i++) {
    WN * index1 = WN_array_index(array_def, i);
    WN * index2 = WN_array_index(array_use, i);
    OPERATOR opr1 = WN_operator(index1);
    OPERATOR opr2 = WN_operator(index2);

    if ((opr1 != opr2) || (opr1 != OPR_SUB))
      return FALSE;

    index1 = WN_kid0(index1);
    index2 = WN_kid0(index2);
    opr1 = WN_operator(index1);
    opr2 = WN_operator(index2);

    if (opr1 == OPR_CVT) 
      index1 = WN_kid0(index1);

    if (opr2 == OPR_CVT)
      index2 = WN_kid0(index2);

    ST * st1 = NULL;
    ST * st2 = NULL;

    if (WN_operator(index1) == OPR_LDID)
      st1 = WN_st(index1);

    if (WN_operator(index2) == OPR_LDID)
      st2 = WN_st(index2);

    WN * loop1 = WN_find_loop_by_index(array_def, st1, PU_Parent_Map);

    if (!loop1)
      return FALSE;

    WN * loop2 = WN_find_loop_by_index(array_use, st2, PU_Parent_Map);

    if (loop2) {
      if (loop1 != loop2) {
	int lower_diff = 0;
	int upper_diff = 0;
	if (!WN_has_compatible_iter_space(loop1, loop2, &lower_diff, &upper_diff, FALSE)
	    || (lower_diff > 0) 
	    || (upper_diff < 0))
	  return FALSE;
      }
    }
    else if (WN_operator(index2) != OPR_INTCONST) 
      return FALSE;
    else {
      int cval = WN_const_val(index2);
      OPCODE ub_compare;
      WN * lb = WN_LOOP_LowerBound(loop1);
      WN * ub = WN_LOOP_UpperBound(loop1, &ub_compare, TRUE);
      OPERATOR opr = OPCODE_operator(ub_compare);

      if (((opr == OPR_LE) || (opr == OPR_LT))
	  && lb && ub
	  && (WN_operator(lb) == OPR_INTCONST)
	  && (WN_operator(ub) == OPR_INTCONST)
	  && (cval >= WN_const_val(lb))
	  && ((cval < WN_const_val(ub)) || ((cval == WN_const_val(ub)) && (opr == OPR_LE)))) {
      }
      else
	return FALSE;
    }
  }

  return TRUE;
}

// If 'wn1' and 'wn2' has a common parent that is a if-statement and
// 'wn1' and 'wn2' locate in the then-path and the else-path separately,
// and every node on the path between the common parent and the two nodes
// is either a block or a do-loop with the same iteration space, return 
// the common parent.
static WN * Get_Common_If(WN * wn1, WN * wn2)
{
  WN * wn_iter1 = (WN *) WN_MAP_Get(PU_Parent_Map, wn1);
  WN * wn_iter2 = (WN *) WN_MAP_Get(PU_Parent_Map, wn2);
  int lower_diff = 0;
  int upper_diff = 0;

  while (wn_iter1 && wn_iter2) {
    OPERATOR opr1 = WN_operator(wn_iter1);
    OPERATOR opr2 = WN_operator(wn_iter2);
    
    if (opr1 != opr2)
      break;
    
    switch (opr1) {
    case OPR_BLOCK:
      break;
    case OPR_DO_LOOP:
      if (!WN_has_compatible_iter_space(wn_iter1, wn_iter2, 
					&lower_diff, &upper_diff, TRUE)
	  || (lower_diff != 0)
	  || (upper_diff != 0)) {
	return NULL;
      }
      break;
    case OPR_IF:
      if (wn_iter1 == wn_iter2) 
	return wn_iter1;

    default:
      return NULL;
    }

    wn_iter1 = (WN *) WN_MAP_Get(PU_Parent_Map, wn_iter1);
    wn_iter2 = (WN *) WN_MAP_Get(PU_Parent_Map, wn_iter2);
  }
  
  return NULL;
}

// Legality check for the global-as-local candidate 'wn'
// whose base symbol is 'st'.
static void Enter_Global_As_Local(WN * wn , ST * st)
{
  if (!st || !ST_is_global_as_local(st))
    return;

  int st_ndx = Find_Global_As_Local_Ndx(st);
  if (st_ndx < 0)
    return;

  if (ST_export(st) == EXPORT_PREEMPTIBLE) {
    Clear_ST_is_global_as_local(st);
    return;
  }

  OPERATOR opr = WN_operator(wn);
  
  if (opr == OPR_LDA) {
    // Address-taken.
    Clear_ST_is_global_as_local(st);
  }

  WN * def = Global_Defs[st_ndx];

  if (OPERATOR_is_store(opr)) {
    if (def) {
      WN * array1 = WN_kid1(def);
      WN * array2 = WN_kid1(wn);
      BOOL do_clear = TRUE;
      if (WN_Simp_Compare_Trees(array1, array2) == 0) {
	WN * wn_if = Get_Common_If(def, wn);
	if (wn_if) {
	  // Allow defs in the then-path and the else-path of a if-statement
	  // if the if-statement is at a dominating point in the innermost loop.
	  WN * parent = (WN *) WN_MAP_Get(PU_Parent_Map, wn_if);
	  DEF_FLAG flag = Get_Def_Flag(parent);
	  if (flag == INNERMOST_DOM) {
	    Global_Flags[st_ndx] = flag;
	    do_clear = FALSE;
	  }
	}
      }
      if (do_clear)
	Clear_ST_is_global_as_local(st);
    }
    else {
      // Is a first-def.
      Global_Defs[st_ndx] = wn;
      Global_Flags[st_ndx] = Get_Def_Flag(wn);
    }
  }
  else {
    if (def) {
      if (!Dominate(def, wn, st_ndx)) {
	Clear_ST_is_global_as_local(st);
      }
    }
    else {
      // Is a upward-exposed use.
      Clear_ST_is_global_as_local(st);
    }
  }
}

// Traverse 'tree' to do legality check for global-as-local candidates.
// The candidate's use must not be upward-exposed, i.e., every use has 
// a dominating reaching def in the same function.
// Notice that the logic here applies to Fortran programs only, and we
// only tracks initialized global data without equivalences.
static void Traverse_Tree_For_Global_As_Local(WN * tree)
{
  OPERATOR opr = WN_operator(tree);
  ST * st;
  char * name;
  WN * wn;
  int ndx;

  if (opr == OPR_BLOCK) {
    for (WN * wn_iter = WN_first(tree); wn_iter; wn_iter = WN_next(wn_iter)) {
      Traverse_Tree_For_Global_As_Local(wn_iter);
    }
  }
  else {
    switch (opr) {
    case OPR_LDA:
      st = WN_st(tree);
      Enter_Global_As_Local(tree, st);
      break;

    case OPR_CALL:
      st = WN_st(tree);
      if (st) {
	name = ST_name(st);
	if ((strcmp(name, "_DEALLOCATE") == 0)
	    || (strcmp(name, "_DEALLOC") == 0)
	    || (strcmp(name, "_F90_ALLOCATE_B") == 0)
	    || (strcmp(name, "_SIZE_4") == 0)) {
	  return;
	}
      }

      break;

    case OPR_ISTORE:
      // track store data first.
      wn = WN_kid(tree, 0);
      Traverse_Tree_For_Global_As_Local(wn);

      wn = WN_kid(tree, 1);
      if (WN_operator(wn) == OPR_ARRAY) {	
	WN * base = WN_array_base(wn);
	if (base && WN_has_sym(base)) {
	  Enter_Global_As_Local(tree, WN_st(base));
	}
      }

      break;

    case OPR_ILOAD:
      wn = WN_kid(tree, 0);
      if (WN_operator(wn) == OPR_ARRAY) {
	WN * base = WN_array_base(wn);
	if (base && WN_has_sym(base)) {
	  Enter_Global_As_Local(tree, WN_st(base));
	}
      }
      break;

    default:
      ;
    }

    for (INT kidno = 0; kidno < WN_kid_count(tree); kidno++) {
      wn = WN_kid(tree, kidno);
      Traverse_Tree_For_Global_As_Local(wn);
    }
  }
}

// Identify global-as-local candidates for 'node'.
static void
Identify_Global_As_Local_Candidates(IPA_NODE * node)
{
  if (!IPA_Enable_Global_As_Local || (Global_As_Local_Ndx < 0))
    return;

  if (!node->PU_Info())
    return;

  IPA_NODE_CONTEXT context (node); 
  WN * tree = node->Whirl_Tree(FALSE);
  if (!tree)
    return;
  
  for (int i = 0; i <= Global_As_Local_Ndx; i++) {
    Global_Defs[i] = NULL;
    Global_Flags[i] = NONE;
  }

  PU_Parent_Map = node->Parent_Map();
  PU_Map_Tab = node->Map_Table();
  WN_Parentize(tree, PU_Parent_Map, PU_Map_Tab);
  MEM_POOL_Initialize(&Temp_pool, "global-as-local temp pool", FALSE);
  MEM_POOL_Push(&Temp_pool);
  Global_As_Local_Hash = CXX_NEW(GLOBAL_AS_LOCAL_HASH(100, &Temp_pool), &Temp_pool);
  Traverse_Tree_For_Global_As_Local(WN_func_body(tree));
  MEM_POOL_Pop(&Temp_pool);
}

static void
Perform_Transformation (IPA_NODE* caller, IPA_CALL_GRAPH* cg)
{

    Is_True (caller != NULL, ("Invalid IPA_NODE"));
	
#ifdef TODO
    // Only process those nodes that has the same partition group
    // as "partition_num" if files need to be unmapped DUE to
    // space problem.  This is assuming if IPA encounters the
    // space problem, it would have partitioned the call-graph
    // so that all functions in the SAME file have the SAME 
    // partition group.

    if (IPA_Space_Access_Mode == SAVE_SPACE_MODE &&
	caller->Get_partition_group () != partition_num)
	return;

    if ( IPA_Enable_Recycle ) {
	MEM_POOL_Push (&Recycle_mem_pool);
    }
#endif

    Init_inline(); // opcode related and dst initialization
	
    if (!caller->Is_Processed ()) {
	caller = IPO_Process_node (caller, cg);
	caller->Set_Processed ();
    }

    IPA_SUCC_ITER succ_iter (caller);
    for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) { 

	IPA_EDGE *edge = succ_iter.Current_Edge ();
	IPA_NODE *callee = cg->Callee (edge);
	    
#ifdef KEY
	if (caller->Is_Recursive())
	    callee->Set_Undeletable();
#endif

#ifdef _DEBUG_CALL_GRAPH
        printf("%s   ---->    %s\n", caller->Name(), callee->Name());
#endif // _DEBUG_CALL_GRAPH

	// turn off cloning for recursive edges	
	if (cg->Graph()->Is_Recursive_Edge(edge->Edge_Index()))
	    callee->Clear_Clone_Candidate();

#ifdef TODO
	if (!callee->Is_Processed () && // (!IPA_Enable_SP_Partition ||
	    ((IPA_Space_Access_Mode != SAVE_SPACE_MODE) || 
	     (callee->Get_partition_group () == COMMON_PARTITION) || 
	     (callee->Get_partition_group () == partition_num))) {
	    callee = IPO_Process_node (callee, cg);
	    callee->Set_Processed ();
	}
#else
	if (!callee->Is_Processed ()) {
	    callee = IPO_Process_node (callee, cg);
	    callee->Set_Processed ();
	}
#endif // TODO
	    
	    
	if (!edge->Is_Processed ())
	    IPO_Process_edge (caller, callee, edge, cg);

	if (caller == callee)
	    continue;
	    
#ifdef TODO
	if (IPA_Space_Access_Mode == SAVE_SPACE_MODE &&
	    callee->Get_partition_group() != partition_num &&
	    /* Below is for STATIC functions with user-specified
	     * partitioning
	     */
	    ((IP_FILE_HDR_sp_partition_group(shd_callee) !=
	      partition_num) || (partition_num == COMMON_PARTITION)))
	    continue;
#endif
	    
	if (! caller->Summary_Proc ()->Is_alt_entry () &&
	    All_Calls_Processed (callee, cg)) {
	    if (callee->Is_Deletable ()) {
		Delete_Proc (callee);
	    } else {
		if (IPA_Enable_Array_Sections)
		    IPA_LNO_Map_Node(callee, IPA_LNO_Summary);
#ifdef KEY
		if (IPA_Enable_PU_Reorder == REORDER_DISABLE && 
		    !Opt_Options_Inconsistent &&
		    !IPA_Enable_Source_PU_Order)
		{
#endif // KEY
		IPA_Rename_Builtins(callee);
		callee->Write_PU ();
#ifdef _DEBUG_CALL_GRAPH
	    	printf("Writing   %s \n", callee->Name());
#endif // _DEBUG_CALL_GRAPH
#ifdef KEY
		}
#endif // KEY
	    }
	}

    }

#if defined(KEY) && !defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER)
    // Inline the recursive edges now.
    for (vector<inline_info>::iterator iter = inline_list.begin(); 
    	 iter != inline_list.end(); ++iter)
    {
    	inline_info i = *iter;
	IPA_NODE *r_caller = i._node;
	IPA_EDGE *r_edge = i._edge;
	IPA_NODE *r_callee = cg->Callee (r_edge);
	FmtAssert (r_caller == r_callee && IPA_Enable_Inline && 
		   r_edge->Has_Inline_Attrib () && 
		   !r_callee->Has_Noinline_Attrib(), 
		   ("Unexpected attributes during recursive inlining"));
	MEM_POOL_Popper ipo_pool (&Ipo_mem_pool);
	BOOL action_taken = Inline_Call (r_caller, r_callee, r_edge, cg);
	if (action_taken) 
	{
	    IPO_Total_Inlined++;
	    // We should clear the cloned symtab since the caller, i.e. 
	    // the callee symtab has changed. We need to clone it again.
	    r_callee->Clear_Cloned_Symtab();
	}
    	++(*Num_In_Calls_Processed)[r_callee];
    	++(*Num_Out_Calls_Processed)[r_caller];
#ifdef _DEBUG_CALL_GRAPH
    	printf("Processed %s   -->   %s\n", r_caller->Name(), r_callee->Name());
#endif // _DEBUG_CALL_GRAPH
    	r_edge->Set_Processed ();
    }
    inline_list.clear ();
#endif

    // When we inline multiple times of the same callee to this caller
    // we optimized it in IPO_CLONE to same the SYMTAB info for the
    // caller/callee pair.  Now we have to clear the SYMTAB info for
    // the callee so that info has to be redone for a different caller.
    IPA_SUCC_ITER succ_iter2 (caller);
    for (succ_iter2.First(); !succ_iter2.Is_Empty(); succ_iter2.Next()) { 
        IPA_EDGE* edge = succ_iter2.Current_Edge();
        IPA_NODE* callee = IPA_Call_Graph->Callee(edge);

        callee->Clear_Cloned_Symtab();
    }

    if (All_Calls_Processed (caller, cg)) {
	if (caller->Is_Deletable ())
	    Delete_Proc (caller);
	else {
	    if (IPA_Enable_Array_Sections)
		IPA_LNO_Map_Node(caller, IPA_LNO_Summary);
#ifdef KEY
	    if (IPA_Enable_PU_Reorder == REORDER_DISABLE && 
	        !Opt_Options_Inconsistent &&
	        !IPA_Enable_Source_PU_Order)
	    {
#endif // KEY
	    IPA_Rename_Builtins(caller);
	    Identify_Global_As_Local_Candidates(caller);
	    caller->Write_PU ();
#ifdef _DEBUG_CALL_GRAPH
   	    printf("Writing   %s \n", caller->Name());
#endif // _DEBUG_CALL_GRAPH
#ifdef KEY
	    }
#endif // KEY

	}
    }
    else
      Identify_Global_As_Local_Candidates(caller);

#ifdef TODO
    if (IPA_Enable_Recycle) {
	caller->Cleanup_State(cg);
	MEM_POOL_Pop(&Recycle_mem_pool);   
    }
#endif
} // Perform_Transformation

static void
Preorder_annotate_PU_and_kids(const char *const input_file_name,
			      PU_Info    *      current_pu)
{
  MEM_POOL_Push(MEM_pu_nz_pool_ptr);

  Cur_PU_Feedback = NULL;		// don't bother creating the
					// FEEDBACK structure, for we'll
					// just copy the feedback info
					// directly to the output file.
  // save the size of the feedback section, for this will be trashed by
  // Read_Local_Info which replaces it with PU_Info_subsect_ptr
  Elf64_Word feedback_size = PU_Info_subsect_size (current_pu, WT_FEEDBACK);
  
  // Read the analyzed PU from the input file
  Read_Local_Info(MEM_pu_nz_pool_ptr, current_pu);


  Ip_alias_class->Finalize_memops(PU_Info_tree_ptr(current_pu));

  // Get rid of the WHIRL subsection corresponding to the
  // AC_INTERNAL map.
  Set_PU_Info_state(current_pu, WT_AC_INTERNAL, Subsect_Missing);

  // Write the annotated PU to the output file
  // restore the feedback size
  PU_Info_subsect_offset (current_pu, WT_FEEDBACK) = feedback_size;
  Write_PU_Info(current_pu);

  // Annotate and write the kids of this PU
  for (current_pu = PU_Info_child(current_pu);
       current_pu != NULL;
       current_pu = PU_Info_next(current_pu)) {
    Preorder_annotate_PU_and_kids(input_file_name, current_pu);
  }

  // clean up
  MEM_POOL_Pop(MEM_pu_nz_pool_ptr);
}

#define AC_FILE_EXT ".acl"

static void
Perform_Alias_Class_Annotation(void)
{
  Write_ALIAS_CLASS_Map = TRUE;
  Write_AC_INTERNAL_Map = FALSE;

  // For each file we wrote previously,
  for (vector<char *>::iterator name = Ip_alias_class_files.begin();
       name != Ip_alias_class_files.end();
       ++name) {
    // open the file
    void *input_file = Open_Local_Input(*name);

    // open the corresponding new output file
    char *output_file_name = (char *) malloc(strlen(*name) + strlen(AC_FILE_EXT) + 1);
    output_file_name = strcpy(output_file_name, *name);
    output_file_name = strcat(output_file_name, AC_FILE_EXT);
    Output_File *output_file = Open_Output_Info(output_file_name);
    add_to_tmp_file_list(output_file_name);

    // Read the DST from the input file. We can't write it to the
    // output file yet because of some stupid global state maintained
    // somewhere. We apparently have to write it after the PU's are
    // written.
    if (WN_get_dst(input_file) == -1) {
#ifdef KEY
      ErrMsg(EC_IR_Scn_Read, "dst", *name);
#else
      ErrMsg(EC_IR_Scn_Read, "dst", name);
#endif
    }

    // Note that no Read_Global_Info is needed because the global
    // information is already in memory.
    // Get the PU_Info structure for the PU's in the file.
    PU_Info *pu_info_tree = WN_get_PU_Infos(input_file, NULL);
    for (PU_Info *current_pu = pu_info_tree;
	 current_pu != NULL;
	 current_pu = PU_Info_next(current_pu)) {
      Preorder_annotate_PU_and_kids(*name, current_pu);
    }

    // Write the PU_Info's and DST to the output file.
    WN_write_PU_Infos(pu_info_tree, output_file);
    WN_write_dst(Current_DST, output_file);
    
    if (Alias_Nystrom_Analyzer)
    {
      IPA_write_alias_summary(pu_info_tree, output_file);
    }

#if defined(TARG_SL)
    if (ld_ipa_opt[LD_IPA_IPISR].flag)
        WN_write_isr_cg(ipisr_cg, output_file);
#endif

    // close the two files
    Close_Output_Info();
    Free_Local_Input();

    if (!IPA_Debug_AC_Temp_Files) {
      if (keep) {
	const char *const save_file_ext = ".save";
	char *save_file_name = (char *) malloc(strlen(*name) +
					       strlen(save_file_ext) + 1);
	save_file_name = strcpy(save_file_name, *name);
	save_file_name = strcat(save_file_name, save_file_ext);

	if (rename(*name, save_file_name) != 0) {
	  ErrMsg(EC_Ipa_Rename, *name, save_file_name);
	}
	free(save_file_name);
      }
      if (rename(output_file_name, *name) != 0) {
	ErrMsg(EC_Ipa_Rename, output_file_name, *name);
      }
    }
    // Free memory
    free(output_file_name);
  }
  if (IPA_Debug_AC_Temp_Files) {
    for (vector<char *>::iterator name = Ip_alias_class_files.begin();
	 name != Ip_alias_class_files.end();
	 ++name) {
      if (keep) {
	const char *const save_file_ext = ".save";
	char *save_file_name = (char *) malloc(strlen(*name) +
					       strlen(save_file_ext) + 1);
	save_file_name = strcpy(save_file_name, *name);
	save_file_name = strcat(save_file_name, save_file_ext);

	if (rename(*name, save_file_name) != 0) {
	  ErrMsg(EC_Ipa_Rename, *name, save_file_name);
	}
      }
      char *output_file_name = (char *) malloc(strlen(*name) +
					       strlen(AC_FILE_EXT) + 1);
      output_file_name = strcpy(output_file_name, *name);
      output_file_name = strcat(output_file_name, AC_FILE_EXT);
      if (rename(output_file_name, *name) != 0) {
	ErrMsg(EC_Ipa_Rename, output_file_name, *name);
      }
    }
  }
}


static void 
Evaluate_RSE_Cost(MEM_POOL *pool) {
    if (! have_open_input_file) {
        fin = fopen("struc_feedback","r");
        have_open_input_file = TRUE;
        while (!feof(fin)) {
            REG_FB_POINTER reg_fb = CXX_NEW(struct reg_feedback,pool);
            fread(reg_fb,1,sizeof(struct reg_feedback),fin);
            REG_FB_INFO_TABLE[reg_fb->_func_name] = reg_fb;
        }
        fclose(fin);
    }
}

static void
Construct_Budget_Table(IPA_CALL_GRAPH *cg,MEM_POOL *pool) {
    IPA_NODE_ITER cg_iter(cg,PREORDER);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE* node = cg_iter.Current();
        if (node) { 
            if (! node->Is_Deletable()) {
                REG_BUDGET_POINTER reg_budget_ptr = CXX_NEW(struct reg_budget,pool);
                reg_budget_ptr->_node = node; 
                REG_BUDGET_TABLE[IPA_Node_Name(node)] = reg_budget_ptr;
            }
        }
    }
}


static IPA_EDGE *
Get_Most_Frequent_Succ(IPA_NODE *seed,IPA_CALL_GRAPH *cg) {
    IPA_SUCC_ITER succ_iter (seed);
    IPA_EDGE *result = NULL;
    float max_weight = -100;
    for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) { 
        IPA_EDGE *edge = succ_iter.Current_Edge ();
        if ((!edge) || (edge->Is_Deletable())) continue;
        IPA_NODE *node = cg->Callee(edge);
        if (node == seed) {
            FB_FREQ edge_freq = edge->Get_frequency();
            REG_BUDGET_POINTER reg_budget_ptr = REG_BUDGET_TABLE[IPA_Node_Name(node)];
            reg_budget_ptr->_self_recursive_freq += edge_freq.Value();
            continue;
        }
  
        if ((! node->Is_Deletable()) && (node->Get_Partition_Num() == 0)) {
            FB_FREQ edge_freq = edge->Get_frequency();
            FB_FREQ node_freq = node->Get_frequency();
            if (edge_freq > node_freq) {
                DevWarn("STANGE! %s HAS HIGHER EDGE FREQ!",IPA_Node_Name(node));
            }
            REG_FB_POINTER reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(node)];
            if (! reg_fb) {
                DevWarn("Function %s has no reg_fb!",IPA_Node_Name(node));
                continue;
            }
            INT32 stack_regs_used = reg_fb->_stacked_callee_used +
                                    reg_fb->_stacked_caller_used;
            float weight = (edge_freq.Value())*stack_regs_used;
            if ((edge_freq.Value()/node_freq.Value()) > 0.3) {
                if (weight > max_weight) {
                    max_weight = weight;
                    result = edge;
                }
            }
        }
    }
    
    return result;
}
static void 
Print_Partition(IPA_NODE_VECTOR par) {
    for (IPA_NODE_VECTOR::iterator first = par.begin ();
         first != par.end ();++first) {
        IPA_NODE *node = *first;
        REG_BUDGET_POINTER reg_budget_ptr = REG_BUDGET_TABLE[IPA_Node_Name(node)];
        DevWarn("PARTITION %d HAS FUNCTION %s SELFRECURSIVE %f",node->Get_Partition_Num(),IPA_Node_Name(node),reg_budget_ptr->_self_recursive_freq);
    }
}

static void 
Print_List(reg_list *head) {
    INT32 count = 0;
    for (reg_list *begin = head;begin != NULL;begin = begin->_next) {
        count++; 
        if (begin->_node) {
        DevWarn("THE %d REG'S SPILL COST IS %f FUNCTION %s",count,begin->_spill_cost,IPA_Node_Name(begin->_node)); 
       }
    }
}      
   

static IPA_EDGE *
Get_Most_Frequent_Pred(IPA_NODE *seed,IPA_CALL_GRAPH *cg) {
    IPA_PRED_ITER pred_iter (seed);
    IPA_EDGE *result = NULL;
    float max_weight = -100;
    for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) { 
        IPA_EDGE *edge = pred_iter.Current_Edge ();
        if ((!edge) || (edge->Is_Deletable())) continue; 
        IPA_NODE *node = cg->Caller(edge);
        if (node == seed) {
            /*FB_FREQ edge_freq = edge->Get_frequency();
            REG_BUDGET_POINTER reg_budget_ptr = REG_BUDGET_TABLE[IPA_Node_Name(node)];
            reg_budget_ptr->_self_recursive_freq = edge_freq.Value();*/
         
            continue;
        }
 
        if ((! node->Is_Deletable()) && (node->Get_Partition_Num() == 0)) { 
            FB_FREQ edge_freq = edge->Get_frequency();
            FB_FREQ node_freq = node->Get_frequency();
            REG_FB_POINTER reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(node)];
            if (! reg_fb) { 
                DevWarn("Function %s has no reg_fb!",IPA_Node_Name(node));
                continue;
            }
            INT32 stack_regs_used = reg_fb->_stacked_callee_used +
                                    reg_fb->_stacked_caller_used;
            float weight = edge_freq.Value()*stack_regs_used;   
            if ((edge_freq/node_freq) > 0.3) {
                if (weight > max_weight) {
                    max_weight = weight;
                    result = edge;
                }
            }
        }
    }
    
    return result;
}

static BOOL
Pred_Thre(IPA_NODE *seed,IPA_EDGE *pred,IPA_CALL_GRAPH *cg) {
    if (pred == NULL) return FALSE;
    IPA_NODE *caller = cg->Caller(pred);
    reg_feedback *reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(caller)];
    if ((caller->Get_frequency()/seed->Get_frequency()) > 0.1) {
        return TRUE;
    }
    return FALSE;
}

static BOOL 
Succ_Thre(IPA_NODE *seed,IPA_EDGE *succ,IPA_CALL_GRAPH *cg) {
    if (succ == NULL) return FALSE;
    IPA_NODE *callee = cg->Callee(succ);
    reg_feedback *reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(callee)];
    if ((callee->Get_frequency()/seed->Get_frequency()) > 0.1) {
        return TRUE;
    } 

    return FALSE;
}

static void
Get_Next_Partition (IPA_CALL_GRAPH *cg,IPA_NODE_VECTOR& partition,
                    INT32 partition_num,IPA_NODE *entry) {
    //Do this after a transformation pass.
    float max_weight = -100;
    IPA_NODE *seed = NULL;
    IPA_NODE_ITER cg_iter(IPA_Call_Graph,PREORDER);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE* node = cg_iter.Current();
        if (node) {
            if (! node->Is_Deletable()) {
                REG_FB_POINTER reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(node)];
                if (!reg_fb) {
                    DevWarn("Function %s has no reg_fb!",IPA_Node_Name(node)); 
                    continue;
                }
 
	        INT32 stack_regs_used = reg_fb->_stacked_caller_used +
                                        reg_fb->_stacked_callee_used;
                if ((stack_regs_used > 20) && 
                    (reg_fb->_stacked_callee_used > 15)) {
                    FB_FREQ freq = node->Get_frequency();
                    float weight = freq.Value() * stack_regs_used; 
	            if ((weight > max_weight)&&(node->Get_Partition_Num()==0)) {
	                 max_weight = weight;
	                 seed = node;
	            }
                }
            }
        } 
    }
    if (seed == NULL) return;
    seed->Set_Partition_Num(partition_num);
    partition.push_back(seed);
    
    IPA_EDGE *edge = Get_Most_Frequent_Succ(seed,cg);
    while (Succ_Thre(seed,edge,cg)) {
        IPA_NODE *callee = cg->Callee(edge);
        callee->Set_Partition_Num(partition_num);
        partition.push_back(callee);
        IPA_NODE *caller = callee;
        edge = Get_Most_Frequent_Succ(caller,cg);
    }
    
    edge = Get_Most_Frequent_Pred(seed,cg);
    while (Pred_Thre(seed,edge,cg)) {
        IPA_NODE *caller = cg->Caller(edge);
        entry = caller; 
        caller->Set_Partition_Num(partition_num);
        partition.push_back(caller);
        IPA_NODE *callee = caller;
        edge = Get_Most_Frequent_Pred(callee,cg);
    }
            
    //Extend the partition to a region
    /*callee = Find_Most_Frequent_Succ_In_Partition(partition,cg);
    while (Succ_Thre(callee) && callee->Get_Partition_Num()==0) {
        callee->Set_Partition_Num(partition_num);
        partition.push_back(callee);
        callee = Find_Most_Frequent_Succ_In_Partition(&partition,cg);
        partition.push_back(callee);
    }*/             
}

static BOOL 
Compare_Cost(reg_list *list,IPA_CALL_GRAPH *cg,INT32 partition_num) {
    float spill_cost = list->_spill_cost;
    float freq = 0;
    IPA_NODE *node = list->_node;
    IPA_SUCC_ITER succ_iter(node);
    for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {
        IPA_EDGE* edge = succ_iter.Current_Edge();
        if ((!edge) || (edge->Is_Deletable())) continue;
        IPA_NODE* callee = cg->Callee(edge);
        if (callee->Get_Partition_Num() == partition_num) {
            if (callee == node) {
                reg_budget *budget = REG_BUDGET_TABLE[IPA_Node_Name(node)];
                freq = freq + (edge->Get_frequency()).Value()*REC_OVERFLOW_COST;
            } else {
                freq = freq + (edge->Get_frequency()).Value()*OVERFLOW_COST;
            }
        }
    }
    
    //SHOULD US ALSO USE THE PRED CALLED FREQUENCY?
    if (spill_cost > freq) {
        return TRUE;
    } else {
        return FALSE;
    }
}

static BOOL 
Compare_Self_Recursive_Cost(reg_list *list) {
    float spill_cost = list->_spill_cost;
    IPA_NODE *node = list->_node;
    if (!node->Is_Deletable()) {
        reg_budget *budget = REG_BUDGET_TABLE[IPA_Node_Name(node)];
        if (spill_cost > (budget->_self_recursive_freq*REC_OVERFLOW_COST)) {
            return TRUE;
        } else {
            return FALSE;
        }
    }
} 

static reg_list* 
Construct_List(IPA_NODE_VECTOR par) {
struct reg_list* head = (reg_list *) malloc(sizeof(reg_list));
    struct reg_list* tail = (reg_list *) malloc(sizeof(reg_list));
    head->_node = NULL;
    tail->_node = NULL;
    head->_spill_cost = FLT_MAX;
    tail->_spill_cost = -FLT_MAX;
    tail->_next = NULL;
    tail->_prev = head;
    head->_prev = NULL;
    head->_next = tail;
    for (IPA_NODE_VECTOR::iterator first = par.begin ();
        first != par.end ();++first) {
        IPA_NODE *node = *first;
        if (! node->Is_Deletable()) {
            //Here all nodes in partition should not be deletable;
            REG_FB_POINTER reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(node)];
            if (reg_fb == NULL) {
                DevWarn("Function %s has no reg_fb!",IPA_Node_Name(node));
                continue;
            }

            for (INT32 i = 0;i < 96;i++) {
                if (reg_fb->_cost[i] > 0) {
                    struct reg_list* list = (reg_list*) malloc(sizeof(reg_list))
;
                    list->_spill_cost = reg_fb->_cost[i];
                    list->_node       = node;
                    for (reg_list *begin = head;begin != NULL;
                        begin = begin->_next) {
                        if (list->_spill_cost > begin->_spill_cost) {
                            list->_prev = begin->_prev;
                            list->_next = begin;
                            begin->_prev->_next = list;
                            begin->_prev = list;
                            break;
                        }
                    }
                }
            }
        }
    }//End of list construction;
    
    return head;
}

static void
Delete_List(reg_list* head) {
}

static void
Print_Budget(IPA_NODE_VECTOR par) {
    for (IPA_NODE_VECTOR::iterator first = par.begin ();
        first != par.end ();++first) {
        IPA_NODE *node = *first;
        REG_FB_POINTER reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(node)];
        REG_BUDGET_POINTER reg_budget_ptr = 
                                 REG_BUDGET_TABLE[IPA_Node_Name(node)];
        INT32 ori = reg_fb->_stacked_callee_used+reg_fb->_stacked_caller_used;
        INT32 now = reg_budget_ptr->_budget;
        DevWarn("ORIGINAL %d NOW %d FOR FUNCTION %s",ori,now,IPA_Node_Name(node));
    }    
}

static void 
Distribute_Partition(IPA_NODE_VECTOR par,IPA_CALL_GRAPH *cg,INT32 partition_num){
    struct reg_list* head = Construct_List(par);   
    Print_List(head); 
    INT32 count = 0;
    for (reg_list *begin = head; begin != NULL; begin = begin->_next) {
        IPA_NODE *node = begin->_node;
        if (node) {
            REG_BUDGET_POINTER reg_budget_ptr =
                                       REG_BUDGET_TABLE[IPA_Node_Name(node)];
            if (count > 100) {
                BOOL use = Compare_Cost(begin,cg,partition_num);
                if (use) {
                    reg_budget_ptr->_budget++;
                    count++;
                }
            } else {
                if (reg_budget_ptr->_self_recursive_freq > 0) {
                    if (Compare_Self_Recursive_Cost(begin)) {
                        count++; 
                        reg_budget_ptr->_budget++;
                    }
                } else {
                    reg_budget_ptr->_budget++;
                    count++;
                }
            }       
        }
    }
    Print_Budget(par);  
    //TODO::Should delete the list.    
}

static void Initialize_Partition_Num(IPA_CALL_GRAPH *cg) {
    IPA_NODE_ITER cg_iter(IPA_Call_Graph,PREORDER);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE* node = cg_iter.Current();
        if (node) {
            node->Set_Partition_Num(0);
        } 
    }     
   
}

static INT32 
Count_Total_Regs(IPA_NODE_VECTOR partition) {
    INT32 count = 0;
    for (IPA_NODE_VECTOR::iterator iter = partition.begin ();
        iter != partition.end ();++iter) {
        IPA_NODE *node = *iter;
        REG_FB_POINTER reg_fb = REG_FB_INFO_TABLE[IPA_Node_Name(node)];
        count = count + reg_fb->_stacked_callee_used
                + reg_fb->_stacked_caller_used;
    }
 
   return count;
} 

static void
Stacked_Regs_Distribution(IPA_CALL_GRAPH *cg) {
    BOOL finished = FALSE;
    Initialize_Partition_Num(cg);
      
    INT32 partition_num = 1;
    IPA_NODE *entry;
    
    while (!finished) {
       IPA_NODE_VECTOR partition;
       Get_Next_Partition(cg,partition,partition_num,entry);
       if (!partition.empty()) {
           Print_Partition(partition);
           INT32 total = Count_Total_Regs(partition);
           BOOL no_self_recursive_node = TRUE;
           for (IPA_NODE_VECTOR::iterator iter = partition.begin();
               iter != partition.end();++iter) {
               IPA_NODE *n = *iter;
               REG_BUDGET_POINTER p = REG_BUDGET_TABLE[IPA_Node_Name(n)];
               if (p->_self_recursive_freq > 0) {
                   no_self_recursive_node = FALSE;
                   break;
               }
           }
 
           if ((total < 96) && (no_self_recursive_node)) {
               DevWarn("REGISTER USAGE OF THIS PARTITION LESS THAN 96!");
               for (IPA_NODE_VECTOR::iterator iter = partition.begin();
                   iter != partition.end();++iter) {
                   (*iter)->Set_Partition_Num(-1);
               } 
               continue;
           } else {
               Distribute_Partition(partition,cg,partition_num);
           }
       } else {
           finished = TRUE;
       } 
       partition_num++; 
       if (partition_num == 10) finished = TRUE;
    } 
}

#ifdef KEY
#include <queue>
struct order_node_by_freq {
    bool operator() (IPA_NODE * first, IPA_NODE * second)
    {
    	return first->Get_frequency() < second->Get_frequency();
    }
};

// This operator ensures that PUs are emitted after IPA in the same order
// as in the original source code. This has two parts:
//   * sort PUs by file-id
//   * sort PUs with same file-id by their order in source code
struct order_node_by_file_id {
    bool operator() (IPA_NODE * first, IPA_NODE * second)
    {
      // (bug 11837) Fortran alternate-entry points do not get their
      // own pu_info, so their file_id will be -1 (uninitialized). We
      // also do not bother because they will never be emitted
      // separately, but as part of the containing procedure. If we do
      // want to get their file_id, we could access a pu_info in the
      // same file through IPA_NODE::File_Header().pu_list, and get
      // its file_id.
      //
      // same file, use source code order
      if (first->File_Id() == second->File_Id() &&
          first->File_Id() != -1 /* not a Fortran alt-entry point */)
        return PU_Info_subsect_offset(first->PU_Info(), WT_TREE) >
               PU_Info_subsect_offset(second->PU_Info(), WT_TREE);

      // use order in which files were originally compiled
      return first->File_Id() > second->File_Id();
    }
};
static void
IPA_Remove_Regions (IPA_NODE_VECTOR v, IPA_CALL_GRAPH * cg)
{
    SCOPE * old_scope = Scope_tab;

    for (IPA_NODE_VECTOR::iterator node = v.begin ();
	 node != v.end (); ++node)
    
    {
      PU pu = Pu_Table[ST_pu((*node)->Func_ST())];

      if (!(PU_src_lang (pu) & PU_CXX_LANG) || !PU_has_region (pu))
      	continue;

      IPA_NODE_CONTEXT context (*node);	// switch to the node context
      cg->Map_Callsites (*node);
      WN_MAP Node_Parent_Map = (*node)->Parent_Map();
      WN_MAP_TAB * Node_Map_Tab = PU_Info_maptab ((*node)->PU_Info());

      IPA_SUCC_ITER succ_iter (*node);
      BOOL changed = false;
      for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next())
      {
	IPA_EDGE *edge = succ_iter.Current_Edge ();
	IPA_NODE *callee = cg->Callee (edge);

	if (callee->PU_Can_Throw())
	  continue;
	
	// Remove any region immediately surrounding this edge at caller
	WN * call = edge->Whirl_Node();
	Is_True (call, ("Call whirl node absent in IPA edge"));
	WN * parent = WN_Get_Parent (call, Node_Parent_Map, Node_Map_Tab);
	for (; parent; parent = WN_Get_Parent (parent, Node_Parent_Map, Node_Map_Tab))
	{
	    if (WN_operator(parent) != OPR_REGION || !WN_region_is_EH(parent))
	    	continue;
	    if (WN_block_empty (WN_region_pragmas (parent)))
	    {
	      WN * body = WN_region_body (parent);
	      if (WN_first (body) == WN_last (body) && 
	      	  WN_first (body) == call)
		{ // remove the region
		    changed = true;

      		    WN * parent_of_region = WN_Get_Parent (parent, 
		    				Node_Parent_Map, Node_Map_Tab);
		    
		    Is_True (parent_of_region, 
		    	     ("Region node not within any block"));
		    Is_True (WN_prev (parent) || 
		    	     (WN_operator (parent_of_region) == 
			     OPR_BLOCK && WN_first (parent_of_region) == 
			     parent), ("Error removing EH region"));

		    // if region is the 1st stmt
		    if (!WN_prev (parent))
		      WN_first (parent_of_region) = call;
		    else
		      WN_next (WN_prev (parent)) = call;
		    WN_prev (call) = WN_prev (parent);

		    // if region is the last stmt
		    if (!WN_next (parent))
		      WN_last (parent_of_region) = call;
		    else
		      WN_prev (WN_next (parent)) = call;
		    WN_next (call) = WN_next (parent);
		    // remove the call node from the region body
		    WN_first (body) = WN_last (body) = NULL;
		    // detach the region
		    WN_prev (parent) = WN_next (parent) = NULL;
		}
	    }
	}
      }
      if (changed)
      {
	WN_Parentize ((*node)->Whirl_Tree (FALSE), Node_Parent_Map, Node_Map_Tab);
      	changed = false;
      }
    }
    Scope_tab = old_scope;
}

// Bug 4244: Check if the PU we are going to write has child-PUs. If 
// yes, then write them out (writing them out of course does not write 
// all their sections). See IP_WRITE_pu for more details on handling 
// of PUs with lexical-level > 2.
static void
check_for_nested_PU (IPA_NODE * node)
{
    PU_Info * info = node->PU_Info();

    if (info)
    {
      PU_Info * child = PU_Info_child (info);
      // check for children
      while (child)
      {
	IPA_NODE * child_node = Get_Node_From_PU (child);
	if (child_node == NULL)
	{ // bug 11647: the node may have been deleted by DFE.
	  child = PU_Info_next (child);
	  continue;
	}
#ifdef Is_True_On
	int lexical_level = PU_lexical_level (child_node->Func_ST());
	Is_True (lexical_level > GLOBAL_SYMTAB+1, 
	         ("Nested PU's level should be greater than 2"));
	Is_True (node->File_Id() == child_node->File_Id(),
	         ("Parent and child PU are in different files"));
	if (!PU_ftn_lang (Pu_Table[ST_pu (child_node->Func_ST())]))
	  DevWarn ("Encountered nested PU in non-Fortran langage");
#endif
	// If it has been written out, the nested PUs should also
	// have been written out
	if (!PU_Written (IPA_Call_Graph->Graph(), child_node,
	    &child_node->File_Header()) &&
	    !PU_Deleted (IPA_Call_Graph->Graph(), child_node->Node_Index(),
	    &child_node->File_Header()))
	{
          check_for_nested_PU (child_node);
#if defined(TARG_SL)
	  if (ld_ipa_opt[LD_IPA_IPISR].flag) {
	    ErrMsg(EC_Ipa_Outfile, "Encountered nested PU when -ipisr is turned on");
	  }
#endif
	  IPA_Rename_Builtins (child_node);
	  child_node->Write_PU();
	}
	child = PU_Info_next (child);
      }
    }
}
#endif // KEY

#if defined(TARG_SL)
// ipisr_cg is the call graph used for interprocedural ISR register
// allocation. It'll put into MIPS_WHISRL_IPISR section of *.I by ipa.
// The struct of the call graph is as follows.
//   <PU0's 1st caller id>, ..., <PU0's last caller id>, <-1>
//   <PU1's 1st caller id>, ..., <PU1's last caller id>, <-1>
//   ...
//   <PUn's 1st caller id>, ..., <PUn's last caller id>, <-1>
// The PU id is equal to the ID in be. -1 is used to mark the end of callers. 
vector<mINT32> ipisr_cg;
#endif

static void mark_argument_array_remapping_candidate_malloc(WN *wn,
  int argument_num, IPA_NODE *callee);

static void
IPO_main (IPA_CALL_GRAPH* cg)
{
    MEM_POOL_Constructor ipo_pool (&Ipo_mem_pool, "Inline mem pool", FALSE); 
    MEM_POOL_Constructor recycle_pool (&Recycle_mem_pool,
				       "Recycle_mem_pool", FALSE); 
    MEM_POOL_Constructor array_pool (&IPA_LNO_mem_pool, "IPA_LNO_mem_pool",
				     FALSE); 
    
    Set_Error_Phase ("IPA Transformation");

    if (IPA_Enable_Array_Sections) {
	IPA_LNO_Summary = CXX_NEW(IPA_LNO_WRITE_SUMMARY(array_pool.Pool ()),
				  array_pool.Pool ());
    }
    
    if (IPA_Enable_Padding)
	IPO_Pad_Symtab (IPA_Common_Table);

    if (IPA_Enable_Split_Common)
	IPO_Split_Common ();

    //reorder :(the follwoing two lines)
    if(IPA_Enable_Reorder && reorder_candidate.size){
       IPO_get_new_ordering();
       IPO_reorder_Fld_Tab();
    }

    Init_Num_Calls_Processed (cg, ipo_pool.Pool ());

#ifdef KEY
    IPA_Create_Builtins();
#endif

    IPA_NODE_VECTOR walk_order;
     
    Build_Transformation_Order (walk_order, cg->Graph(), cg->Root());

#ifdef KEY
    if (IPA_Enable_EH_Region_Removal)
    	IPA_Remove_Regions (walk_order, cg); // Remove EH regions that are not required
#endif
    
    if (!IPA_Enable_Scale)
      IPA_Enable_Global_As_Local = FALSE;

    if (Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL)) {
       fprintf( TFile, "\n\nIPA_Call_Graph:\n");
       IPA_Call_Graph->Print(TFile);
    }

    // we will use the following loop to check whether it is legal to perform
    // the complete structure relayout optimization; later (in the subsequent
    // loop) we will perform the actual optimization (if it is legal)

    // same with array remapping optimization

    // comment out the following since this optimization benefits both mso as
    // well as non-mso compilations; besides, the following will disable
    // structure peeling also
    // if (!IPA_Enable_Scale)
    //   -mso (multi-core scaling optimization is not on
    //   IPA_Enable_Struct_Opt = 0;
    for (IPA_NODE_VECTOR::iterator first = walk_order.begin();
         first != walk_order.end(); first++)
    {
      int argument_num;

      // Limit global-as-local analysis to Fortran programs.  C/C++ programs will
      // need more work.
      if (!PU_f77_lang((*first)->Get_PU()) 
	  && !PU_f90_lang((*first)->Get_PU()))
	IPA_Enable_Global_As_Local = FALSE;
      
      if (IPA_Enable_Struct_Opt != 0 &&
	  (PU_src_lang((*first)->Get_PU()) & PU_C_LANG)) {
        IPA_NODE_CONTEXT context(*first);
        IPO_WN_Update_For_Complete_Structure_Relayout_Legality(*first);

        argument_num = -1;
        IPO_WN_Update_For_Array_Remapping_Legality(*first, 1, &argument_num);
        if (argument_num != -1)
        {
          // argument_num != -1 means that an array remapping candidate was
          // found inside the function (*first), and that it was nicely
          // malloced, but that this candidate is an argument into the function
          // (*first).  When this happens, we need to propagate this finding
          // upward to all the callers of (*first)
          IPA_PRED_ITER pred_iter(*first);
          for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next())
          {
            IPA_EDGE *edge = pred_iter.Current_Edge();
            if (edge != NULL)
            {
              IPA_NODE *caller = IPA_Call_Graph->Caller(edge);
              IPA_NODE_CONTEXT context(caller);
              WN *wn_tree = WN_func_body(caller->Whirl_Tree());
              // look for all the calls to (*first) inside this wn_tree, and
              // mark the corresponding argument of all these calls as an array
              // remapping candidate that has been malloced
              mark_argument_array_remapping_candidate_malloc(wn_tree,
                argument_num, *first);
            }
          }
        }
      }
      else
        IPA_Enable_Struct_Opt = 0; // only do this for C programs
    }

    // normally we only need the above loop to check for legality, but in the
    // case of array remapping optimization, there is a phase ordering problem:
    // we need to mark all the array remapping candidates' malloc bit before we
    // can decide if they are valid candidates to apply the stringent legality
    // checks on, but unfortunately they cannot both occur in the same pass,
    // since we cannot rely on the order the functions are compiled.  Let's run
    // another loop
    if (IPA_Enable_Struct_Opt != 0)
    {
      for (IPA_NODE_VECTOR::iterator first = walk_order.begin();
           first != walk_order.end(); first++)
      {
        IPA_NODE_CONTEXT context(*first);
        IPO_WN_Update_For_Array_Remapping_Legality(*first, 2, NULL);
      }
    }

    if (IPA_Enable_Global_As_Local) {
      int i;
      ST * s;
      Global_As_Local_Ndx = -1;
      // Initialize global-as-local scratch data.
      FOREACH_SYMBOL(GLOBAL_SYMTAB, s, i) {
	if ((ST_sclass(s) == SCLASS_DGLOBAL) 
	    && !ST_is_equivalenced(s)
	    && (ST_export(s) != EXPORT_PREEMPTIBLE)) {
	  Global_As_Local_Ndx++;
	  if (Global_As_Local_Ndx < MAX_GLOBAL_AS_LOCAL) {
	    Set_ST_is_global_as_local(s);
	    Global_Sts[Global_As_Local_Ndx] = s;
	  }
	  else
	    break;
	}
      }
    }

    for (IPA_NODE_VECTOR::iterator first = walk_order.begin ();
	 first != walk_order.end ();
	 ++first) {

      // IP alias class analysis takes place in IP_WRITE_pu, which is
      // a call-graph descendant of Perform_Transformation. When the
      // following call returns, alias class analysis has been done
      // for the current PU.
      //ST *func_st = (*first)->Func_ST();
      //Set_PU_rse_budget(Pu_Table[ST_pu(func_st)],30);   
      Perform_Transformation (*first, cg);
    }

#if defined(TARG_SL)
    // Other non-postorder emitions

    // If -ipisr, emit PUs in BFS order. 
    // At this time, ld_ipa_opt[LD_IPA_IPISR].flag is true. 
    if (IPA_Enable_PU_Reorder == REORDER_BY_BFS)
    {	// reorder by breadth-first ordering 

        IPA_NODE_VECTOR emit_order, emit_queue;
        Build_BFS_Order (emit_order, cg->Graph(), cg->Root());

        mINT32 cnt = 0;
      
        // Emit the PU in BFS order 
        for (IPA_NODE_VECTOR::iterator first = emit_order.begin ();
	        first != emit_order.end (); ++first) 
        {
            IPA_NODE* emit = *first;
            if (!PU_Deleted (cg->Graph(), emit->Node_Index(), // deleted
                                &emit->File_Header()) && 
                !PU_Written (cg->Graph(), emit,    // written
                                &emit->File_Header())
            ) {
	          // bug 4668
	          check_for_nested_PU (emit);
	          IPA_Rename_Builtins(emit);
	          emit->Write_PU();
              emit->Set_Emit_Id(cnt++); // It equals to the PU id in cg
              emit_queue.push_back(emit);
	        }
        }

        // Build ipisr_cg. 
        for (IPA_NODE_VECTOR::iterator first = emit_queue.begin ();
	        first != emit_queue.end (); ++first) 
        {
            IPA_NODE* callee = *first;
            IPA_PRED_ITER pred_iter(callee);
            vector<mINT32> visited;

            //printf("Find %s's preds\n", IPA_Node_Name(callee));
            for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) { 

                IPA_EDGE *edge = pred_iter.Current_Edge ();
                if (edge == NULL) continue;

                IPA_NODE *caller = cg->Caller (edge);
                mINT32 id = caller->Emit_Id();

                //printf("%s --> %s\n", IPA_Node_Name(callee), IPA_Node_Name(caller));

                Is_True(emit_queue.end() 
                            != find(emit_queue.begin(), emit_queue.end(), caller),
                        ("In IPISR, the caller is not in the emit queue!\n"));

                // If this caller is not added yet
                if (visited.end() == find(visited.begin(), visited.end(), id)) {
                    ipisr_cg.push_back(id);
                    visited.push_back(id);
                }
            }

            ipisr_cg.push_back(-1); // -1 marks the end of the call list
        }
    }
#endif // TARG_SL

#ifdef KEY
    { // PU reordering heuristics
      if (IPA_Enable_Source_PU_Order || Opt_Options_Inconsistent)
      { // We cannot do any PU-reordering
      	priority_queue<IPA_NODE*, vector<IPA_NODE*>, order_node_by_file_id> emit_order;
    	for (IPA_NODE_VECTOR::iterator first = walk_order.begin ();
	 	first != walk_order.end (); ++first)
    	{
      	    if (!PU_Deleted (cg->Graph(), (*first)->Node_Index(), 
      		&(*first)->File_Header()))
      	    {
      	    	emit_order.push (*first);
      	    }
    	}
    	while (!emit_order.empty())
    	{
      	    IPA_NODE *emit = emit_order.top();
	    emit_order.pop();
	    if (!PU_Written (cg->Graph(), emit,
	        &emit->File_Header()))
	    {
	      // bug 4244
	      check_for_nested_PU (emit);
	      IPA_Rename_Builtins(emit);
	      IPA_NODE::next_file_id = emit_order.empty() ? -1 : emit_order.top()->File_Id();
	      emit->Write_PU();
	    }
    	}
      }
      else if (IPA_Enable_PU_Reorder == REORDER_BY_EDGE_FREQ)
      {	// reorder by edge frequency
	for (vector<IPA_NODE*>::iterator it = emit_order.begin(); 
					 it != emit_order.end(); ++it)
	{
	    IPA_NODE * n = *it;
      	    if (!PU_Deleted (cg->Graph(), n->Node_Index(), // deleted
	    		&n->File_Header()) && 
			!PU_Written (cg->Graph(), n,	// written
			&n->File_Header()))
	    {
		IPA_Rename_Builtins(n);
		n->Write_PU();
	    }
	}
	emit_order.clear();
	// Do we have nodes not belonging to an edge? Write them now.
    	for (IPA_NODE_VECTOR::iterator first = walk_order.begin ();
	 	first != walk_order.end (); ++first)
    	{
      	    if (!PU_Deleted (cg->Graph(), (*first)->Node_Index(), 
      			&(*first)->File_Header()) &&
			!PU_Written (cg->Graph(), *first,
			&(*first)->File_Header()))
	    {
		check_for_nested_PU (*first);
		IPA_Rename_Builtins(*first);
		(*first)->Write_PU();
	    }
	}
      }
      else if (IPA_Enable_PU_Reorder == REORDER_BY_NODE_FREQ)
      {	// reorder by node frequency
    	priority_queue<IPA_NODE*, vector<IPA_NODE*>, order_node_by_freq> emit_order;
    	for (IPA_NODE_VECTOR::iterator first = walk_order.begin ();
	 	first != walk_order.end (); ++first)
    	{
      	    if (!PU_Deleted (cg->Graph(), (*first)->Node_Index(), 
      		&(*first)->File_Header()))
      	    {
      	    	emit_order.push (*first);
      	    }
    	}

    	while (!emit_order.empty())
    	{
      	    IPA_NODE *emit = emit_order.top();
	    emit_order.pop();
      	    //printf("Writing %s with frequency %f\n", emit->Name(), emit->Get_frequency().Value());
	    if (!PU_Written (cg->Graph(), emit,
	        &emit->File_Header()))
	    {
	      // bug 4668
	      check_for_nested_PU (emit);
	      IPA_Rename_Builtins(emit);
	      emit->Write_PU();
	    }
    	}
      }
    }
#endif // KEY

    if(IPA_Enable_Reorder)
       IPO_Finish_reorder(); //MEM_POOL_Pop (&reorder_local_pool);pop reorder_candidate
	 
    IP_flush_output ();			// Finish writing the PUs
    
    if (IPA_Enable_Array_Sections)
	IPA_LNO_Write_Summary (IPA_LNO_Summary);

    Clear_Common_Block_Element_Map ();

    if ( Trace_IPA || Trace_Perf ) {
	fprintf ( TFile, "Total number of edges = %d\n", IPA_Call_Graph->Edge_Size() );
    }

    if ( INLINE_List_Actions ) {
        fprintf ( stderr, "Total number of edges = %d\n", IPA_Call_Graph->Edge_Size() );
    }

  if( Get_Trace ( TP_IPA, IPA_TRACE_TUNING) ){
    fclose(Y_inlining);
  }

} // IPO_main


static BOOL BE_symtab_initialized = FALSE; 

//-----------------------------------------------------------------------
// Initialize the back-end symbol table (lifted from be/be/driver.cxx)
//-----------------------------------------------------------------------
static void 
BE_Symtab_Initialize()
{
  if (!BE_symtab_initialized) {
    BE_symtab_initialize_be_scopes();
    BE_symtab_alloc_scope_level (GLOBAL_SYMTAB);
    for (SYMTAB_IDX scope_level = 0;
	 scope_level <= GLOBAL_SYMTAB;
	 ++scope_level) {
      // No need to deal with levels that don't have st_tab's. Currently
      // this should be only zero.
      if (Scope_tab[scope_level].st_tab != NULL) {
	Scope_tab[scope_level].st_tab->
	  Register(*Be_scope_tab[scope_level].be_st_tab);
      }
      else {
	Is_True(scope_level == 0,
		("Nonexistent st_tab for level %d", scope_level));
      }
    }
    BE_symtab_initialized = TRUE; 
  } 
} 

//-----------------------------------------------------------------------
// Free back-end symbol table resources (lifted from be/be/driver.cxx)
//-----------------------------------------------------------------------
static void 
BE_Symtab_Finalize()
{
  if (BE_symtab_initialized) { 
    for (SYMTAB_IDX idx = GLOBAL_SYMTAB + 1; idx > 0; --idx) {
      // No need to deal with levels that don't have st_tab's. Currently
      // this should be only zero.
      SYMTAB_IDX scope_level = idx - 1;
      if (Scope_tab[scope_level].st_tab != NULL) {
        Scope_tab[scope_level].st_tab->
          Un_register(*Be_scope_tab[scope_level].be_st_tab);
        Be_scope_tab[scope_level].be_st_tab->Clear();
      }
      else {
        Is_True(scope_level == 0,
                ("Nonexistent st_tab for level %d", scope_level));
      }
    }

    BE_symtab_free_be_scopes();
    BE_symtab_initialized = FALSE; 
  } 
}


void
Perform_Interprocedural_Optimization (void)
{
  if (IP_File_header.size() == 0) {
    if (IPA_Enable_ipacom) {
      ipa_compile_init ();
      ipacom_doit (NULL);
      exit (RC_SYSTEM_ERROR); // should never reach here
    } else {
      Signal_Cleanup (0);
#ifdef KEY
      exit(1);	// Moved exit(1) from Signal_Cleanup to here.
#endif
    }
  }

  Set_ipa_tlog_phase(PHASE_IPA);

  if (IPA_Enable_ipacom) {
    ipa_compile_init ();
  }

#ifdef TODO
  // should move to analysis phase
  if( IPA_Enable_Feedback ) {
    setup_IPA_feedback();
  }
#endif

  BE_Symtab_Initialize();

  Perform_Interprocedural_Analysis ();
                                // Must be called before any of the output
                                // files are written, since symtab must be
                                // compiled first.
#ifdef Is_True_On
  CGB_IPA_Initialize(IPA_Call_Graph);
#endif

  if (IPA_Enable_AutoGnum)
  	Autognum_Driver ();
  
  if (IPA_Enable_ipacom)
    ipacom_process_symtab (IP_global_symtab_name());

  CXX_MEM_POOL ip_alias_class_mem_pool ("Alias class pool", FALSE);

  // Declare the alias classification object on the stack, but make
  // it globally accessible through the Ip_alias_class global
  // pointer. Declaration of the object as an automatic variable is
  // just so we don't have to worry about constructing the object at
  // program startup time. In the current implementation, this is
  // because we need to have the ip_alias_class_mem_pool initialized
  // when we construct the ip_alias_class object. There are other
  // ways to accomplish it, of course...
  IP_ALIAS_CLASSIFICATION ip_alias_class_instance(ip_alias_class_mem_pool());
  Ip_alias_class = &ip_alias_class_instance;
  if (IPA_Enable_Alias_Class) {
    Ip_alias_class->Init_maps();
  }

#if Is_True_On
  if ( Get_Trace ( TKIND_ALLOC, TP_IPA) ) {
    fprintf ( TFile,
	      "\n%s%s\tMemory allocation information before IPO_main\n%s%s\n",
	      DBar, DBar, DBar, DBar );
    MEM_Trace ();
  }
#endif

  if (Get_Trace(TP_IPA, IPA_TRACE_TUNING_NEW)) {
    fprintf(TFile, "\t+++++++++++++++++++++++++++++++++++++++\n");
    IPA_Call_Graph->Print_vobose(TFile);
    fprintf(TFile, "\t+++++++++++++++++++++++++++++++++++++++\n");
  }

  IPO_main (IPA_Call_Graph);

#if Is_True_On
  if ( Get_Trace ( TKIND_ALLOC, TP_IPA) ) {
    fprintf ( TFile,
	      "\n%s%s\tMemory allocation information after IPO_main\n%s%s\n",
	      DBar, DBar, DBar, DBar );
    MEM_Trace ();
  }
#endif

  // Classify the global initialized data after classifying all the
  // code so we get the benefit of all the available function arity
  // information.
  if (IPA_Enable_Alias_Class) {
    Ip_alias_class->
      Classify_initialized_data(Scope_tab[GLOBAL_SYMTAB].inito_tab);
  }

  IP_write_global_symtab();

  if (IPA_Enable_Alias_Class) {
    Perform_Alias_Class_Annotation();
  }

#if Is_True_On
  if ( Get_Trace ( TKIND_ALLOC, TP_IPA) ) {
    fprintf ( TFile,
	      "\n%s%s\tMemory allocation information after alias class annotation\n%s%s\n",
	      DBar, DBar, DBar, DBar );
    MEM_Trace ();
  }
#endif

  Ip_alias_class->Release_resources();
  Ip_alias_class = NULL;

  IPA_NystromAliasAnalyzer::clean();

#if Is_True_On
  {
    for (IP_FILE_HDR_TABLE::iterator f = IP_File_header.begin();
         f != IP_File_header.end();
         ++f) {
      Is_True(IP_FILE_HDR_all_procs_processed(*f),
              ("At end of IPA, file header %d has %u PUs, %u processed PUs",
               IP_FILE_HDR_file_name(*f) ? IP_FILE_HDR_file_name(*f) : "***",
               IP_FILE_HDR_num_procs(*f),
               IP_FILE_HDR_num_procs_processed(*f)));
    }
  }
#endif

  BE_Symtab_Finalize();

  if (IPA_Enable_ipacom) {
#ifdef Is_True_On
      CGB_IPA_Terminate();
#endif
      ipacom_doit (IPA_Enable_Opt_Alias ? Ipa_File_Name : NULL);
      exit (RC_SYSTEM_ERROR);		// should never reach here
  } else {
#ifdef Is_True_On
      CGB_IPA_Terminate();
#endif
      kill (getpid (), SIGQUIT);
  } 

  // should never reach here

} // Perform_Interprocedural_Optimization

// This function prints the inlining decision in ecc style.
void
Print_inline_decision (void) {

  FILE *orc_script = fopen ("orc_script.log", "w");
  INT32 callsite_linenum;
  INT32 callsite_colnum;
  USRCPOS callsite_srcpos;
  char *caller_filename, *callee_filename;
  char *caller_funcname, *callee_funcname;

  IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);
  fprintf (orc_script, "\n#BEGIN_INLINE\n\n");
  
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {

    IPA_NODE* node = cg_iter.Current();
    if (node) {
      // Important for the getting WN from edge
      IPA_NODE_CONTEXT context (node);
      IPA_Call_Graph->Map_Callsites (node);

      // get the node-caller's filename
      IP_FILE_HDR& caller_hdr = node->File_Header ();
      caller_filename = (char *) alloca(strlen(caller_hdr.file_name)+1);
      strcpy(caller_filename, caller_hdr.file_name);
			
      fprintf (orc_script, "COMPILE (\"%s\",%s,NOREG) {\n", 
               DEMANGLE(caller_filename), DEMANGLE(IPA_Node_Name(node)));
      BOOL seen_callee = FALSE;

      IPA_SUCC_ITER succ_iter(node);
      for (succ_iter.First(); !succ_iter.Is_Empty(); succ_iter.Next()) {
         IPA_EDGE* tmp_edge = succ_iter.Current_Edge();
         if (tmp_edge) {
	   EDGE_INDEX   tmp_idx = tmp_edge->Edge_Index();
	   WN* call_wn = tmp_edge->Whirl_Node();
	   IPA_NODE* callee =IPA_Call_Graph->Callee( tmp_idx ); 

    	   IP_FILE_HDR& callee_hdr = callee->File_Header ();

    	   if (call_wn == NULL) {
             fprintf (orc_script, "Warning: no source line number found for call-edge [%s --> %s]\n", node->Name(), callee->Name());
       	     callsite_linenum = 0;
	     callsite_colnum = -1;
    	   } else {
             USRCPOS_srcpos(callsite_srcpos) = WN_Get_Linenum (call_wn);
             callsite_linenum = USRCPOS_linenum(callsite_srcpos);
	     callsite_colnum  = USRCPOS_column(callsite_srcpos);
           }

           callee_filename = (char *) alloca(strlen(callee_hdr.file_name)+1);
           strcpy (callee_filename, callee_hdr.file_name);
		  
	   if (IPA_Enable_Inline && tmp_edge->Has_Inline_Attrib () && 
	       !callee->Has_Noinline_Attrib()) {
              fprintf (orc_script, "  INLINE (%d,%d,\"%s\",%s,NOREG) {\n  }\n",
	               callsite_linenum,callsite_colnum, callee_filename, 
		       DEMANGLE(IPA_Node_Name(callee)) );
              seen_callee = TRUE;
           } else {
              // should inline the callee
              fprintf (orc_script, "  CALL (%d,%d,\"%s\",%s,NOREG)\n",
	               callsite_linenum,callsite_colnum, callee_filename, 
		       DEMANGLE(IPA_Node_Name(callee)) );
           }
         }
      }//end of for-loop

      fprintf(orc_script, "}\n");
    }
  }

  fprintf(orc_script, "\n#END_INLINE\n\n");
  fclose (orc_script);
}

// Visit the input wn and look for a call to "callee".  Locate the argument in
// this call statement corresponding to the input argument number, and mark that
// argument as an array remapping candidate that has been malloced.
static void mark_argument_array_remapping_candidate_malloc(WN *wn,
  int argument_num, IPA_NODE *callee)
{
  if (wn == NULL || argument_num < 0 || callee == NULL)
    return;
  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        mark_argument_array_remapping_candidate_malloc(child_wn, argument_num,
          callee);
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
          mark_argument_array_remapping_candidate_malloc(child_wn, argument_num,
            callee);
      }
    }
  }
  switch (WN_operator(wn))
  {
    case OPR_CALL:
      if (callee->Func_ST() == WN_st(wn) &&
          WN_operator(WN_kid(wn, argument_num)) == OPR_PARM &&
          WN_operator(WN_kid0(WN_kid(wn, argument_num))) == OPR_LDA)
        // it's the right call, and &argument is passed (inside the callee we
        // have *argument = malloc()
        Set_ST_is_array_remapping_candidate_malloc(WN_st(WN_kid0(WN_kid(wn,
          argument_num))));
      break;

    default:
      break;
  }
  return;
}

// Visit the input wn and look for a call to "callee".  Locate the argument in
// this call statement corresponding to the input argument number, and see if
// that argument is an array remapping candidate that has been malloced.  This
// function returns TRUE if the above is true for all such calls in the input
// wn.
BOOL argument_in_wn_is_array_remapping_candidate_malloc(WN *wn,
  int argument_num, IPA_NODE *callee, BOOL *found_a_call)
{
  BOOL result;

  // found_a_call is meant to be checked when the entire function wn tree has
  // been processed, not after each recursive call (because it is not reasonable
  // to expect each block of wn's to contain such a call)

  if (wn == NULL || argument_num < 0 || callee == NULL)
    return FALSE;
  result = TRUE;
  if (!OPCODE_is_leaf(WN_opcode(wn)))
  {
    if (WN_opcode(wn) == OPC_BLOCK)
    {
      WN *child_wn = WN_first(wn);
      while (child_wn != NULL)
      {
        result = result && argument_in_wn_is_array_remapping_candidate_malloc
          (child_wn, argument_num, callee, found_a_call);
        if (result == FALSE)
          return FALSE; // no need to continue
        child_wn = WN_next(child_wn);
      }
    }
    else
    {
      INT child_num;
      WN *child_wn;
      for (child_num = 0; child_num < WN_kid_count(wn); child_num++)
      {
        child_wn = WN_kid(wn, child_num);
        if (child_wn != NULL)
        {
          result = result && argument_in_wn_is_array_remapping_candidate_malloc
            (child_wn, argument_num, callee, found_a_call);
          if (result == FALSE)
            return FALSE; // no need to continue
        }
      }
    }
  }
  switch (WN_operator(wn))
  {
    case OPR_CALL:
      if (callee->Func_ST() == WN_st(wn))
      {
        // right call
        *found_a_call = TRUE;
        if (WN_operator(WN_kid(wn, argument_num)) == OPR_PARM &&
            WN_operator(WN_kid0(WN_kid(wn, argument_num))) == OPR_LDID)
          return ST_is_array_remapping_candidate_malloc(WN_st(WN_kid0(WN_kid(wn,
            argument_num))));
        else
          return FALSE; // something's wrong
      }
      break;

    default:
      break;
  }
  return result;
}

// Given a node (e.g. "foo") in the call graph and an argument number, this
// function returns TRUE if the corresponding argument of *all* the callers of
// foo is an array remapping candidate that has been malloced.  (Usage note:  if
// this function returns TRUE, then the caller of this function can mark the
// array corresponding to the argument number as an array remapping candidate
// that has been malloced also.)
BOOL argument_in_callers_is_array_remapping_candidate_malloc(IPA_NODE *node,
  int argument_num)
{
  BOOL result;
  BOOL found_a_caller;
  BOOL found_a_call;

  if (node == NULL || argument_num < 0)
    return FALSE;
  result = TRUE;
  found_a_caller = FALSE;
  // visit all the callers of node
  IPA_PRED_ITER pred_iter(node);
  for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next())
  {
    IPA_EDGE *edge = pred_iter.Current_Edge();
    if (edge != NULL)
    {
      IPA_NODE *caller = IPA_Call_Graph->Caller(edge);
      IPA_NODE_CONTEXT context(caller);
      found_a_caller = TRUE;
      WN *wn_tree = WN_func_body(caller->Whirl_Tree());
      // look for all the calls to node inside this wn_tree, and see if the
      // corresponding argument of all these calls is an array remapping
      // candidate that has been malloced
      result = result && argument_in_wn_is_array_remapping_candidate_malloc
        (wn_tree, argument_num, node, &found_a_call);
      if (result == FALSE)
        return FALSE; // no need to continue
      if (found_a_call == FALSE)
        return FALSE; // something is wrong:  the call graph says there is a
                      // call to node, but no such call was found
    }
  }
  return (result && found_a_caller); // to prevent returning TRUE when there are
                                     // no callers at all
}
