/*
 * Copyright (C) 2009-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *
 * Copyright (C) 2006, 2007, Tsinghua University.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement
 * or the like.  Any license provided herein, whether implied or
 * otherwise, applies only to this software file.  Patent licenses, if
 * any, provided herein do not apply to combinations of this program with
 * other software, or any other product whatsoever.
 *
 * For further information regarding this notice, see:
 * http://hpc.cs.tsinghua.edu.cn
 *
 */


#include <queue>
#include <ext/hash_map>
#include <ext/hash_set>
#include <list>
#include <set>
#include <algorithm>
#include <iterator>
#include <string>
#include "symtab.h"
#include "wn_util.h"
#include "wn_lower.h"
#include "ir_reader.h"
#include "ipo_defs.h"
#include "ipo_parent.h"
#include "ipl_summarize.h"
#include "ipl_summarize_util.h"
#include "ipc_symtab_merge.h"
#include "ipc_ty_hash.h"
#include "ipa_chg.h"
#include "ipa_trace.h"
#include "ipa_devirtual.h"

using std::queue;
using std::list;
using std::set;
using std::string;
using __gnu_cxx::hash_map;
using __gnu_cxx::hash_set;

// The candidates for conversion to direct call
typedef struct {
    SUMMARY_CALLSITE *callsite;  // the callsite
    TY_INDEX actual_class;       // the actual class it should be converted to
} CONVERSION_CANDIDATE;

#define SUBCLASS_UNSET 0
#define SUBCLASS_MORE_THAN_ONE (TY_INDEX)-1

// all instantiated classes and its corresponding constructor
// one constructor is enough to one class because we just use the vptr setting info
static hash_map <TY_INDEX, PU_IDX> live_class;

// base_sub_map stores a base has how many instantiated subclasses
// if one, map the type of its subclass;
// if zero, map SUBCLASS_UNSET;
// if more than one, map SUBCLASS_MORE_THAN_ONE
static hash_map <TY_INDEX, TY_INDEX> base_sub_map;

// map PU to NODE_INDEX
static hash_map <PU_IDX, NODE_INDEX> pu_node_index_map;

// map ST to INITO index
static hash_map <ST_IDX, INITO_IDX> st_inito_map;

// conversion_list stores the virtual function calls that need to be converted.
static queue <CONVERSION_CANDIDATE> conversion_list;

/*
 * Find the corresponding INITO entry that stores vtable by ST index.
 */
INITO_IDX
Find_inito_by_st(ST_IDX st_idx)
{
    if (st_inito_map.find(st_idx) == st_inito_map.end()) {
        UINT inito_count = INITO_Table_Size(GLOBAL_SYMTAB);
        for (UINT i = 1; i < inito_count; i++) {
            INITO_IDX idx = make_INITO_IDX(i, GLOBAL_SYMTAB);
            if (INITO_st_idx(Inito_Table[idx]) == st_idx) {
                st_inito_map[st_idx] = idx;
                return idx;
            }
        }
        st_inito_map[st_idx] = INITO_IDX_ZERO;
        return INITO_IDX_ZERO;
    }
    return st_inito_map[st_idx];
}

/*
 *  Find the ST_IDX of a virtual function
 *  by the class type and its offset in virtual table.
 *  actual_class    : the class which the actual member function belongs to
 *  formal_class    : explicit class type of the member function in the callsite
 *  formal_field_id : explicit field id in the callsite
 *  func_offset     : the position of target function call in vtable
 */
ST_IDX
Find_virtual_function(TY_IDX actual_class,
                      TY_IDX formal_class,
                      UINT64 formal_vptr_offset,
                      size_t func_offset)
{
    PU_IDX pu_idx = live_class[TY_IDX_index(actual_class)];
    IPA_NODE *constructor = IPA_Call_Graph->Node(pu_node_index_map[pu_idx]);
    IPA_NODE_CONTEXT context(constructor);
    WN *wn_start = constructor->Whirl_Tree(TRUE);
    WN *vtab = NULL;
    size_t ancestor_offset = 
        IPA_Class_Hierarchy->Get_Ancestor_Offset(
          TY_IDX_index(actual_class),
          TY_IDX_index(formal_class));

    Is_True(ancestor_offset != BASE_CLASS_NOT_FOUND,
            ("Wrong ancestor class."));

    // Traverse the constructor of the actual class
    // and find the statement that sets the value of vtable.
    for (WN_ITER *wni = WN_WALK_StmtIter(wn_start);
            wni != NULL; wni = WN_WALK_StmtNext(wni))
    {
        WN *wn = WN_ITER_wn(wni);
        if (WN_operator(wn) == OPR_ISTORE) {
            WN *rhs = WN_kid0(wn);
            WN *lhs = WN_kid1(wn);
            UINT64 add_ofst = 0;
            if (WN_operator(lhs) == OPR_ADD) {
                add_ofst = WN_const_val(WN_kid1(lhs));
                lhs = WN_kid0(lhs);
            }
            if (WN_operator(rhs) == OPR_LDA && WN_operator(lhs) == OPR_LDID) {
                TY_IDX this_class_ptr = ST_type(WN_st(lhs));
                if (TY_kind(this_class_ptr) != KIND_POINTER)
                    continue;
                TY_IDX this_class = TY_pointed(this_class_ptr);
                if (TY_IDX_index(this_class) == TY_IDX_index(actual_class)) {
                    // Find the field by the statement in constructor
                    UINT64 ofst = WN_store_offset(wn) + add_ofst;
                    // If they are matched, this statement indicates the vtable of the actual class
                    if (ofst == formal_vptr_offset + ancestor_offset) {
                        vtab = wn;
                        break;
                    }
                }
            }
        }
    }

    // If no statement found, skip this callsite
    if (vtab == NULL)
        return ST_IDX_ZERO;

    // pos is the WN of vtable
    WN *pos = WN_kid0(vtab);
    // vtab_st is the ST of vtable
    ST *vtab_st = WN_st(pos);
    // base_offset is the offset of vtable
    UINT32 base_offset = WN_lda_offset(pos);

    // Find the INITO entry via ST index
    INITO_IDX inito_idx = Find_inito_by_st(ST_st_idx(vtab_st));
    if (inito_idx <= INITO_IDX_ZERO)
      return ST_IDX_ZERO;

    // Get the INITV entry, then find the target function call
    INITV_IDX initv_idx = INITO_val(inito_idx);
    // The position of target function call in INITO entry
    // is at func_offset after base_offset.
    UINT32 offset = base_offset + func_offset;

    if (initv_idx <= INITV_IDX_ZERO)
        return ST_IDX_ZERO;
    do {
        if (offset == 0) {
#ifdef TARG_IA64
            Is_True(INITV_kind(initv_idx) == INITVKIND_SYMIPLT,
                    ("Wrong INITV for virtual function."));
#else
            Is_True(INITV_kind(initv_idx) == INITVKIND_SYMOFF,
                    ("Wrong INITV for virtual function."));
#endif
            return INITV_st(initv_idx);
        }
        switch (INITV_kind(initv_idx)) {
        case INITVKIND_BLOCK:
            initv_idx = INITV_blk(initv_idx);
            break;
        case INITVKIND_VAL:
        case INITVKIND_ZERO:
            offset -= Pointer_Size;
            initv_idx = INITV_next(initv_idx);
            break;
        case INITVKIND_SYMOFF:
            offset -= Pointer_Size;
            initv_idx = INITV_next(initv_idx);
            break;
#ifdef TARG_IA64
        case INITVKIND_SYMIPLT:
            offset -= (Pointer_Size << 1);
            initv_idx = INITV_next(initv_idx);
            break;
#endif
        default:
            Is_True(FALSE, ("Unexcepted INITV kind."));
        }
    } while (initv_idx > INITV_IDX_ZERO);
    Is_True(FALSE, ("Initv entry not found."));
    return ST_IDX_ZERO;
}


// if the function is constructor, return its base class type and set sym_pu as its PU_IDX.
TY_INDEX
Is_constructor(SUMMARY_SYMBOL *func_sym, PU_IDX *sym_pu)
{
    ST_IDX func_st_idx = func_sym->St_idx();
    ST *func_st = &St_Table[func_st_idx];
    PU_IDX pui = ST_pu(func_st);
    if (PU_is_constructor(Pu_Table[pui])) {
        TY_IDX class_ty = PU_base_class(Pu_Table[pui]);
        Is_True(TY_kind(class_ty) == KIND_STRUCT, ("Wrong base class."));
        *sym_pu = pui;
        return TY_IDX_index(class_ty);
    }
    return TY_IDX_ZERO;
}

// Collect the instances of classes
void
IPA_collect_class_instances()
{
    hash_set <TY_INDEX> invoked_base_classes;
    IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);
    // Traverse the call graph
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next() ) {
        IPA_NODE *method = cg_iter.Current();
        if (method == NULL)
            continue;

        PU_IDX pui = ST_pu(method->Func_ST());
        pu_node_index_map[pui] = method->Node_Index();

        invoked_base_classes.clear();

        SUMMARY_PROCEDURE* method_summary = method->Summary_Proc();
        SUMMARY_CALLSITE* callsite_array =
            IPA_get_callsite_array(method) + method_summary->Get_callsite_index();
        int count = method_summary->Get_callsite_count();
        while (count > 0) {
            if (!callsite_array->Is_func_ptr() && !callsite_array->Is_intrinsic()) {
                SUMMARY_SYMBOL *sym = IPA_get_symbol_array(method) +
                                      callsite_array->Get_symbol_index();
                // if sym is a constructor, add it into live_class
                PU_IDX sym_pu;
                TY_INDEX new_class = Is_constructor(sym, &sym_pu);
                if (new_class) {
                    TY_INDEX bclass = TY_IDX_index(PU_base_class(Pu_Table[pui]));
                    // If new_class is the base class of the class of this consturctor,
                    // its must be invoked by this constructor once.
                    // If new_class occurs more than once, it must be live.
                    if (IPA_Class_Hierarchy->Is_Sub_Class(new_class, bclass) &&
                            invoked_base_classes.find(new_class) == invoked_base_classes.end()) {
                        invoked_base_classes.insert(new_class);
                    } else {
                        live_class[new_class] = sym_pu;
                    }
                }
            }
            callsite_array++;
            count--;
        }
    }
}

/*
 *  If there is only one instantiated class in base and all its subclass,
 *  return this instantiated class.
 *  Otherwise, return SUBCLASS_UNSET (no instantiated class) or SUBCLASS_MORE_THAN_ONE.
 */
TY_INDEX
Class_has_subclass(TY_INDEX base)
{
    if (base_sub_map.find(base) == base_sub_map.end()) {
        TY_INDEX live_sub = SUBCLASS_UNSET;
        for (hash_map <TY_INDEX, PU_IDX>::const_iterator iter
                = live_class.begin();
                iter != live_class.end(); iter++)
        {
            TY_INDEX sub = iter->first;
            if (IPA_Class_Hierarchy->Is_Ancestor(base, sub)) {
                if (live_sub == SUBCLASS_UNSET) {
                    live_sub = sub;
                }
                else if (live_sub != SUBCLASS_MORE_THAN_ONE) {
                    live_sub = SUBCLASS_MORE_THAN_ONE;
                    break;
                }
            }
        }
        base_sub_map[base] = live_sub;
    }
    return base_sub_map[base];
}

/*
 * Convert the virtual functions in conversion_list.
 * They must be in the same PU node that method indicates.
 */
void
Convert_virtual_call(IPA_NODE *method)
{
   // Left by earlier developer.

    // wn_map maps the id of WN to WN
    hash_map <WN_MAP_ID, WN *> wn_map;
    IPA_NODE_CONTEXT context(method);

    WN *wn_start = method->Whirl_Tree(TRUE);
    Is_True(wn_start, ("Whirl node is empty."));

    // Traverse the PU and map all virtual function callsites.
    wn_map.clear();
    for (WN_ITER *wni = WN_WALK_StmtIter(wn_start);
            wni != NULL; wni = WN_WALK_StmtNext(wni))
    {
        WN *wn = WN_ITER_wn(wni);
        if (WN_Call_Is_Virtual(wn))
            wn_map[WN_map_id(wn)] = wn;
    }

    // process all candidates in conversion_list
    while (!conversion_list.empty()) {

        // get the front node in conversion_list and process it
        CONVERSION_CANDIDATE cand = conversion_list.front();
        conversion_list.pop();

        // get the corresponding WN to the candidate
        WN_MAP_ID map_id = cand.callsite->Get_map_id();
        Is_True(wn_map.find(map_id) != wn_map.end(), ("No corresponding whirl node."));
        WN *old_wn = wn_map[cand.callsite->Get_map_id()];

        SUMMARY_CALLSITE *callsite = cand.callsite;
        // the explicit formal class in callsite
        TY_IDX orig_class = callsite->Get_virtual_class();
        // the offset of vtable in callsite
        size_t func_offset = callsite->Get_vtable_offset();
        // the field id in callsite
        UINT64 vptr_ofst = callsite->Get_vptr_offset();
        // find the actual target function of this virtual function callsite
        ST_IDX callee_st_idx =
            Find_virtual_function(make_TY_IDX(cand.actual_class),
                                  orig_class, vptr_ofst, func_offset);

        // fail to find the actual function, skip the callsite
        if (callee_st_idx == ST_IDX_ZERO) {
            DevWarn("Find virtual function for class %s (offset %lu) failed.",
                    &Str_Table[TY_name_idx(make_TY_IDX(cand.actual_class))],
                    (long)func_offset);
            continue;
        }

        // gerenate the new direct call instead of the old virtual function call
        ST *callee_st = ST_ptr(callee_st_idx);
        WN *new_wn = WN_generic_call(OPR_CALL, WN_rtype(old_wn),
                                     WN_desc(old_wn), WN_kid_count(old_wn)-1, callee_st);
        for (size_t j = 0; j < WN_kid_count(new_wn); j++) {
            WN_kid(new_wn, j) = WN_kid(old_wn, j);
        }
        WN_set_map_id(new_wn, WN_map_id(old_wn));
        WN_set_flag(new_wn, WN_flag(old_wn));
        WN_Reset_Call_Is_Virtual(new_wn);
        WN *parent = WN_Get_Parent(old_wn, method->Parent_Map(), method->Map_Table());
        WN_Set_Parent(new_wn, parent, method->Parent_Map(), method->Map_Table());
        WN_INSERT_BlockAfter(parent, old_wn, new_wn);
        WN_EXTRACT_FromBlock(parent, old_wn);

        // update summary info
        for (IPA_ICALL_LIST::iterator iter = method->Icall_List().begin();
                iter != method->Icall_List().end(); iter++)
        {
            if ((*iter)->Callsite() == callsite) {
                method->Icall_List().erase(iter);
                break;
            }
        }
        callsite->Reset_icall_target();
        callsite->Reset_func_ptr();
        callsite->Set_param_count(WN_num_actuals(new_wn));
        callsite->Set_return_type(WN_rtype(new_wn));
        callsite->Set_callsite_freq();
        callsite->Set_probability(-1);
        callsite->Set_symbol_index(0);

        IPA_NODE *calle =
            IPA_Call_Graph->Node(pu_node_index_map[ST_pu(callee_st)]);

        //calle->Summary_Proc()->Clear_Never_Invoked_ah();

        // update call graph
        IPA_EDGE* edge = IPA_Call_Graph->Add_New_Edge(callsite,
                         method->Node_Index(),
                         pu_node_index_map[ST_pu(callee_st)]);

        // print debug info
        DevWarn("Convert indirect call (class %s, offset %lu) to direct call %s (class %s) in function %s.",
                &Str_Table[TY_name_idx(orig_class)],
                (long)func_offset, &Str_Table[ST_name_idx(*callee_st)],
                &Str_Table[TY_name_idx(make_TY_IDX(cand.actual_class))],
                &Str_Table[ST_name_idx(*(method->Func_ST()))]);
    }
}


static BOOL
Is_Return_Store_Stmt (WN * wn)
{
    if (wn && WN_operator (wn) == OPR_STID)
    {
        WN *val = WN_kid (wn, 0);
        if (WN_operator (val) == OPR_LDID)
        {
            ST *st = WN_st (val);
            if (ST_sym_class (st) == CLASS_PREG
                    && (st == Return_Val_Preg))
                return TRUE;
        }
    }
    return FALSE;
}

/*
 *  Main function of devirtualizaton.
 */

void
IPA_devirtualization()
{ 

    live_class.clear();
    base_sub_map.clear();
    pu_node_index_map.clear();
    st_inito_map.clear();

    // collect the liveness infomation of all classes
    IPA_collect_class_instances();

    // Traverse the call graph and find the candidates of devirtualization
    IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE *method = cg_iter.Current();
        if (method == NULL)
            continue;

        // get the callsites infomation
        SUMMARY_PROCEDURE* method_summary = method->Summary_Proc();
        SUMMARY_CALLSITE* callsite_array =
            IPA_get_callsite_array(method) + method_summary->Get_callsite_index();
        int count = method_summary->Get_callsite_count();

        Is_True(conversion_list.empty(), ("Conversion list is not empty."));
        while (count > 0) {
            // only process virtual function callsites
            if (callsite_array->Is_virtual_call()) {
                // the formal class indicated by callsite
                TY_INDEX method_class = TY_IDX_index(callsite_array->Get_virtual_class());
                // check if the formal class has only one instantiated sub(sub)class
                TY_INDEX actual_class = Class_has_subclass(method_class);
                if (actual_class != SUBCLASS_UNSET && actual_class != SUBCLASS_MORE_THAN_ONE) {
                    // add the callsite into the candidate list
                    CONVERSION_CANDIDATE cand;
                    cand.callsite = callsite_array;
                    cand.actual_class = actual_class;
                    conversion_list.push(cand);
                }
            }
            callsite_array++;
            count--;
        }

        // If there exists devirtualization candidates in the PU node,
        // convert them to direct function calls.
        if (!conversion_list.empty())
            Convert_virtual_call(method);
    }
}

/*
------------------
The optimization is enabled if the following variable is true:
   common/com/config_ipa.cxx BOOL 
   IPA_Enable_fast_static_analysis_VF variable is TRUE
-------------------------------
Effect of pass on compiled code:
-------------------------------
The pass changes the WHIRL at a virtual 
call site to look like the following:
1: At the virtual function call site an 
if-then-else structure is introduced.
    Example 1:
        Assumme virtual function call bar->foo(...). 
        After the pass, a if-then-else 
        structure wraps around this virtual function call.
        if (bar->foo()'s class type is CLS)
            CLS::foo(bar)
        else
            bar->foo()
2: A new edge is added in the call graph with the inferred callee.
    Example 2:
        Take the previous example, Example 1. 
        An edge from the caller that 
        contains bar->foo() is added to CLS::foo(bar)
-------------------------------
Description and psuedo code:
-------------------------------
This is the file osprey/ipa/main/analyze/ipa_devirtual.cxx 
containing most of the devirtualization work. 
All of the pass is done through a class called 
IPA_VIRTUAL_FUNCTION_TRANSFORM in this file, ipa_devirtual.cxx.
This file, ipa_devirtual.cxx file has been left 
by the earlier developer. 
The addition now is the class IPA_VIRTUAL_FUNCTION_TRANSFORM
and the function IPA_Fast_Static_Analysis_VF. 
But most of the member functions in IPA_VIRTUAL_FUNCTION_TRANSFORM 
borrow code or idea directly from the existing functions 
left by the earlier developer.

psuedo code for IPA_Fast_Static_Analysis_VF:
IPA_Fast_Static_Analysis_VF() {
    if IPA_Enable_Fast_Static_Analysis_VF() {

     Prepare_Virtual_Function_Transform() which contains 
       {
 code to construct the class inheritance graph 
        Build_Class_Hierarchy() ; // ipa/main/analyze/ipa_chg.cxx ipa_chg.h
        Identify_Constructors(); // Gets all constructors from call graph
 Note:Since a constructor is called at instantiation time, the
 effect is that we collect all instantiations of classes declared in program.
 Constructors are collected by iterating over the nodes in the Call graph. 
 So there is a need for the call graph before this pass.
        }
        Transform_Virtual_Functions()   //  Code to walk over the 
 IPA_NODES in IPA_Call_Graph and check if 
 these functions make any virtual function calls and 
 replace the virtual function call with an oracle 
 if-then-else structure.
 
 Now that all virtual functions for IPA_NODEs are processed, 
 we got to update call node to SUMMARY_CALLSITE object mapping 
 in callsite_array of the IPA_NODE. 
 This is necessary because a new call may have been 
 inserted by the transformation. 
 ipl adds SUMMARY_CALLSITEs for every 
 call node, icall node etc in each of the files being compiled. 
 This data is placed along with the rest of the WHIRLs, symbol tables 
 in the .o object file.
 Every CALL or ICALL WHIRL node must have an associated 
 SUMMARY_CALLSITE so that the call graph can be constructed.
 In our pass, we are going to be inserting an additional call. And
 we don't have a SUMMARY_CALLSITE for it, as SUMMARY_CALLSITEs are added 
 by IPL. It is not feasible to add an 
 additional SUMMARY_CALLSITE into the file being 
 processed right now during IPA. 
 The best idea is to insert the SUMMARY_CALLSITE 
 during IPL. This requires very little effort.
 This is a particular need that is shared by this pass and the
 ICALL transformation pass elsewhere in the ipa/.

 In order to let this all work, 
 IPL has to assume that we will have an additional call for every virtual 
 function after the pass is through, and it inserts a dummy 
 callsite for ALL virtual functions, 
 without knowing if IPA will insert the direct 
 call or not. i.e. we just have the dummy SUMMARY_CALLSITEs 
 without the WHIRL CALL/ICALL node that corresponds to them. 
 Dummy SUMMARY_CALLSITEs are mapped by IPL to WN_MAP_ID value of -1 
 or undefined. Dummy SUMMARY_CALLSITES are also flagged as
 callsite_targets. 
 There are some more call graph issues due to the linkage between callsites 
 and call whirl nodes.  An integer mapping is maintained between 
 callsites and WN_MAP_ID of a call WHIRL Node.
 It is imperative to also fix up this mapping after we insert 
 a call WHIRL node. i.e. watch out if there a clean mapping, 
 or some pass will crash.
 
 Fixup_Virtual_Function_Transform basically renumbers 
 SUMMARY_CALLSITEs, i.e fixes up the 
 mapping between callsites and call nodes.
 
 The reference code fix_up_only is in Add_Edges_For_Node 
 function called by Build_Call_Graph.       
        Fixup_Virtual_Function_Callsites()
transformation work is done
    }
}

pseudo code for Transform_Virtual_Functions():
This function wraps around a 
Transform_Virtual_Functions_Per_Node
which does the real work
Transform_Virtual_Functions_Per_Node() {
    For each virtual call in a IPA_Node, 
    map its WN_Map_ID to the whirl node,
    make_VF_wn_map(...);
    if there are VFs, walk over the callsites in the IPA_Node and 
    analyze and transform
    count = callsite_count(IPA_Node)
    while(count > 0) {
        count --;
        if the callsite is a virtual function dummy callee inserted
        by ipl, the next callsite will be the virtual function
        if (callsite[count] is a dummy_callee) {
            dummy_callsite = callsite[count]
            count--
            virtual_function_call = callsite[count]
            get class instantiations of classes rooted at the declared class of virtual_function_call
            if the number of such class instantiations is 1, we have a pass for heuristic 2
            class_instantiations uses constructor data collected earlier
            instances = class_instantiations(type(virtual_function_class))

            if we want class_hierarchy_analysis:
            if num_subclasses(type(virtual_function_class)) == 0 {
                heuristic 1 pass that enables Class Hierarchy Analysis transformation
                add Virtual function, and its dummy_callsite for 
                optimization
            }
            else if we want class type analysis: 
                if length(instances) == 1 {
                heuristic 2 pass that enables Class Type Analysis transformation
                add Virtual function, and its dummy_callsite for 
                optimization 
            }
        }
    }

    if the loop marked virtual functions in the IPA_Node, apply transformation on them
    for all VIRTUAL_FUNCTION_CANDIDATEs{
       Apply_Virtual_Function_Transform ();
    }
}
pseudo code for Apply_Virtual_Function_Transform():
Apply_Virtual_Function_Transform() {
    Change the code to look like Example 1.
    insert the call from the if-then part into 
    IPA_Call_Graph by using dummy callsite as 
    the callsite argument to IPA_CALL_GRAPH::Add_New_Edge
    There are some miscellaneous profiling/test/assertions here.
    If you are debugging use them otherwise disable them 
    during release time.
}
*/

void 
IPA_Fast_Static_Analysis_VF ()
{
    IPA_VIRTUAL_FUNCTION_TRANSFORM vf_transform;
    vf_transform.Initialize_Virtual_Function_Transform ();
    vf_transform.Prepare_Virtual_Function_Transform ();
if(IPA_Enable_New_VF)
    vf_transform.Build_Virtual_Function_Overridden_Map();
    vf_transform.Dump_Constructors();
    vf_transform.Transform_Virtual_Functions ();
    vf_transform.Fixup_Virtual_Function_Callsites ();
    vf_transform.Finalize_Virtual_Function_Transform ();
    vf_transform.Print_Statistics ();
    vf_transform.Dump_Virtual_Function_Transform_Candidates ();
    vf_transform.Histogram_Statistics ();
    vf_transform.Miss_Hit_Profile ();
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Initialize_Virtual_Function_Transform ()
{
    Enable_Debug = Get_Trace(TP_IPA, IPA_TRACE_ICALL_DEVIRTURAL);
    if (Enable_Debug == true) {
        Virtual_Whirls = fopen ("Virtual_Whirls.log", "w");
        Transformed_Whirls = fopen ("Tranformed_Whirls.log", "w");
        Callsite_Dump_file = fopen ("Callsite_Dump_.log", "w");
        Enable_Statistics = true;
    } else {
        Virtual_Whirls = NULL;
        Transformed_Whirls = NULL;
        Callsite_Dump_file = NULL;
        Enable_Statistics = false;
    }

    Read_Callsite(); // get callsite devirtualization spec from file

    Enable_Profile = false;
    // In case profiling is enabled locate the global STs 
    // __MISS_VIRTUAL_FUNCTION__ and __HIT_VIRTUAL_FUNCTION__
    if (Enable_Profile == true) {
        Miss_ST_IDX = ST_IDX_ZERO;
        Hit_ST_IDX = ST_IDX_ZERO;
        for (ST_IDX sts = 1; sts < ST_Table_Size(GLOBAL_SYMTAB) ; ++sts) {
            ST* cur_st = &St_Table(GLOBAL_SYMTAB,sts);
            typedef mUINT32 INDEX;
            INDEX ty_index = cur_st->u2.type>>8;
            if (Ty_tab[ty_index].kind==KIND_ARRAY){ 
                if (strcmp(ST_name(cur_st), 
                            "__MISS_VIRTUAL_FUNCTION__") == 0) {
                    Miss_ST_IDX = sts;
                } else if (strcmp(ST_name(cur_st), 
                            "__HIT_VIRTUAL_FUNCTION__") == 0) {
                    Hit_ST_IDX = sts;
                }
            }
            if (Hit_ST_IDX != ST_IDX_ZERO && Miss_ST_IDX != ST_IDX_ZERO)
                break;
        }
    } else {
        Miss_ST_IDX = ST_IDX_ZERO;
        Hit_ST_IDX = ST_IDX_ZERO;
    }
    
    Num_VFs_Count = 0;
    Class_Hierarchy_Transform_Count = 0;
    Class_Instance_Transform_Count = 0;
    // build PU -> NODE_INDEX mapping
    Build_PU_NODE_INDEX_Map();
    // disable or enable heuristics using these booleans
    Class_Hierarchy_Analysis = true;
    Class_Type_Analysis = false;
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Finalize_Virtual_Function_Transform ()
{
    if (Enable_Debug == true) {
        fclose(Virtual_Whirls);
        fclose(Transformed_Whirls);
        fclose(Callsite_Dump_file);
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Build_PU_NODE_INDEX_Map ()
{
    IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE *method = cg_iter.Current();
        if (method == NULL) 
            continue;
        PU_IDX pui = ST_pu(method->Func_ST());
    
        BOOL is_cxx = PU_cxx_lang(method->Get_PU());
        if (!is_cxx)
            continue;
    
        Is_True(pui >= PU_IDX_ZERO && pui < PU_Table_Size(), 
                ("PUI index:%d not in range:%d <= indx < %d\n",
                 pui, PU_IDX_ZERO, PU_Table_Size()));
    
        Is_True (pu_node_index_map.find(pui) == 
                 pu_node_index_map.end(), 
                 ("PU_IDX already in pu_node_index_map; %d -> %d; newly:%d -> %d\n",
                  pui, pu_node_index_map[pui], 
                  pui, method->Node_Index()));
        pu_node_index_map[pui] = method->Node_Index();
    }
}

int IPA_VIRTUAL_FUNCTION_TRANSFORM::Get_Callsite_Count (IPA_NODE* method)
{
    SUMMARY_PROCEDURE* method_summary = method->Summary_Proc();
    SUMMARY_CALLSITE* callsite_array =
        IPA_get_callsite_array(method) + 
        method_summary->Get_callsite_index();
    int count = method_summary->Get_callsite_count();
    return count;
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Dump_Callsite( IPA_NODE *method,
                                                    SUMMARY_CALLSITE* callsite,
                                                    WN_IPA_MAP& wn_map, 
                                                    bool devirtualized,
                                                    ST_IDX callee_st_idx)
{
  //
  if (Callsite_Dump_file == NULL)
    return;
  if (callsite->Is_virtual_call() ) {
    WN *wn = wn_map[callsite->Get_map_id()];

    Is_True(wn != NULL, ("WN node is empty for the virtual function call"));
    fprintf(Callsite_Dump_file, "Caller:%s line:%d callsite_id:%d",
              method->Name(), Srcpos_To_Line(WN_linenum(wn)),
              callsite->Get_callsite_id() );
    fprintf(Callsite_Dump_file, " vtable_offset:%d vptr_offset:%d", 
            callsite->Get_vtable_offset(), (int)callsite->Get_vptr_offset());
    if (devirtualized)
       fprintf(Callsite_Dump_file, " devirtualized callee:%s",
               ST_name(ST_ptr(callee_st_idx)));
    fprintf(Callsite_Dump_file, "\n");
  }
}

static IPA_VIRTUAL_FUNCTION_TRANSFORM::DevirCallsiteMap theDevirCallsiteMap;

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Read_Callsite()
{
   if (IPA_Devirtualization_Input_File) {
      FILE *fp = fopen(IPA_Devirtualization_Input_File, "r");
      if (fp == NULL) {
         fprintf(stderr, "Invalid DEVIR_CALLSITE file %s!\n", IPA_Devirtualization_Input_File);
         return;
      }
      size_t sz = 4096;
      char *linebuf = (char*)malloc(sz);
      char buffer[2048];
      int lineno;
      int callsite_id;
      while (getline(&linebuf, &sz, fp) != -1) {
         if (*linebuf == '#') {
            // comment line starts with #
            continue;
         }
         if ( sscanf(linebuf, "Caller:%s line:%d callsite_id:%d",
                     buffer, &lineno, &callsite_id) == 3) {
            DevirCallsiteInfo *p = new DevirCallsiteInfo(buffer, lineno, callsite_id);
            DevirCallsiteMap::iterator itr = theDevirCallsiteMap.find(buffer);
            if (itr == theDevirCallsiteMap.end()) {
               // create a new entry with a new vector
                  vector<DevirCallsiteInfo *> *v = new vector<DevirCallsiteInfo *> ( );
               v->push_back(p);
               theDevirCallsiteMap.insert(std::make_pair(strdup(buffer), v));
            }
            else {
               // add the callsite info to the end of the exisiting vector
               itr->second->push_back(p);
            }
         }
#if 0 // def Is_True_On
         printf("Read callsite: Caller:%s line:%d callsite_id:%d\n", 
                 buffer, lineno, callsite_id);
#endif
      }
   }
}

DevirCallsiteInfo *
IPA_VIRTUAL_FUNCTION_TRANSFORM::Find_Callsite(IPA_NODE *method,
                                              SUMMARY_CALLSITE* callsite,
                                              WN_IPA_MAP& wn_map)

{
   Is_True(method && callsite, ("method or callsite empty"));
    DevirCallsiteMap::iterator itr = theDevirCallsiteMap.find(method->Name());
    if (itr == theDevirCallsiteMap.end()) return NULL;
    vector<DevirCallsiteInfo *> *v = itr->second;
    if (v) {
       vector<DevirCallsiteInfo *>::iterator itr = v->begin();
       while (itr != v->end()) {
          DevirCallsiteInfo *d = *itr++;
          if (d->callsite_id == callsite->Get_callsite_id())
             return d;
       }
    }
    return NULL;
}

// Following two functions,Identify_Constructors, Get_Constructor_Type
// are for collecting constructors from call graph
void IPA_VIRTUAL_FUNCTION_TRANSFORM::Identify_Constructors ()
{
    IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE *method = cg_iter.Current();
        if (method == NULL) 
            continue;
    
        BOOL is_cxx = PU_cxx_lang(method->Get_PU());
        if (!is_cxx)
            continue;

        IPA_NODE_CONTEXT context(method);
        PU_IDX pui = ST_pu(method->Func_ST());
        SUMMARY_PROCEDURE* method_summary =
            method->Summary_Proc();

        SUMMARY_CALLSITE* callsite_array =
            IPA_get_callsite_array(method) + 
            method_summary->Get_callsite_index();
        int count = Get_Callsite_Count(method);
        while (count > 0) {
            if (!callsite_array->Is_func_ptr() && 
                !callsite_array->Is_intrinsic()) {
                SUMMARY_SYMBOL *cons_sym = 
                    IPA_get_symbol_array(method) +
                    callsite_array->Get_symbol_index();
                TY_INDEX constructor_class = 
                    Get_Constructor_Type(cons_sym);
                if (constructor_class != TY_IDX_ZERO) {
                    if (Constructor_Map.find(constructor_class) == 
                        Constructor_Map.end()) {
                        ST_IDX func_st_idx = cons_sym->St_idx();
                        ST *func_st = &St_Table[func_st_idx];
                        PU_IDX cons_pui = ST_pu(func_st);
                        Is_True (pui >= PU_IDX_ZERO && pui < PU_Table_Size(),
                                 ("PUI of constructor:%d not in range:%s:%d\n",
                                  pui, "0 <= pui < PU_Table_Size:",
                                  PU_Table_Size()));

                        if (pu_node_index_map.find(cons_pui) !=
                            pu_node_index_map.end()) {
                            Constructed_Types.insert(constructor_class);
                            Constructor_Map[constructor_class] = cons_pui;
                      } else {
                          if (Enable_Debug == true) {
                              if (pui < PU_IDX_ZERO || pui > PU_Table_Size()) 
                                  fprintf (Virtual_Whirls, "IPA_fast_static_analysis_VF:%s %d %s %d",  
                                          "Node's PU_IDX:", pui,
                                          "not in range 0 <= idx <",
                                           PU_Table_Size());
                          }
                      }
                  } // We are required to be omitting automatic base class 
                  // constructor calls. This code does it.
                  // As PU_IS_CONSTRUCTOR flag is not set on 
                  // them by the spin frontend we are automatically 
                  // handling them in the above code.
                  //
                }
            }
            count--;
            callsite_array++;
        }
    }
}

TY_INDEX IPA_VIRTUAL_FUNCTION_TRANSFORM::Get_Constructor_Type (
        SUMMARY_SYMBOL* func_sym)
{
    Is_True((ST_IDX_level(func_sym->St_idx()) <= GLOBAL_SYMTAB),
            ("SUMMARY_SYMBOL for function call not a global Symbol"));

    ST_IDX func_st_idx = func_sym->St_idx();
    ST *func_st = &St_Table[func_st_idx];
    PU_IDX pui = ST_pu(func_st);
    if (PU_is_constructor(Pu_Table[pui])) {
        TY_IDX class_ty = PU_base_class(Pu_Table[pui]);
        Is_True(TY_kind(class_ty) == KIND_STRUCT,
                ("Wrong base class."));
        return TY_IDX_index(class_ty);
    }
    return TY_IDX_ZERO;
}

// This function IPA_VIRTUAL_FUNCTION_TRANSFORM::Prepare_Virtual_Function_Transform 
// 1: builds the class hierarchy, 
// 2: collects constructor calls from the call graph
void IPA_VIRTUAL_FUNCTION_TRANSFORM::Prepare_Virtual_Function_Transform ()
{
    IPA_Class_Hierarchy = Build_Class_Hierarchy (); 
    Identify_Constructors ();
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Build_Virtual_Function_Overridden_Map()
{
    Record_Virtual_Functions_for_Class();
    Mark_Secondary_Bases_Unusable();
    Propagate_Overridden_Info();
    if (Enable_Debug == true) {
       Print_Overridden_Map(stdout);
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Record_Virtual_Functions_for_Class() {
    UINT32 num_type = TY_Table_Size();

    // for every type in Ty_tab
    for (UINT32 i = 1; i < num_type; i++) {
        TY &ty = Ty_tab[i];    
        if (Is_Structure_Type(ty) && !TY_is_union(ty) && ty.Vtable()) {
            if (Enable_Debug == true)
               fprintf(stdout, "Type Name: %s, TY_INDEX: %d\n", TY_name(ty), i);

            Virtual_Functions VFs;
			
            ST_IDX vtab_st_idx = ty.Vtable();
            VFs.Set_class_ty_index(i);
            VFs.Set_vtable_st_idx(vtab_st_idx);

            Is_True (vtab_st_idx != 0, ("invalid vtable index"));
            class_vf_map[i] = VFs;
  
            // Find the INITO entry via ST index
            INITO_IDX inito_idx = Find_inito_by_st(vtab_st_idx);
	
            if(inito_idx == INITO_IDX_ZERO)
            {
                class_vf_map[i].Set_Unusable();
                continue;
            }

            // Get the INITV entry, then find the target function call
            INITV_IDX initv_idx = INITO_val(inito_idx);
            // The position of target function call in INITO entry
            // is at func_offset after base_offset.

            Is_True (initv_idx > INITV_IDX_ZERO, ("vtable is empty"));
			
            if(INITV_kind(initv_idx) == INITVKIND_BLOCK)
                initv_idx = INITV_blk(initv_idx);

            bool start = false;
            UINT32 vf_count = 0;
            UINT32 offset = 0;
            while(initv_idx > INITV_IDX_ZERO)
            {
                if(INITV_kind(initv_idx) == INITVKIND_SYMOFF && ST_class(INITV_st(initv_idx)) == CLASS_FUNC)
                {
                    if(!start)
                    {
                        start = true;
                        class_vf_map[i].Set_vtable_offset(offset * Pointer_Size);
                    }
        
                    Virtual_Function vf(i, INITV_st(initv_idx), vf_count * Pointer_Size);
                    class_vf_map[i].Add_Virtual_Function(vf);
                    vf_count++;
                }
                else if(start)
                {
                    break;
                }
                offset++;
                initv_idx = INITV_next(initv_idx);
            }
            if (Enable_Debug == true)
            {
                //  dump vf info
               fprintf(stdout, "Added %d virtual functions to class_vf_map[%d]\n", vf_count, i);
               class_vf_map[i].Print(stdout);
               fprintf(stdout, "\n");
            }
        }
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Mark_Secondary_Bases_Unusable()
{
    for(CLASS_VF_MAP::iterator iter = class_vf_map.begin(); iter != class_vf_map.end(); iter++)
    {
        if(iter->second.Is_Usable())
        {
            TY_INDEX ty_idx = iter->first;
            Virtual_Functions &vf = iter->second;
            UINT32 base_num = IPA_Class_Hierarchy->Get_Num_Base_Classes(ty_idx);
            if(base_num > 0)
            {
                // check if the first base is virtual base, we cannot
                // handle virtual base either
                if (IPA_Class_Hierarchy->Is_Virtual_Base(ty_idx,
                                               IPA_Class_Hierarchy->Get_Base_Class(ty_idx, 0)))
                    Mark_Self_And_Ancestor_Unusable(IPA_Class_Hierarchy->Get_Base_Class(ty_idx, 0));

                for(int i = 1; i < base_num; i++)
                {
                    Mark_Self_And_Ancestor_Unusable(IPA_Class_Hierarchy->Get_Base_Class(ty_idx, i));
                }
            }
        }

    }
    // if the class has any derived class which doesn't has vtable, 
    // make the base and derived class unusable
    for(CLASS_VF_MAP::iterator iter = class_vf_map.begin(); iter != class_vf_map.end(); iter++)
    {
        if(iter->second.Is_Usable())
        {
            TY_INDEX ty_idx = iter->first;
            TY &base_ty = Ty_tab[ty_idx];    
            FmtAssert(base_ty.Vtable(), ("The vtable should not be empty!"));
            Virtual_Functions &vf = iter->second;
            UINT32 sub_num = IPA_Class_Hierarchy->Get_Num_Sub_Classes(ty_idx);
            if(sub_num > 0)
            {
                for(int i = 0; i < sub_num; i++)
                {
                   TY_INDEX sub_ty_idx = IPA_Class_Hierarchy->Get_Sub_Class(ty_idx, i);
                   TY &sub_ty = Ty_tab[sub_ty_idx];    
                   FmtAssert(sub_ty_idx && Is_Structure_Type(sub_ty) && !TY_is_union(sub_ty), 
                         ("Invalid sub class!"));
                   if (!sub_ty.Vtable()) {
                      // set base unusable in case the sub class is not in class_vf_map
                      iter->second.Set_Unusable();
                      Mark_Self_And_Ancestor_Unusable(IPA_Class_Hierarchy->Get_Sub_Class(sub_ty_idx, i));
                   }
                }
            }
        }

    }

}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Mark_Self_And_Ancestor_Unusable(TY_INDEX declared_class)
{	
    if(IPA_Class_Hierarchy->Get_Num_Base_Classes(declared_class) != 0)
    {
        for(UINT32 i = 0; i < IPA_Class_Hierarchy->Get_Num_Base_Classes(declared_class); i++)
        {
            Mark_Self_And_Ancestor_Unusable(IPA_Class_Hierarchy->Get_Base_Class(declared_class, i));
        }
    }
    if (class_vf_map.find(declared_class) == class_vf_map.end())
       return ;  // the base is not in class_vf_map
    class_vf_map[declared_class].Set_Unusable();
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Propagate_Overridden_Info()
{
    for(IPA_CLASS_HIERARCHY::CLASS_RELATIONSHIP::iterator iter = IPA_Class_Hierarchy->Get_Begin_Sub_Classes();
	iter != IPA_Class_Hierarchy->Get_End_Sub_Classes(); iter++)
    {
        Propagate_Overridden_Info_For_Class(iter->first);
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Propagate_Overridden_Info_For_Class(TY_INDEX declared_class)
{
    if(class_vf_map.find(declared_class) == class_vf_map.end())
        return;

    if(!class_vf_map[declared_class].Is_Usable())
        return;
	
    if(class_vf_map[declared_class].Is_Propagated())
        return;

    if(IPA_Class_Hierarchy->Get_Num_Sub_Classes(declared_class) != 0)
    {
        for(UINT32 i = 0; i < IPA_Class_Hierarchy->Get_Num_Sub_Classes(declared_class); i++)
        {
            Propagate_Overridden_Info_For_Class(IPA_Class_Hierarchy->Get_Sub_Class(declared_class, i));
        }
    
        for(UINT32 i = 0; i < class_vf_map[declared_class].Get_Count(); i++)
        {
            for(UINT32 j = 0; j <IPA_Class_Hierarchy->Get_Num_Sub_Classes(declared_class); j++)
            {
                TY_INDEX sub_class_index = IPA_Class_Hierarchy->Get_Sub_Class(declared_class, j);
                if(class_vf_map.find(sub_class_index) != class_vf_map.end() &&
                     class_vf_map[sub_class_index].Get_Count() > 0 )
                {
                    class_vf_map[declared_class].Add_Overriders(i, 
                          class_vf_map[sub_class_index].Get_Virtual_Function_by_Index(i));
                }
            }
        }
    }
	
    class_vf_map[declared_class].Set_Propagated();
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Print_Overridden_Map(FILE *fp)
{
    IPA_Class_Hierarchy->Print_IPA_CLASS_HIERARCHY();
    for(CLASS_VF_MAP::iterator iter = class_vf_map.begin(); iter != class_vf_map.end(); iter++)
    {
        Virtual_Functions &vf = iter->second;
        if (vf.Get_class_ty_index() != 0)
           vf.Print(fp);
        else
           FmtAssert(0, ("class_ty_index should not be 0!"));
    }
}

UINT32 IPA_VIRTUAL_FUNCTION_TRANSFORM::Candidate_Count(TY_INDEX declared_class, size_t offset)
{
    if(class_vf_map.find(declared_class) == class_vf_map.end() ||
          class_vf_map[declared_class].Get_Count() == 0)
        return INVALID_VIRTUAL_FUNCTION_COUNT;

    if(!class_vf_map[declared_class].Is_Usable())
        return INVALID_VIRTUAL_FUNCTION_COUNT;

    if(class_vf_map[declared_class].Out_Of_Range(offset))
        return INVALID_VIRTUAL_FUNCTION_COUNT;

    return class_vf_map[declared_class].Get_Virtual_Function_by_Offset(offset)->Candidate_count();
}

ST_IDX IPA_VIRTUAL_FUNCTION_TRANSFORM::Get_The_Only_Candidate(TY_INDEX declared_class, size_t offset)
{
    Virtual_Function* p_vf = class_vf_map[declared_class].Get_Virtual_Function_by_Offset(offset);
    return p_vf->Get_The_Only_Candidate();
}


// This function does the crux of the work, find virtual function and transform them
// after heuristic analysis
void IPA_VIRTUAL_FUNCTION_TRANSFORM::Transform_Virtual_Functions ()
{
    IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);

    if(IPA_Enable_Original_VF)
    {
        IPA_During_Original_VF = TRUE;
        IPA_During_New_VF = FALSE;
        for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
            IPA_NODE *method = cg_iter.Current();
            if (method == NULL) 
                continue;
    
            BOOL is_cxx = PU_cxx_lang(method->Get_PU());
            if (!is_cxx)
                continue;
            Transform_Virtual_Functions_Per_Node_ORIG(method);
        }
    }
	
    if(IPA_Enable_New_VF)
    {
        IPA_During_Original_VF = FALSE;
        IPA_During_New_VF = TRUE;
        for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
            IPA_NODE *method = cg_iter.Current();
            if (method == NULL) 
                continue;
    
            BOOL is_cxx = PU_cxx_lang(method->Get_PU());
            if (!is_cxx)
                continue;
            Transform_Virtual_Functions_Per_Node_NEW (method);
        }
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Transform_Virtual_Functions_Per_Node_ORIG (
        IPA_NODE *method)
{
    Is_True ((method != NULL), 
             ("IPA_fast_static_analysis_VF:%s %s\n",
              "Test method != NULL fails in", 
              "Transform_Virtual_Functions_Per_Node"));
    PU_IDX pui = ST_pu(method->Func_ST());
    if (pu_node_index_map.find(pui) == pu_node_index_map.end()) {
        //printf ("Skipping method:pu_node_index_map does not carry pui:%d\n",
        //        pui);
        return;
    }
    WN_IPA_MAP a_wn_ipa_map;
    // This function maps virtual function whirlnodes in a method 
    // to their WN_map_id.
    // if there are no virtual functions in a method, 
    // a_wn_ipa_map will be empty
    Build_Virtual_Function_Whirl_Map (method, a_wn_ipa_map);

    if (a_wn_ipa_map.size() != 0) {
        // we have passed stage_1 test. i.e we have 
        // a virtual function callsite in 'method'. 
        int count = Get_Callsite_Count(method);
        SUMMARY_PROCEDURE* method_summary = method->Summary_Proc();
        SUMMARY_CALLSITE* callsite_array = IPA_get_callsite_array(method) + 
            method_summary->Get_callsite_index();

        list<VIRTUAL_FUNCTION_CANDIDATE> vcands;
        // vf_object_instances is only used for getting debug data
        hash_map <WN_MAP_ID, hash_set<TY_INDEX> > vf_object_instances;

        while (count > 0) {
            // 
            // This is what we expect to do for
            // virtual functions. For each virtual function
            // site, ipl had added a sister call site
            // with a call to a dummy_callee
            // This dummy callsite precedes virtual function 
            // callsite in callsite_array.
            //
            // 1: After this transformation we have an additional CALL
            // instruction in the WHIRL for a 
            // transformed WHIRL node. The dummy callsite
            // is used to insert an edge into the call graph 
            // for this additional CALL instruction. 
            // 2: Many of the VFs may never get through the heuristics.
            // If there are no replacements due to that, 
            // there are going to be unused dummy callsites
            // in the SUMMARY_CALLSITE* array used by
            // the IPA_NODE. 
            // 3: The virtual function ICALL WN* and 
            // the callsite will always remain in WHIRL.
            // 4: It should be noted that it is required to pass
            // a SUMMARY_CALLSITE* to Add_New_Edge in order 
            // to insert IPA_EDGEs into the IPA_CALL_GRAPH. 
            // The actual WN* corresponding to the CALL
            // instruction is not required. Thus we have a 
            // somewhat disjoint representation
            // for the IPA_CALL_GRAPH mainly because the graph
            // is not directly tied to the WHIRL. 
            // 
            // Finally please note that we are decrementing 
            // callsite count variable 2 times in
            // every iteration in case the present 
            // callsite is a dummy callsite
            // 
            SUMMARY_CALLSITE * dummy_cs = callsite_array;
            callsite_array++;
            count--;
            if (count == 0) break;
            if (dummy_cs->Is_virtual_function_target()) {
                if (callsite_array->Is_virtual_call()) {
                    SUMMARY_CALLSITE* callsite = callsite_array;
                    if (Enable_Statistics == true) {
                        Update_Class_Hierarchy_Depth(
                            IPA_Class_Hierarchy->Num_Sub_Class_In_Hierarchy (TY_IDX_index(
                                callsite->Get_virtual_class()))); 
                    }
                    if (a_wn_ipa_map.find(callsite->Get_map_id()) != 
                            a_wn_ipa_map.end()) {
                        bool devirtualized = false;
                        ST_IDX callee_st_idx = ST_IDX_ZERO;
                        hash_set<TY_INDEX> instance_set = 
                            Identify_Instances_From_Subclass_Hierarchy (
                                    TY_IDX_index( 
                                        callsite->Get_virtual_class()));

                        if (Class_Hierarchy_Analysis == true &&
                            (IPA_Class_Hierarchy->Num_Sub_Class_In_Hierarchy (TY_IDX_index(
                                 callsite->Get_virtual_class())) == 1)) {
                            //
                            // this is heuristic 1, 
                            // `Class Hierarchy Analysis'
                            // we check if num_subcls of
                            // the declared_class is == 1, 
                            // i.e. when the declared class 
                            // is the same as the true class of the object.
                            // Declared class is typically the 
                            // type used during argument passing time and true 
                            // class is the type used with the new operator 
                            // at object creation time.
                            VIRTUAL_FUNCTION_CANDIDATE vcand;
                            vcand.Virtual_Table = (WN*) NULL;
                            vcand.Transform_Function_ST_IDX = ST_IDX_ZERO;
                            vcand.Caller = method;
                            Identify_Virtual_Function (instance_set, 
                                        callsite, vcand);
                            // After Identify_Virtual_Function the 
                            // vcand object will be containing the 
                            // infered callee or ST_IDX_ZERO, 
                            // in case of no inference
                            if (vcand.Transform_Function_ST_IDX != 
                                ST_IDX_ZERO) {
                                // Now that we have infered a call target
                                // and added that to vcand, 
                                // we also fill vcand up 
                                // rest of data that is used during 
                                // transformation and finally push vcand into
                                // a list, 'vcands', that holds all transform 
                                // candidates.

                                if (Enable_Statistics == true) {
                                    Class_Hierarchy_Transform_Count++;
                                }
                                vcand.Single_Callee =
                                        (IPA_Enable_Whole_Program_Mode == true);
                                vcand.Virtual_Call_Site = callsite;
                                vcand.Dummy_Call_Site = dummy_cs;
                                vcand.Caller = method;
                                vcands.push_back(vcand);
                                devirtualized = true;
                                callee_st_idx = vcand.Transform_Function_ST_IDX;
                                if (Enable_Debug == true) {
                                    // the set of classes that may been instantiated 
                                    // is stored in vf_object_instances, which is
                                    // used to get debug dumps only
                                    vf_object_instances[callsite->Get_map_id()] = 
                                            instance_set;
                                }
                            }
                        } else if (Class_Type_Analysis == true && (instance_set.size() == 1)) {
                            if (Enable_Statistics == true) {
                                Update_Instances(instance_set.size());
                            }

                            // this is heuristic 2, 
                            // `Class Type Analysis'. We check
                            // if out of a class hierarchy only 
                            // one instance is instantiated ever.
                            VIRTUAL_FUNCTION_CANDIDATE vcand;
                            vcand.Single_Callee = false;
                            vcand.Virtual_Table = (WN*) NULL;
                            vcand.Transform_Function_ST_IDX = ST_IDX_ZERO;
                            Identify_Virtual_Function (instance_set, 
                                        callsite, vcand);
                            // After Identify_Virtual_Function the 
                            // vcand object will be containing the 
                            // infered callee or ST_IDX_ZERO, 
                            // in case of no inference
                            if (vcand.Transform_Function_ST_IDX != 
                                ST_IDX_ZERO) {
                                // Now that we have infered a call target
                                // and added that to vcand, 
                                // we also fill vcand up 
                                // rest of data that is used during 
                                // transformation and finally push vcand into
                                // a list, 'vcands', that holds all transform 
                                // candidates.
                                
                                if (Enable_Statistics == true) {
                                    Class_Instance_Transform_Count++;
                                }
                                vcand.Virtual_Call_Site = callsite;
                                vcand.Dummy_Call_Site = dummy_cs;
                                vcand.Caller = method;
                                vcands.push_back(vcand);
                                devirtualized = true;
                                callee_st_idx = vcand.Transform_Function_ST_IDX;
                                if (Enable_Debug == true) {
                                    vf_object_instances[callsite->Get_map_id()] = 
                                            instance_set;
                                }
                            }
                        }
                        else if(!IPA_Enable_New_VF){
                            // change the dummy callsite to icall target so later 
                            // the icall_process can handle the call
                            dummy_cs->Set_icall_target();
                            dummy_cs->Reset_virtual_function_target();
                        }
                        if (devirtualized || !IPA_Enable_New_VF)
                            Dump_Callsite(method, callsite, a_wn_ipa_map,
                                  devirtualized, callee_st_idx);
                    }
                    callsite_array++;
                    count--;
                }
            }
        }

        // if analysis found some virtual functions
        if (vcands.size() > 0) {
            Node_Transform_Lists[method->Node_Index()] = vcands;
            Node_Virtual_Function_Whirl_Map[method->Node_Index()] = 
                a_wn_ipa_map;
            if (Enable_Debug == true) {
                Transform_Debug_Data[method->Node_Index()] = 
                    vf_object_instances;
            }
            for (list <VIRTUAL_FUNCTION_CANDIDATE>::iterator  vc_cit = 
                vcands.begin(); vc_cit != vcands.end(); ++vc_cit) {
                // We could have done this right after
                // the virtual function ST_IDX was found, 
                // but I find this kind of check pointing useful 
                // while debugging.
                // Obviously this means we double traverse 
                // but that is on a small list and the cost will not 
                // be much.
                VIRTUAL_FUNCTION_CANDIDATE vcand = *vc_cit;
                Apply_Virtual_Function_Transform (vcand);
            }
            if (Enable_Debug) {   
                IPA_NODE_CONTEXT context(method);
                fprintf (Transformed_Whirls, "\n%s after transformation...\n", method->Name());
                fdump_tree (Transformed_Whirls, method->Whirl_Tree(TRUE));
                fprintf (Transformed_Whirls, "Caller->Total_Succ():%d", 
                method->Total_Succ());
                fprintf (Transformed_Whirls, "...done\n");
            }
        }
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Transform_Virtual_Functions_Per_Node_NEW (
        IPA_NODE *method)
{
    Is_True ((method != NULL), 
             ("IPA_fast_static_analysis_VF:%s %s\n",
              "Test method != NULL fails in", 
              "Transform_Virtual_Functions_Per_Node"));
    PU_IDX pui = ST_pu(method->Func_ST());
    if (pu_node_index_map.find(pui) == pu_node_index_map.end()) {
        //printf ("Skipping method:pu_node_index_map does not carry pui:%d\n",
        //        pui);
        return;
    }
    WN_IPA_MAP a_wn_ipa_map;
    // This function maps virtual function whirlnodes in a method 
    // to their WN_map_id.
    // if there are no virtual functions in a method, 
    // a_wn_ipa_map will be empty
    Build_Virtual_Function_Whirl_Map (method, a_wn_ipa_map);

    if (a_wn_ipa_map.size() != 0) {
        // we have passed stage_1 test. i.e we have 
        // a virtual function callsite in 'method'. 
        int count = Get_Callsite_Count(method);
        SUMMARY_PROCEDURE* method_summary = method->Summary_Proc();
        SUMMARY_CALLSITE* callsite_array = IPA_get_callsite_array(method) + 
            method_summary->Get_callsite_index();

        list<VIRTUAL_FUNCTION_CANDIDATE> vcands;
        // vf_object_instances is only used for getting debug data
        hash_map <WN_MAP_ID, hash_set<TY_INDEX> > vf_object_instances;

        while (count > 0) {
            // 
            // This is what we expect to do for
            // virtual functions. For each virtual function
            // site, ipl had added a sister call site
            // with a call to a dummy_callee
            // This dummy callsite precedes virtual function 
            // callsite in callsite_array.
            //
            // 1: After this transformation we have an additional CALL
            // instruction in the WHIRL for a 
            // transformed WHIRL node. The dummy callsite
            // is used to insert an edge into the call graph 
            // for this additional CALL instruction. 
            // 2: Many of the VFs may never get through the heuristics.
            // If there are no replacements due to that, 
            // there are going to be unused dummy callsites
            // in the SUMMARY_CALLSITE* array used by
            // the IPA_NODE. 
            // 3: The virtual function ICALL WN* and 
            // the callsite will always remain in WHIRL.
            // 4: It should be noted that it is required to pass
            // a SUMMARY_CALLSITE* to Add_New_Edge in order 
            // to insert IPA_EDGEs into the IPA_CALL_GRAPH. 
            // The actual WN* corresponding to the CALL
            // instruction is not required. Thus we have a 
            // somewhat disjoint representation
            // for the IPA_CALL_GRAPH mainly because the graph
            // is not directly tied to the WHIRL. 
            // 
            // Finally please note that we are decrementing 
            // callsite count variable 2 times in
            // every iteration in case the present 
            // callsite is a dummy callsite
            // 
            SUMMARY_CALLSITE * dummy_cs = callsite_array;
            callsite_array++;
            count--;
            if (count == 0) break;
            if (dummy_cs->Is_virtual_function_target()) {
                if (callsite_array->Is_virtual_call()) {
                    SUMMARY_CALLSITE* callsite = callsite_array;
                    TY_INDEX class_ty_index = TY_IDX_index(callsite->Get_virtual_class());
                    UINT32 func_offset = callsite->Get_vtable_offset();
                    VIRTUAL_FUNCTION_CANDIDATE vcand;
					
                    if (Enable_Statistics == true) {
                        Update_Class_Hierarchy_Depth(
                            IPA_Class_Hierarchy->Num_Sub_Class_In_Hierarchy (class_ty_index)); 
                    }
                    if (a_wn_ipa_map.find(callsite->Get_map_id()) != 
                            a_wn_ipa_map.end()) {
                        hash_set<TY_INDEX> instance_set = 
                            Identify_Instances_From_Subclass_Hierarchy (class_ty_index);

                        DevirCallsiteInfo *dc = NULL;
                        bool devirtualized = false;
                        ST_IDX callee_st_idx = ST_IDX_ZERO;
                        if (theDevirCallsiteMap.size() > 0) {
                            // get devirtualization candidates from a file which uses
                            // the callsite id to specify which callsite should be
                            // devirtualized. The spec file normally is dumped by
                            // open64 compiler, which guarantees the callsite id is valid
                            dc = Find_Callsite(method, callsite, a_wn_ipa_map);
                            if (Enable_Debug && dc) {
                                fprintf(Virtual_Whirls,
                                        "Found callsite %s line:%d, id:%d to devirtualize\n",
                                         dc->caller, dc->lineno, dc->callsite_id);
                            }
                        }
                        if (Class_Hierarchy_Analysis == true &&
                            callsite->Get_vptr_offset() == 0 && // cannot handle multiple inheritance now  
                            (Candidate_Count(class_ty_index, func_offset) == 1)) {
                            
                            vcand.Virtual_Table_st_idx = class_vf_map[class_ty_index].Get_vtable_st_idx();
                            vcand.Offset = class_vf_map[class_ty_index].Get_vtable_offset();							
                            vcand.Transform_Function_ST_IDX = Get_The_Only_Candidate(class_ty_index, func_offset);
                            vcand.Caller = method;
                            if (vcand.Transform_Function_ST_IDX != 
                                ST_IDX_ZERO) {
                                // Now that we have infered a call target
                                // and added that to vcand, 
                                // we also fill vcand up 
                                // rest of data that is used during 
                                // transformation and finally push vcand into
                                // a list, 'vcands', that holds all transform 
                                // candidates.

                                if (Enable_Statistics == true) {
                                    Class_Hierarchy_Transform_Count++;
                                }
                                vcand.Single_Callee =
                                        (IPA_Enable_Whole_Program_Mode == true);
                                vcand.Virtual_Call_Site = callsite;
                                vcand.Dummy_Call_Site = dummy_cs;
                                vcand.Caller = method;
                                // do not add the vcand to the list if devirtual callsite is
                                // specified by a spec file (theDevirCallsiteMap.size() > 0)
                                // and the callsite is not found in the file
                                if (dc || theDevirCallsiteMap.size() == 0)  {
                                    devirtualized = true;
                                    callee_st_idx = vcand.Transform_Function_ST_IDX;
                                    vcands.push_back(vcand);
                                    if (Enable_Debug == true) {
                                        // the set of classes that may been instantiated 
                                        // is stored in vf_object_instances, which is
                                        // used to get debug dumps only
                                       vf_object_instances[callsite->Get_map_id()] = 
                                            instance_set;
                                    }
                                    if (Enable_Debug && dc) {
                                        ST_IDX calle_st = vcand.Transform_Function_ST_IDX;
                                        ST *callee_st = ST_ptr (calle_st);
                                        fprintf(Virtual_Whirls,
                                                "  the virtual function is %s\n", 
                                                 ST_name(callee_st));
                                    }
                                }
                            }
                        }
                        if (!devirtualized) {
                            // change the dummy callsite to icall target so later 
                            // the icall_process can handle the call
                            dummy_cs->Set_icall_target();
                            dummy_cs->Reset_virtual_function_target();
                            if (Enable_Debug && dc) {
                                fprintf(Virtual_Whirls,
                                        "  the Candidate_Count is not 1, no devirtualization is done!\n");
                            }
                        }
                        Dump_Callsite(method, callsite, a_wn_ipa_map, 
                                      devirtualized, callee_st_idx);
                    }
                    callsite_array++;
                    count--;
                }
            }
        }

        // if analysis found some virtual functions
        if (vcands.size() > 0) {
            Node_Transform_Lists[method->Node_Index()] = vcands;
            Node_Virtual_Function_Whirl_Map[method->Node_Index()] = 
                a_wn_ipa_map;
            if (Enable_Debug == true) {
                Transform_Debug_Data[method->Node_Index()] = 
                    vf_object_instances;
            }
            for (list <VIRTUAL_FUNCTION_CANDIDATE>::iterator  vc_cit = 
                vcands.begin(); vc_cit != vcands.end(); ++vc_cit) {
                // We could have done this right after
                // the virtual function ST_IDX was found, 
                // but I find this kind of check pointing useful 
                // while debugging.
                // Obviously this means we double traverse 
                // but that is on a small list and the cost will not 
                // be much.
                VIRTUAL_FUNCTION_CANDIDATE vcand = *vc_cit;
                Apply_Virtual_Function_Transform (vcand);
            }
            if (Enable_Debug) {   
                IPA_NODE_CONTEXT context(method);
                fprintf (Transformed_Whirls, "\n%s after transformation...\n", method->Name());
                fdump_tree (Transformed_Whirls, method->Whirl_Tree(TRUE));
                fprintf (Transformed_Whirls, "Caller->Total_Succ():%d", 
                method->Total_Succ());
                fprintf (Transformed_Whirls, "...done\n");
            }
        }
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Apply_Virtual_Function_Transform (
        VIRTUAL_FUNCTION_CANDIDATE vcand)
{
    //
    // This function contains the "oracle based static dispatch" 
    // function calling scheme.  We add code to load the ST
    // entry of the virtual table at this callsite and during run time,
    // compare if the same virtual table is used by the object.
    // Depending on the compare we take the if-then or if-else. 
    //
    IPA_NODE *method = vcand.Caller;
    SUMMARY_CALLSITE *callsite = vcand.Virtual_Call_Site;
    SUMMARY_CALLSITE *dummy_cs = vcand.Dummy_Call_Site;
    WN *vtab = vcand.Virtual_Table;
    ST_IDX vtab_st_idx = vcand.Virtual_Table_st_idx;
    UINT32 vtab_offset = vcand.Offset;
    BOOL assert_single_target = vcand.Single_Callee;

    IPA_NODE_CONTEXT context(method);
    if (Node_Virtual_Function_Whirl_Map.find (method->Node_Index()) == 
        Node_Virtual_Function_Whirl_Map.end()) {
       return;
    } else {
        hash_map<WN_MAP_ID, WN*> wn_map = (Node_Virtual_Function_Whirl_Map.find(
             method->Node_Index()))->second;
        if ( wn_map.size() == 0) {
            return;
        }
    }

    hash_map<WN_MAP_ID, WN*> wn_map = 
        Node_Virtual_Function_Whirl_Map[method->Node_Index()];
            
    ST_IDX calle_st = vcand.Transform_Function_ST_IDX;
    ST *callee_st = ST_ptr (calle_st);
    PU_IDX pui = ST_pu(callee_st);
    if (pu_node_index_map.find(pui) == pu_node_index_map.end()) {
        return;
     }

    IPA_NODE_ITER cg_iter(IPA_Call_Graph, DONTCARE);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE *ipaNode = cg_iter.Current();
        if (ipaNode == NULL)
            continue;
        if (ipaNode == method) {
            IPA_SUCC_ITER succIter(method);
            for (succIter.First(); !succIter.Is_Empty(); succIter.Next()) {
                IPA_EDGE *ipaEdge = succIter.Current_Edge();
                if (ipaEdge == NULL)
                    continue;
                if (ipaEdge->Summary_Callsite() == dummy_cs)
                    return;
            }
        }
    }
 
    NODE_INDEX callee_nod_idx = AUX_PU_node(Aux_Pu_Table[ST_pu(callee_st)]);

    if (callee_nod_idx == INVALID_NODE_INDEX) {
        return;
    }

    TY_IDX ty_callee = ST_pu_type (callee_st);
    Is_True ((!(ty_callee <= TY_IDX_ZERO && ty_callee > TY_Table_Size())),
             ("TY_IDX:%d is not in range:(%d %d)", 
              ty_callee, TY_IDX_ZERO, TY_Table_Size()));

     WN *old_wn = wn_map[callsite->Get_map_id()];

     Is_True (WN_operator_is(old_wn, OPR_ICALL), 
              ("Transformation only for ICALL"));

     if (Enable_Debug == true) {   
         fprintf (Transformed_Whirls, "\n%s Going to transform callsite %d... \n",
                  method->Name(),
                  callsite->Get_callsite_id() );
         fprintf (Transformed_Whirls, "Caller->Total_Succ():%d\n",
                   method->Total_Succ());
         fprintf (Transformed_Whirls, "Single_Callee: %d\n",assert_single_target);
         fdump_tree (Transformed_Whirls, old_wn);
     }
 
     WN_verifier (method->Whirl_Tree(TRUE));

     WN *transformed_wn = NULL;
     if ( assert_single_target ) {
         /* Generate a new direct call operation with the same parameters */
          WN *new_wn = WN_generic_call (OPR_CALL,
                                      WN_rtype(old_wn),
                                      /* result type is same as old_wn */
                                      WN_desc(old_wn),  /*  */
                                      WN_kid_count(old_wn)-1,
                                      callee_st);
          for (size_t j = 0; j < WN_kid_count(new_wn); j++) {
               WN_kid(new_wn,j) = WN_COPY_Tree_With_Map(WN_kid(old_wn,j));
          }

           WN_set_map_id(new_wn, WN_map_id(old_wn));
           WN_set_flag(new_wn, WN_flag(old_wn));

           /* Insert the new call after the original */
           WN *block = WN_Get_Parent (old_wn,
                                       method->Parent_Map(),
                                       method->Map_Table());
           WN_INSERT_BlockAfter(block,old_wn,new_wn);

           /* Remove the old call from the code stream */
           /*WN_Delete(*/WN_EXTRACT_FromBlock(block, old_wn)/*)*/;
           WN_Parentize(block, method->Parent_Map(), method->Map_Table());

           /* Now we fix up the the original callsite information */
           //callsite->Reset_icall_target(); // paranoia?
           //callsite->Reset_virtual_function_target();
           callsite->Reset_is_virtual_call();
           callsite->Reset_func_ptr();
           Is_True( WN_num_actuals(new_wn) == callsite->Get_param_count(),
                   ("Parameter count mismatch on original call site"));
           Is_True( WN_rtype(new_wn) == callsite->Get_return_type(),
                   ("Return type mismatch on original call site"));
           callsite->Set_callsite_freq();
           callsite->Set_probability(-1);

           /* Get the symbol index for the now direct call */
           method->Set_Pending_Virtual_Functions();
           IPA_NODE* callee_ipa_node = IPA_Call_Graph->Node(callee_nod_idx);
           SUMMARY_PROCEDURE* proc_summ_callee =
                   callee_ipa_node->Summary_Proc();
           callsite->Set_symbol_index(proc_summ_callee->Get_symbol_index());

           /* The call graph does not contain any edges for virtual
            * calls, but the IPA_NODE does contain a list of indirect
            * calls that must be updated.  We do two things:
            * (1) Add an edge for the now direct call
            * (2) Remove the indirect callsite from the icall list
            */
           IPA_EDGE* edge = IPA_Call_Graph->Add_New_Edge(callsite,
                                    method->Node_Index(),
                                    pu_node_index_map[ST_pu(callee_st)]);
                                    
           edge->Set_Whirl_Node(new_wn);
           if(IPA_During_Original_VF)
               edge->Set_Orig_Devirtualized();
           if(IPA_During_New_VF)
               edge->Set_New_Devirtualized();
           
           // set edge frequency in order to inline this direct call, 
           // if we can find icall's frequency set to it
           // otherwise set to 1
           BOOL set_freq = false;
           if (Cur_PU_Feedback) {
                FB_Info_Icall info_icall = Cur_PU_Feedback->Query_icall(old_wn);
                if (!info_icall.Is_uninit())
                {    
                    INT64 callee_counter = info_icall.tnv._counters[0];
                    edge->Set_frequency(FB_FREQ(callee_counter));
                    set_freq = true;
                }   
           }     
           if (!set_freq)
                edge->Set_frequency(1);

           Is_True(!method->Icall_List().empty(),
                   ("Expecting a non-empty list of indirect calls"));

           BOOL found = false;
           IPA_ICALL_LIST& icall_list = method->Icall_List ();
           for (IPA_ICALL_LIST::iterator icall_iter = icall_list.begin ();
                   icall_iter != icall_list.end (); ++icall_iter) {
               if ((*icall_iter)->Callsite() == callsite) {
                   method->Icall_List().erase(icall_iter);
                   found = true;
                   break;
               }
           }
           Is_True(found == true,("Unable to find indirect callsite"));
           transformed_wn = new_wn;
     }
     else {
         WN *copy_lda;
     if(IPA_During_Original_VF)
         copy_lda = WN_CreateLda (Use_32_Bit_Pointers ?
                 OPC_U4LDA : OPC_U8LDA,
                 0, Make_Pointer_Type (ty_callee),
                 WN_st(WN_kid(vtab,0)));
     if((IPA_During_New_VF))
         copy_lda = WN_CreateLda (Use_32_Bit_Pointers ?
                 OPC_U4LDA : OPC_U8LDA,
                 0, Make_Pointer_Type (ty_callee),
                 &St_Table[vtab_st_idx]);

         /* Generate a new direct call operation with the same parameters */
         WN *new_wn = WN_generic_call (OPR_CALL,
                 WN_rtype(old_wn),
                 /* result type is same as old_wn */
                 WN_desc(old_wn),  /*  */
                 WN_kid_count(old_wn)-1,
                 callee_st);
         for (size_t j = 0; j < WN_kid_count(new_wn); j++) {
             WN_kid(new_wn,j) = WN_COPY_Tree_With_Map(WN_kid(old_wn,j));
         }

         WN *block = WN_Get_Parent (old_wn,
                 method->Parent_Map(),
                 method->Map_Table());


#ifdef TARG_IA64
         WN* copy_load = WN_COPY_Tree_With_Map(WN_kid(old_wn,
                 WN_kid_count(old_wn)-1));
#else
         WN* copy_load = WN_COPY_Tree_With_Map(WN_kid(WN_kid(old_wn,
                 WN_kid_count(old_wn)-1),
                 0));
#endif

         OPCODE incopcode = OPCODE_make_op(OPR_SUB,
                 WN_rtype(copy_load),MTYPE_V);
         WN * sub_op;
    if(IPA_During_Original_VF)
         sub_op = WN_CreateExp2 (incopcode,
                 WN_COPY_Tree_With_Map(copy_load),
                 WN_CreateIntconst(OPC_I4INTCONST,
                         (WN_lda_offset(WN_kid0(vtab)))));
    if(IPA_During_New_VF)
         sub_op = WN_CreateExp2 (incopcode,
                 WN_COPY_Tree_With_Map(copy_load),
                 WN_CreateIntconst(OPC_I4INTCONST,
                         vtab_offset));
	
         WN *cmp = WN_Create (Use_32_Bit_Pointers?
                 OPC_U4U4EQ:OPC_U8U8EQ,2);

         WN_kid0(cmp) = copy_lda;
         WN_kid1(cmp) = sub_op;

         WN *then_blk = WN_CreateBlock();
         WN *else_blk = WN_CreateBlock();
         WN* if_type = WN_CreateIf (cmp, then_blk, else_blk);
         WN* aold_wn = WN_COPY_Tree_With_Map (old_wn);

         WN_INSERT_BlockLast (then_blk, new_wn);
         WN_INSERT_BlockLast (else_blk, aold_wn);
         
         for (WN* stmt = WN_next (old_wn); 
               stmt != NULL && Is_Return_Store_Stmt (stmt);) {
             WN_INSERT_BlockLast (then_blk, WN_COPY_Tree(stmt));
             WN_INSERT_BlockLast (else_blk, WN_COPY_Tree(stmt));
             WN * ret_wn = stmt;
             stmt = WN_next (stmt);
             // Should we be calling WN_Delete() here?
             WN_EXTRACT_FromBlock (block, ret_wn);
         }

         if (Enable_Profile == true) {
             //
             // This code is used for profiling only. This is not
             // a production feature.
             // we load from extern variable miss_vf, hit_vf
             // increment and store back to the same location
             // these load/stores are to two arrays
             // else block gets missed load/store
             // if-then block gets hits load/store
             //
             if (Hit_ST_IDX != ST_IDX_ZERO && Miss_ST_IDX != ST_IDX_ZERO) {
                 // hit part
                 // must use ldid instead???
                 ST *Hit_st = &St_Table (GLOBAL_SYMTAB, Hit_ST_IDX);
                 ST *Miss_st = &St_Table (GLOBAL_SYMTAB, Miss_ST_IDX);
                 TY_IDX pty_idx = Make_Pointer_Type (ST_type(Hit_st), FALSE);
                 WN *hit_ldid = WN_Ldid (MTYPE_I4,
                         (WN_OFFSET)(Num_VFs_Count*4), Hit_st,
                         ST_type(Hit_st));
                 WN *miss_ldid = WN_Ldid (MTYPE_I4,
                         (WN_OFFSET)(Num_VFs_Count*4), Miss_st,
                         ST_type(Miss_st));
                 OPCODE myaddopcode = OPCODE_make_op (OPR_ADD,
                         MTYPE_U8,MTYPE_V);
                 WN *hit_add_op = WN_CreateExp2 (myaddopcode,
                         hit_ldid,
                         WN_CreateIntconst(
                                 OPC_I4INTCONST, 1));
                 WN *miss_add_op = WN_CreateExp2 (myaddopcode,
                         miss_ldid,
                         WN_CreateIntconst (
                                 OPC_I4INTCONST, 1));
                 WN *hit_store = WN_Stid (MTYPE_I4,
                         (WN_OFFSET)(Num_VFs_Count*4), Hit_st,
                         ST_type(Hit_st), hit_add_op);
                 WN *miss_store = WN_Stid (MTYPE_I4,
                         (WN_OFFSET)(Num_VFs_Count*4), Miss_st,
                         ST_type(Miss_st), miss_add_op);
                 WN_INSERT_BlockLast (then_blk,hit_store);
                 WN_INSERT_BlockLast (else_blk,miss_store);
                 list <string> mytags;
                 mytags.push_back (string(method->Name()));
                 mytags.push_back (string(ST_name(callee_st)));
                 Miss_Hit_Tag[Num_VFs_Count] = mytags;
             }
         }

         WN_Parentize (then_blk, method->Parent_Map(),
                 method->Map_Table());
         WN_Parentize (else_blk, method->Parent_Map(),
                 method->Map_Table());

         WN_set_map_id(new_wn, WN_map_id(old_wn));
         WN_set_flag(new_wn, WN_flag(old_wn));

         WN_INSERT_BlockAfter(block,old_wn,if_type);
         // This is the old call, we should call WN_Delete()?
         WN_EXTRACT_FromBlock(block, old_wn);
         WN_Parentize (block, method->Parent_Map(), method->Map_Table());

         // Now fix_up_only will update this dummy callsite as well.
         dummy_cs->Reset_virtual_function_target();
         dummy_cs->Reset_func_ptr();
         dummy_cs->Set_param_count(WN_num_actuals(new_wn));
         dummy_cs->Set_return_type(WN_rtype(new_wn));
         //dummy_cs->Set_callsite_freq();
         //
         // We dont have use for probability at all
         // in this transformation. If I
         // dont set probability to -1, the inliner
         // skips this edge.
         //
         dummy_cs->Set_probability(-1);

         // set the symbol index of the inferred function on
         // the dummy callsite
         method->Set_Pending_Virtual_Functions();
         IPA_NODE* callee_ipa_node = IPA_Call_Graph->Node(callee_nod_idx);
         SUMMARY_PROCEDURE* proc_summ_callee =
                 callee_ipa_node->Summary_Proc();
         dummy_cs->Set_symbol_index(proc_summ_callee->Get_symbol_index());

         // update call graph

         IPA_EDGE* edge = IPA_Call_Graph->Add_New_Edge(dummy_cs,
                 method->Node_Index(),
                 pu_node_index_map[ST_pu(callee_st)]);
         edge->Set_Whirl_Node(new_wn);
         if(IPA_During_Original_VF)
               edge->Set_Orig_Devirtualized();
         if(IPA_During_New_VF)
               edge->Set_New_Devirtualized();
         transformed_wn = block;
     }

    // verify if the whirl_tree is proper after my insertions into it
    WN_verifier(method->Whirl_Tree(TRUE));

    if (Enable_Profile == true || 
        Enable_Statistics == true) { 
        Num_VFs_Count ++;
    }
    if (Enable_Profile == true ||
        Enable_Statistics == true ||
        Enable_Debug == true) {
        Optimized_Methods_By_NODE_INDEX[method->Node_Index()] = 
            method;
    }

    if (Enable_Debug == true) {   
        fprintf (Transformed_Whirls, "\nTransformed to ...\n");
        fdump_tree (Transformed_Whirls, transformed_wn);
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Update_Class_Hierarchy_Depth(int index)
{
    Class_Hierarchy_Depth[index] = Class_Hierarchy_Depth[index] + 1;
}
        
void IPA_VIRTUAL_FUNCTION_TRANSFORM::Update_Instances (int index)
{
    Num_Instances[index] = Num_Instances[index] + 1;
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Build_Virtual_Function_Whirl_Map (
    IPA_NODE *method, WN_IPA_MAP& a_wn_ipa_map)
{
    IPA_NODE_CONTEXT context(method);
    WN *wn_start = method->Whirl_Tree(TRUE);
    Is_True(wn_start, ("IPA_fast_static_analysis_VF: Whirl node is empty."));
    //
    // Walk over the whirl nodes in the Whirl_Tree of a node and
    // check if a WHIRL node is a virtual function call or not.
    // If WN* is a virtual function,
    // map it into argument WN_IPA_MAP& using the WN*'s 
    // map_id as hashmap's key.
    // 
    for (WN_ITER *wni = WN_WALK_StmtIter(wn_start);
         wni != NULL; wni = WN_WALK_StmtNext(wni)) {
        WN *wn = WN_ITER_wn(wni);
        if (WN_operator(wn) == OPR_ICALL) {
            if (WN_Call_Is_Virtual(wn)) {
                if (Enable_Debug == true) {
                    method->Print(Virtual_Whirls);
                    fprintf(Virtual_Whirls, 
                            "------ mapid:%d\n ------",
                            WN_map_id(wn));
                    fdump_tree(Virtual_Whirls, wn);
                }
                Is_True(a_wn_ipa_map.find(WN_map_id(wn)) == 
                        a_wn_ipa_map.end(), 
                        ("Whirl map id conflicts in wn_map"));
                a_wn_ipa_map[WN_map_id(wn)] = wn;
            }
        }
    }
}

hash_set<TY_INDEX> 
    IPA_VIRTUAL_FUNCTION_TRANSFORM::Identify_Instances_From_Subclass_Hierarchy (
        TY_INDEX declared_class)
{
    hash_set<TY_INDEX> targets;
    hash_set <TY_INDEX> inter_set;
    IPA_Class_Hierarchy->Get_Sub_Class_Hierarchy (declared_class, targets);
    for (hash_set<TY_INDEX>::iterator tg_it = 
         targets.begin();
         tg_it != targets.end();
         ++tg_it) {
        if (Constructed_Types.find(*tg_it) != Constructed_Types.end()) {
            inter_set.insert(*tg_it);
        }
    }
    return inter_set;
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Identify_Virtual_Function (
            hash_set<TY_INDEX> constructed_types_set, 
            SUMMARY_CALLSITE *callsite,
            VIRTUAL_FUNCTION_CANDIDATE& vcand)
{
    //
    // The following is borrowed directly from the 
    // earlier developer's work.
    // "Multiple Inheritance for C++", 
    // Bjarne Stroustrup, May 1999 issue of "The C/C++ Users Journal"
    // is a very useful read to begin with.
    // Available on citeseer: 
    // http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.36.8766
    // More recent information is in Wikipedia: 
    // http://en.wikipedia.org/wiki/Virtual_function_table
    // These contain C++ specifics on class
    // instance data layout in memory. 
    // 1: A virtual table is shared by all instances of a class.
    // 2: Each subtype has its own virtual table.
    // 3: First element in an instance will be the rooting base
    // class/true class's virtual pointer, 
    // if virtual functions are used in the class hierarchy. 
    // Virtual table pointers are shared between the root base class and 
    // the true type of the object. The second entry in the object 
    // memory layout will be first data 
    // member in base class, followed by second, etc, and after
    // exhausting the base class, entries are the
    // virtual table pointer and data members of subclasses 
    // of base class and then their children and so on. 
    // Thus the data layout goes recursively first from the 
    // root of the class hierarchy refered earlier 
    // as the rooting base class.
    // 4: In case of multiple inheritance 
    // the layout is depth first on the specified inheritance order.
    // 5: Implicit typecasting: When object pointers are passed 
    // using typecasting to a base 
    // in order to support polymorphism via virtual functions, 
    // we see that the callee requires
    // a 'this' pointer that looks like the base type or declared type 
    // in the function parameter and not a subtype of the 
    // declared type. So in order to support this type casting, 
    // C++ offsets the pointer of subtype object, and
    // passes the offseted pointer as though it actually
    // is a type of the declared class. When there is a call
    // to a virtual function call the dynamically
    // resolved virtual function expects the original subtype class.
    // That is an uncast is necessary to make virtual function calls 
    // with an 'this' pointer to instance of the subtype class. 
    // To ensure this 'thunks' are used under the covers in G++. 
    // Thunks are used in both single and multiple inheritance scenarios.
    // 
    // Open64 does not need to do any of this work either to set up 
    // the virtual table or to set up data layout of objects. 
    // These are handled by the GCC frontend. open64 faithfully
    // brings this data from the GCC frontend 
    // into TY_Tab, Inito, Initvs etc.
    // 
    // Sometimes it helps to read the following with 
    // a pen/paper and a print of the 
    // Ty_Tab, Initv, Inito etc 
    // symbol tables of your test file
    //  
    // 1: formal_class is also called as the declared class
    // 2: callsite->Get_vtable_offset() 
    // is the vtable offset at the callsite. 
    // The virtual function is at this 
    // offset in the virtual table. 
    // The virtual table pointer is always 
    // the first entry in an object. 
    // 3: formal_vptr_offset is the offset to get
    // the correct virtual table, nonzero only when
    // multiple inheritance is used 
    // and when the declared class is subtype 
    // using multiple inheritance.
    // Another requirement to have a non-zero formal_vptr_offset
    // is that the virtual function is existing
    // in a virtual table that is niether in the true class nor in the 
    // first base in the inheritance order, 
    // and is beyond these two, i.e. in the second or third etc. 
    // Remember that the object layout of the 
    // leaf class is in depth first order 
    // of the inherited bases. Also remember that 
    // the virtual table that is  as the first element 
    // can only be used directly in situations 
    // when the 'true' class is either the 'declared' class 
    // or when the virtual function resides in virtual table of
    // the first base of the true class.
    // But here the virtual function is in the virtual table of the 
    // second base or the third base etc. 
    // We need to get the pointer of the virtual 
    // table in the particular base to resolve the function.
    // For that we use another offset from this
    // "incorrect" virtual table pointer to resolve to the 
    // correct virtual table. This offset is called
    // formal_vptr_offset here. 
    // After we get the virtual table
    // we can use the callsite->Get_vtable_offset() 
    // to reach the exact virtual function.
    // For ex, if the inheritance is like so:
    // BarFoo:Bar, Foo. Assume that we call 
    // a certain method which is defined in Foo.
    // Assume that the declared class at the 
    // calling site is BarFoo.
    // The virtual table pointer for Foo will be at 
    // formal_vptr_offset offset from 
    // the virtual pointer of BarFoo.
    // Summarizing, if the declared class is known to be 
    // BarFoo, we need to offset the object by formal_vptr_offset 
    // to obtain the virtual table of Foo and use 
    // callsite->Get_vtable_offset 
    // to obtain a function reference in the virtual table.
    // 4:ancestor_offset:
    // Extending the same example is helpful in describing
    // another offset called ancestor_offset and how that relates 
    // to the rest of the offsets. 
    // If the 'true' class of the object is a subclass of BarFoo, 
    // we use the ancestor_offset to resolve
    // to the correct virtual table. This ancestor_offset 
    // is used only during our analysis to locate the virtual table.  
    // Note the while the rest of the offsets are used 
    // during a virtual function call at runtime, 
    // the ancestor class is not required at runtime. 
    // It is required by us here because we start our 
    // search for the virtual table using constructors
    // of true types and try to find the
    // actual function using the formal_vptr_offset
    // and the ancestor_offset. 
    // Ancestor offset is computed by summing 
    // sizes of data members of classes that 
    // are along the path from the constructor class
    // to the ancestor which is refered to as 
    // formal_class or declared class in here.
    // 
    // To get a complete handle on all of this 
    // you need to understand the paper by 
    // C++ Stroustrup on Multiple inheritance, 
    // wikipedia page on Thunks and in general understand 
    // how virtual function resolution happens at run time. 
    // The Stroustrup paper on Multiple Inheritance
    // describes a "naive" implementation for virtual table
    // that is costlier in space than what GCC uses.         
    // The implementation in GCC generates thunks and 
    // using them cuts down on the virtual table sizes against the 
    // naive implementation. Please contact us through 
    // the appropriate forum in case you have comments. 
    // 
    TY_INDEX formal_class = 
        TY_IDX_index(callsite->Get_virtual_class());
    UINT64 formal_vptr_offset =
        callsite->Get_vptr_offset();
    for (hash_set<TY_INDEX>::iterator cons_it = 
         constructed_types_set.begin(); 
         cons_it != constructed_types_set.end();
         ++cons_it) {
        TY_INDEX cons_ty = *cons_it;
        Is_True (Constructor_Map.find(cons_ty) != 
                 Constructor_Map.end(),
                 ("IPA_fast_static_analysis_VF:Constructor_Map does not carry pui for type"));
        Is_True (pu_node_index_map.find(Constructor_Map[cons_ty]) !=
                 pu_node_index_map.end(),
                 ("IPA_fast_static_analysis_VF:PU_node_index_map does not carry node index for pui"));

        size_t ancestor_offset
            = IPA_Class_Hierarchy->Get_Ancestor_Offset(
                       cons_ty, formal_class);

        IPA_NODE * constructor = IPA_Call_Graph->Node(
             pu_node_index_map[Constructor_Map[cons_ty]]);

        Is_True (constructor != NULL,
                 ("IPA_fast_static_analysis_VF: Constructor Node is not found in call graph"));

        IPA_NODE_CONTEXT context(constructor);
        WN *vtab = NULL;
        WN *wn_start = constructor->Whirl_Tree(TRUE);
        for (WN_ITER *wni = WN_WALK_StmtIter(wn_start);
             wni != NULL;
             wni = WN_WALK_StmtNext(wni)) {
            WN *wn = WN_ITER_wn(wni);
            if (WN_operator(wn) == OPR_ISTORE) {
                WN *rhs = WN_kid0(wn);
                WN *lhs = WN_kid1(wn);
                UINT64 add_ofst = 0;
                if (WN_operator(lhs) == OPR_ADD) {
                    if (WN_operator(WN_kid1(lhs)) == OPR_INTCONST) {
                        add_ofst = WN_const_val(WN_kid1(lhs));
                        lhs = WN_kid0(lhs);
                    } else {
                        continue;
                    }
                }
                if (WN_operator(rhs) == OPR_LDA && 
                    WN_operator(lhs) == OPR_LDID) {

                    TY_IDX this_class_ptr = ST_type(WN_st(lhs));
                    if (TY_kind(this_class_ptr) != KIND_POINTER)
                        continue;

                    TY_IDX this_class = TY_pointed(this_class_ptr);
                    if (TY_IDX_index(this_class) == cons_ty) {
                        // Find the field by the
                        // statement in constructor
                        UINT64 ofst = WN_store_offset(wn) + add_ofst;
                        // If they are matched, this
                        // statement indicates the
                        // vtable of the actual class
                        if (ofst == ancestor_offset + formal_vptr_offset) {
                            vtab = wn;
                            break;
                        }
                    }
                }
            }
        }

        if (vtab != NULL) {
            size_t func_offset = callsite->Get_vtable_offset();
            vcand.Virtual_Table = vtab;
            Locate_Virtual_Function_In_Virtual_Table (constructor, 
                    vtab, func_offset, vcand);
        }
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Locate_Virtual_Function_In_Virtual_Table (
    IPA_NODE *constructor,
    WN* vtab, size_t func_offset, 
    VIRTUAL_FUNCTION_CANDIDATE& vcand)
{
    
    // I borrowed everything here from 
    // the earlier developer.
    IPA_NODE_CONTEXT context(constructor);
    WN *pos = WN_kid0(vtab);
    // vtab_st is the ST of vtable
    ST *vtab_st = WN_st(pos);
    // base_offset is the offset of vtable
    UINT32 base_offset = WN_lda_offset(pos);
    
    // The ST for the virtual table is in the INITO/INITV
    // regions in WHIRL
    // Find the INITO entry for the ST index
    INITO_IDX inito_idx = Find_inito_by_st(ST_st_idx(vtab_st));
    if (inito_idx <= INITO_IDX_ZERO)
      return;
    // There is an INITV for the virtual table in INITO.
    // We get the INITV entry to look up the virtual table
    INITV_IDX initv_idx = INITO_val(inito_idx);
    // The position of target function
    // call in the virtual table INITV is the entry
    // at func_offset from base_offset. 
    UINT32 offset = base_offset + func_offset;
    ST_IDX st_id = ST_IDX_ZERO;
    if (initv_idx <= INITV_IDX_ZERO) {
        return;
    }
    do {
        if (offset == 0) {
    #ifdef TARG_IA64
            Is_True(INITV_kind(initv_idx) == INITVKIND_SYMIPLT,
                    ("Wrong INITV for virtual function."));
    #else
            Is_True(INITV_kind(initv_idx) == INITVKIND_SYMOFF,
                    ("Wrong INITV for virtual function."));
    #endif
            st_id = INITV_st(initv_idx);
            break;
        }
        switch (INITV_kind(initv_idx)) {
            case INITVKIND_BLOCK:
                initv_idx = INITV_blk(initv_idx);
                break;
            case INITVKIND_VAL:
            case INITVKIND_ZERO:
                offset -= Pointer_Size;
                initv_idx = INITV_next(initv_idx);
                break;
            case INITVKIND_SYMOFF:
                offset -= Pointer_Size;
                initv_idx = INITV_next(initv_idx);
                break;
    #ifdef TARG_IA64
            case INITVKIND_SYMIPLT:
                offset -= (Pointer_Size << 1);
                initv_idx = INITV_next(initv_idx);
                break;
    #endif
            default:
                Is_True(FALSE, ("Unexcepted INITV kind."));
            }
    } while (initv_idx > INITV_IDX_ZERO);
    vcand.Transform_Function_ST_IDX = st_id;
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Dump_Constructors ()
{
    if (Enable_Debug == true) {
        FILE *fp = fopen("Constructors.log", "w");
        for (hash_map<TY_INDEX, PU_IDX>::iterator cm_it = Constructor_Map.begin();
             cm_it != Constructor_Map.end(); ++cm_it) {
            TY_INDEX ty_indx = cm_it->first;
            PU_IDX pu_indx = cm_it->second;
            fprintf (fp, "type of constructor: <TY_INDEX:%d>\n", ty_indx);
            Ty_tab[ty_indx].Print(fp);
            fprintf (fp, "PU of constructor:\n");
            Pu_Table[pu_indx].Print(fp);
            IPA_NODE *constructor =
                IPA_Call_Graph->Node (pu_node_index_map[pu_indx]);
            if (constructor != NULL) {
                IPA_NODE_CONTEXT context(constructor);
                fprintf(fp, "WHIRL of constructor:\n");
                fdump_tree(fp,constructor->Whirl_Tree(TRUE));
            } else {
                fprintf(fp, "WHIRL of constructor: NOT_FOUND!\n");
            }
                 
            fprintf (fp, "-\n");
        }
        fclose(fp);
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Dump_Virtual_Function_Transform_Candidates ()
{
    if (Enable_Debug == true) {
        FILE *fp = fopen("VF_opt_set.log", "w");
        for (VIRTUAL_FUNCTION_DEBUG_DATA::iterator vf_it = 
             Transform_Debug_Data.begin();
             vf_it != Transform_Debug_Data.end(); ++vf_it) {
            NODE_INDEX nod_indx = vf_it->first;
            hash_map<WN_MAP_ID, hash_set<TY_INDEX> > wn_map_constrs = 
                vf_it->second;
            {
                IPA_NODE* method = Optimized_Methods_By_NODE_INDEX[nod_indx];
                IPA_NODE_CONTEXT context(method);
                fprintf (fp, "WHIRL of caller:\n");
                fdump_tree(fp, method->Whirl_Tree(TRUE));
                fprintf (fp, "@WHIRL of caller ends_here\n");
    
                for (hash_map<WN_MAP_ID, hash_set<TY_INDEX> >::iterator vf_wn_it 
                     = wn_map_constrs.begin(); vf_wn_it != wn_map_constrs.end();
                     ++vf_wn_it) {
                    fprintf (fp, "VF in WHIRL which can be optimized:\n");
                    WN_MAP_ID wn_id = vf_wn_it->first;
                    hash_set<TY_INDEX> vf_obj_types = vf_wn_it->second;
                    WN_IPA_MAP a_wn_map = Node_Virtual_Function_Whirl_Map[method->Node_Index()];
                    fdump_tree(fp, a_wn_map[wn_id]);
                    fprintf (fp, "@VF in WHIRL which can be optimized ends_here\n");
                    fprintf (fp, "VF is accessed by these constructed objects:\n");
                    for (hash_set<TY_INDEX>::iterator vf_obj_it = vf_obj_types.begin();
                         vf_obj_it != vf_obj_types.end(); ++vf_obj_it) {
                        TY_INDEX ty_indx = *vf_obj_it;
                        Ty_tab[ty_indx].Print(fp);
                        {
                            PU_IDX pu_indx = Constructor_Map[ty_indx];
                            fprintf (fp, "PU of constructor:\n");
                            Pu_Table[pu_indx].Print(fp);
                            IPA_NODE *constructor =
                                IPA_Call_Graph->Node (pu_node_index_map[pu_indx]);
                            {
                                IPA_NODE_CONTEXT context(constructor);
                                fprintf (fp, "WHIRL of constructor\n");
                                fdump_tree(fp,constructor->Whirl_Tree(TRUE));
                                fprintf (fp, "@WHIRL of constructor ends_here\n");
                            }
                        }
                    }
                    fprintf (fp, "@VF is accessed by these constructed objects ends_here\n");
                }
            }
        }
        fclose(fp);
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Fixup_Virtual_Function_Callsites ()
{
    IPA_NODE_ITER cg_iter(IPA_Call_Graph, PREORDER);
    for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
        IPA_NODE *method = cg_iter.Current();
        if (method == NULL) 
            continue;
        BOOL is_cxx = PU_cxx_lang(method->Get_PU());
        if (!is_cxx)
            continue;
        Fixup_Virtual_Function_Callsites_Per_Node (method);
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Fixup_Virtual_Function_Callsites_Per_Node (
        IPA_NODE *method)
{
    if (method == NULL)
        return;
    PU_IDX pui = ST_pu(method->Func_ST());
    if (pu_node_index_map.find(pui) == pu_node_index_map.end()) {
        return;
    }
    SUMMARY_PROCEDURE * proc_array = method->Summary_Proc();
    SUMMARY_CALLSITE* callsite_array2 =
            IPA_get_callsite_array(method) + proc_array->Get_callsite_index();
    INT cs_index = proc_array->Get_callsite_index();
    cs_index =0;
    INT count = 0;
    for (INT j = 0; j < proc_array->Get_callsite_count(); ++j, ++cs_index) {
        // 
        // The only calls that will have this flag set are 
        // virtual function dummy callsites.
        // If the pass is enabled, after virtual function replacement, 
        // some dummy callsites will have this flag reset as they now get to 
        // represent the infered callee. Also there is a CALL WHIRL 
        // node that corresponds to it. In case the replacement did not 
        // kick in for a virtual function or in case the pass 
        // is disabled, the corresponding dummy callsite
        // will not have an infered callee and there will not be
        // a CALL WHIRL node corresponding to it. These left-over 
        // dummy callsites will still have the flag set on it.
        // We check for this case here and do not remap 
        // the left-over dummy callsites to the unique id, INT count.
        // Effectively what happens is that only for every 
        // CALL instruction in the WHIRL do we have a corresponding
        // callsite. Each callsite in turn binds back to every CALL 
        // instruction using the newly set callsite_id. 
        // Look at IPO_Process_Virtual_Functions to get more 
        // idea on the use for this callsite id.
        // 
       if (!callsite_array2[cs_index].Is_virtual_function_target() &&
           !callsite_array2[cs_index].Is_icall_target() ) {
            callsite_array2[cs_index].Set_callsite_id (count++);
        }
    }
}

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Print_Statistics ()
{
    if (Enable_Statistics == true) {
        FILE *statistics_file = fopen("Statistics.log", "w");
        fprintf (statistics_file, 
                "Number of vf callees that can be statically replaced: %d\n", 
                Num_VFs_Count);
        fprintf (statistics_file,
                "Number of vf callsites caught by heuristic 1: %d\n", 
                Class_Hierarchy_Transform_Count);
        fprintf (statistics_file,
                "Number of vf callsites caught by heuristic 2: %d\n",
                Class_Instance_Transform_Count);
        fclose(statistics_file);
    }
}
 
/* 
 The following function, IPA_VIRTUAL_FUNCTION_TRANSFORM::Miss_Hit_Profile(), 
 is a profiling cum testing implementation.
 This is an experimental feature for use by developers during test and debug.
 The profiling mechanism counts how many times we had hits, i.e the optimization found
 the right virtual function replacement candidate and how many times the optimization found
 a wrong choice. That way we can see how effective the optimization really is. Because there
 may be thousands of instances for which this transfomation was applied on, it is not practical 
 to eye ball test them all of them. It helps to 
 use a profiling structure in the if-then and if-else to count how many times the optimization
 successefully predicted the right virtual function and how many times the optimization
 failed in predicting the right virtual function. In case we have a very large hit rate, 
 we are somewhat certain that our approach is accurate for all practical purposes.         
 How this feature works is described here. 
 After replacement we have an if-then-else structure for 
 the virtual function. Each such replaced virtual function is assigned an unique integer id. 
 Inside the if-then block and if-else block, profiling code is added. The profiling code
 requires two global arrays named __HIT_VIRTUAL_FUNCTION__ and __MISS_VIRTUAL_FUNCTION__ to be
 declared in the program that is getting compiled (in case these arrays are not defined, the
 pass does not insert the profiling code).
 In the if-then block pass adds a load from 
 __HIT_VIRTUAL_FUNCTION__[id], a piece of code to increment the loaded value and
 a store back to the load from location and likewise, inside the if-else block 
 pass adds a load from __MISS_VIRTUAL_FUNCTION__[id], increment code, store back. 
 In order for this to work we got to add a piece of code to the
 program being compiled in order to add the global arrays, 
 __HIT_VIRTUAL_FUNCTION__ and __MISS_VIRTUAL_FUNCTION__ and initialize and
 finalize these arrays.
 i.e at this time, we need to manually add lines into the  
 source code just like down here.
 Here it is:  
unsigned int __MISS_VIRTUAL_FUNCTION__[2000], __HIT_VIRTUAL_FUNCTION__[2000];
// Add this class for initialization and finalization of these two arrays
class StatMan__ {
    public:
        // initialize elements in the hit, miss arrays to zero
    void init () {
        for (int mi = 0; mi < 2000; ++mi) {
            __MISS_VIRTUAL_FUNCTION__[mi] = 0;
            __HIT_VIRTUAL_FUNCTION__[mi] = 0;
        }
    }

    // create a file miss_hit.csv and dump out the profiling data from 
    // each element in the array. This function is meant to be called
    // just before the program completes execution. 
    // Dumps data into the run directory in a file miss_hit.csv like so
    // (id, misses, hits)
    // 1, 0, 90909 // there were 0 misses, and 90909 hits for virtual function
    // replacement location with id 1
    void fini () {
        FILE *mh = fopen("miss_hit.csv", "w");
        for (int mi = 0; mi < 2000; ++mi) {
            fprintf (mh, "%d,%u,%u\n",
                    mi,
                    __MISS_VIRTUAL_FUNCTION__[mi],
                    __HIT_VIRTUAL_FUNCTION__[mi]);
        }
        fclose(mh);
    }
};

// Add these lines to your main function. First two lines go
// right after entry and the last line goes right before the return
// statement(s) in the main function. 
int main () {
    //    StatMan__ myman;
    //    myman.init();
    ... 
    //    myman.fini();
    return 
}
*/

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Miss_Hit_Profile ()
{
    if (Enable_Profile == true) {
        if (Hit_ST_IDX != ST_IDX_ZERO && Miss_ST_IDX != ST_IDX_ZERO) {
            FILE *csvfile = fopen("Miss_hit_profile.csv", "w");
            for (hash_map<int, list<string> >::iterator mhmi =
                 Miss_Hit_Tag.begin(); mhmi != Miss_Hit_Tag.end(); ++mhmi) {
                int id_num = mhmi->first;
                list<string> str_lis = mhmi->second;
                fprintf (csvfile, "%d,", id_num);
                for (list<string>::iterator myns = str_lis.begin(); 
                     myns != str_lis.end(); ++myns) {
                    fprintf(csvfile, "%s,", (*myns).c_str());
                }
                fprintf (csvfile, "\n");
            }
            fclose(csvfile);
        }
    }
}

/* 
 Histogram_Statistics function collects histogram data. 
 compiler dump goes into a vf_class_hist.csv file in 
 current directory 
 Data looks like this:
 "Number of classes used from hierarchy","Number of virtual function callsites"
   1,888 // There were 888 virtual functions with Size(True_Class) == 1, 
   2,814
   ...
 "Number of instance classes","Number of virtual function callsites"
   1,2 
   2,856 // There were 856 virtual functions with Size(Instance_Class) == 2
   3,1085 
*/

void IPA_VIRTUAL_FUNCTION_TRANSFORM::Histogram_Statistics ()
{
    if (Enable_Statistics == true) {
        if ((Class_Hierarchy_Depth.size() != 0) || (Num_Instances.size() != 0)) {
            FILE *csvfile = fopen ("vf_class_hist.csv", "w");
            fprintf (csvfile, "%s,%s",
                    "\"Number of classes used from hierarchy\"",
                    "\"Number of virtual function callsites\"\n");
            for (hash_map<int, int>::iterator trcit = Class_Hierarchy_Depth.begin();
                 trcit != Class_Hierarchy_Depth.end(); ++trcit) {
                int trval = trcit->first;
                int numvf = trcit->second;
                fprintf(csvfile, "%d,%d\n", trval, numvf);
            }
            fprintf (csvfile, "%s,%s",
                     "\"Number of instance classes\"", 
                     "\"Number of virtual function callsites\"\n");
            for (hash_map<int,int>::iterator incit = Num_Instances.begin();
                 incit != Num_Instances.end(); ++incit) {
                int inval = incit->first;
                int numvf = incit->second;
                fprintf(csvfile, "%d,%d\n", inval, numvf);
            }
            fclose(csvfile);
        }
    }
}
