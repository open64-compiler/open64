/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#ifndef cxx_ipo_inline_INCLUDED
#define cxx_ipo_inline_INCLUDED


#ifndef defs_INCLUDED
#include "defs.h"
#endif

#ifndef wn_INCLUDED
#include "wn.h"                     // WN
#endif

#ifndef wn_util_INCLUDED
#include  "wn_util.h"               // WN_ITER
#endif

#ifndef mempool_INCLUDED
#include "mempool.h"                 // MEM_POOL
#endif

#ifndef irbdata_INCLUDED
#include "irbdata.h"                // INITO
#endif

#ifndef wn_tree_util_INCLUDED
#include "wn_tree_util.h"
#endif

#include <set>

#define IPO_PARAM_TYPE 0x001


extern BOOL
Can_Inline_Call (IPA_NODE* caller, IPA_NODE* callee, const IPA_EDGE* edge);

extern BOOL
IPA_Return_Types_Are_Compatible(ST* callee, WN* call);

extern BOOL
IPA_Call_IO_Statement(WN* call);

extern BOOL
IPA_Exception_Statement(WN* call);

extern void
Init_inline();

// parameter passing methods
enum PASS_METHOD
{
    PM_UNKNOWN,				// undecided pass method
    PM_COPY_IN,				// pass by value; copy the actual
					// to a preg or temp var.
    PM_REPLACE,				// direct replacement (pass by ref).
    PM_COPY_IN_OUT,			// copy-in/copy-out: avoid aliasing 
					// caused by direct replacement
    PM_NEED_BARRIER			// user-specifed non-aliased parms, 
					// or guaranteed by language.  Need 
					// to put barriers around the
					// inlined body to block code
					// motion beyond the non-aliased
					// scope. 
				   
};

// formal parameter fixup methods
enum FIXUP_METHOD
{
    FM_UNKNOWN,				// undecided
    FM_REPLACE_ST,			// replace by the specified ST
    FM_REPLACE_ACTUAL,			// replace by the actual parameter
					// (should be an expression)
    FM_REPLACE_ARRAY,			// similar to FM_REPLACE_ACTUAL for 
					// for array only, need to fixup
					// the array subscript as specified 
					// in the RESHAPE_METHOD
    FM_LOWER_FORMAL_REF			// valid only for
					// SCLASS_FORMAL_REF, lower the
					// formal to SCLASS_FORMAL and then
					// replace by the actual parameter
};
	

// array reshape method
enum RESHAPE_METHOD
{
    RS_UNKNOWN,
    RS_EQ = 1,				// actual_dim = formal_dim, last
					// dim may be different 
    RS_LT = 2,				// formal numdim < actual numdim
    RS_GT = 3,				// formal dim < actual numdim
    RS_LINEARIZE = 4,			// just linearize the array
    RS_EQ_COMMON = 5			// actual is a common block value
};


class PARAMETER_ATTRIBUTES
{
private:
    ST* _formal_st;			
    WN* _actual;			// actual parameters
    ST_IDX _replace_st;			// st to replace the formal
    PREG_NUM formal_preg;		// preg number if replace_st is preg
    ST* actual_aliased;			// ST of actual parameter aliased
					// with another actual parameter
    ST* addr_passed_actual;		// if the actual parameter is an 
					// LDA of a symbol that is not
					// addr_saved, record it here.  We
					// need to check if the formal is
					// ever saved, if so, we need to
					// set the addr_saved bit of the
					// actual. 
    PASS_METHOD pass_method;
    FIXUP_METHOD fixup_method;
    RESHAPE_METHOD reshape_method;
    BOOL is_restrict_pointer;		// formal is a restrict pointer
  
public:
    ST* Formal_St() const		{ return _formal_st;};
    WN* Actual_Wn() const		{ return _actual;};
    ST_IDX Replace_St() const		{ return _replace_st;};
    PREG_NUM Formal_Preg () const	{ return formal_preg; }
    ST* Actual_Aliased () const		{ return actual_aliased; }
    ST* Addr_Passed_Actual () const	{ return addr_passed_actual; }
    PASS_METHOD Pass_Method () const	{ return pass_method; }
    FIXUP_METHOD Fixup_Method () const	{ return fixup_method; }
    RESHAPE_METHOD Reshape_Method () const { return reshape_method; }
    BOOL Restrict_Pointer () const	{ return is_restrict_pointer; }

    void Set_formal_st(ST* s)		{ _formal_st = s;};
    void Set_actual(WN* a)		{ _actual = a;};
    void Set_replace_st (ST_IDX s)	{ _replace_st = s;};
    void Set_formal_preg (PREG_NUM p)	{ formal_preg = p; }
    void Set_actual_aliased (ST* st)	{ actual_aliased = st; }
    void Set_addr_passed_actual (ST* st){ addr_passed_actual = st; }
    void Set_pass_method (PASS_METHOD p){ pass_method = p; }
    void Set_fixup_method (FIXUP_METHOD p){ fixup_method = p; }
    void Set_reshape_method (RESHAPE_METHOD r) { reshape_method = r; }
    void Set_restrict_pointer () { is_restrict_pointer = TRUE; }

    PARAMETER_ATTRIBUTES (WN* actual, ST* formal_st, ST_IDX replace_st = 0) :
	_formal_st (formal_st),
	_actual (actual),
	_replace_st (replace_st),
	formal_preg (0),
	addr_passed_actual (NULL),
	pass_method (PM_UNKNOWN),
	fixup_method (FM_UNKNOWN),
	reshape_method (RS_UNKNOWN),
	is_restrict_pointer (FALSE),
	actual_aliased (NULL) {

	Is_True (actual != NULL, ("missing actual argument"));
    }
}; // PARAMETER_ATTIBUTES


typedef vector<PARAMETER_ATTRIBUTES, mempool_allocator<PARAMETER_ATTRIBUTES> >
	PARM_ATTR_VEC;

typedef PARM_ATTR_VEC::iterator PARM_ITER;

typedef std::set<ST_IDX, std::less<ST_IDX>, mempool_allocator<ST_IDX> > PROCESSED_SET;

#ifdef KEY
struct formal_to_replace_st
{
    ST * formal;
    ST_IDX replace;
    formal_to_replace_st (ST * f, ST_IDX r) : formal (f), replace (r) {}
};

typedef vector<formal_to_replace_st, mempool_allocator<formal_to_replace_st> >
	replace_st_vec;
#endif
#include "clone.h"

#include "ipa_cg.h"

#include "ipo_inline_util.h"

// auxiliary info used by some parts of IPO_INLINE
struct IPO_INLINE_AUX
{
    PARM_ATTR_VEC parm_attr;		// parameter attributes
    RETURN_PREG rp;			// return pseudo register description
    LABEL_IDX return_label;		// replace RETURN in callee by GOTO 
					// to this label
    LABEL_IDX entry_label;		// mark the beginning of the
					// inlined body
    WN* copy_in_block;			// copy the actual to formal
    WN* copy_out_block;			// copy the result back to the actual
    WN* inlined_body;			// callee's function body
    WN* part_inl_leftover_call_site;	// the WN block which will contain the call
                                        // site to the leftover function 
                                        // after partial inlining.
    PROCESSED_SET processed_local_syms; // Set of symbols that have PRAGMA LOCAL processed
#ifdef KEY
    replace_st_vec replace_st;
#endif
    
    IPO_INLINE_AUX (INT num_formals, MEM_POOL* pool) : 
	parm_attr (pool),
	processed_local_syms (std::less<ST_IDX>(), pool),
	rp (),
	return_label (0),
	entry_label (0),
	copy_in_block (NULL),
	copy_out_block (NULL),
	inlined_body (NULL),
        part_inl_leftover_call_site (NULL)
#ifdef KEY
	, replace_st (pool)
#endif
    {

	parm_attr.reserve (num_formals);
    }

    BOOL Local_ST_Not_Processed(ST* st) {
	Is_True(ST_level(st) != GLOBAL_SYMTAB, ("Invalid ST to be processed"));
	
	return ((processed_local_syms.insert(ST_st_idx(st))).second);
    }
    
}; 
    
    
// forward declarations
class IPA_NODE;

class IPO_INLINE
{
private:

    // ======================================================================
    // INPUT data members 
    // ======================================================================
  
    IPA_NODE *_caller_node;		// caller wn, symtab etc (also global)
    IPA_NODE *_callee_node;		// callee wn, symtab etc
    IPA_EDGE *_call_edge;		// call edge
    IPO_SYMTAB *_symtab;		// For handling STs
  
    mUINT32  _flags;			// various Boolean attribute flags
    INT _callee_cross_file_id;		// For Debugging purpose

#ifdef KEY
    // Merge callee's exception typeinfo table into caller's
    void Merge_EH_Typeinfo_Tables (void);
    // Merge callee's exception specification table into caller's
    void Merge_EH_Spec_Tables (void);
#endif
public:
    // ======================================================================
    // member functions
    // ======================================================================

    void Set_caller_node (IPA_NODE* caller_node) {_caller_node = caller_node;}
    void Set_callee_node (IPA_NODE* callee_node) {_callee_node = callee_node;}
    void Set_call_edge (IPA_EDGE* edge)		{_call_edge = edge;}
    void Set_symtab (IPO_SYMTAB* symtab)	{_symtab = symtab;}
    void Set_callee_cross_file_id (INT file_id){_callee_cross_file_id = file_id;}

    IPA_NODE* Caller_node () const	{ return _caller_node; }
    IPA_NODE* Callee_node () const	{ return _callee_node; } 
    IPA_EDGE* Call_edge () const	{ return _call_edge; }    
    IPO_SYMTAB* Symtab () const		{ return _symtab; }
    INT	Callee_cross_file_id () const	{ return _callee_cross_file_id; }

    // Access to IPO_INLINE flags
    void Set_flags (UINT32 flags)         { _flags |= flags; }
    void Clear_flags (UINT32 flags)       { _flags &= ~flags; }
    UINT32 Flags () const                 { return _flags; }

    // ======================================================================
    // short hands for accessing data in IPA_NODE
    // ======================================================================

    WN* Caller_Wn () const	{ return _caller_node->Whirl_Tree();}
    WN* Callee_Wn () const	{ return _callee_node->Whirl_Tree();}
    WN* Call_Wn () const	{ return _call_edge->Whirl_Node();}

    SUMMARY_PROCEDURE* Caller_Summary_Proc () const {
	return _caller_node->Summary_Proc ();
    }
    SUMMARY_PROCEDURE* Callee_Summary_Proc () const {
	return _callee_node->Summary_Proc ();
    }

    SCOPE* Caller_Scope ()	{ return _caller_node->Scope_Table (); }
    SCOPE* Callee_Scope ()	{ return _callee_node->Scope_Table (); }

    WN_MAP_TAB* Caller_Map_Table () const { return _caller_node->Map_Table(); }
    WN_MAP_TAB* Callee_Map_Table () const { return _callee_node->Map_Table(); }

    SYMTAB_IDX Caller_level() const { return _caller_node->Lexical_Level();}
    SYMTAB_IDX Callee_level() const { return _callee_node->Lexical_Level();}
    
    FILE_INFO& Caller_file_info() const { return _caller_node->File_Info();}
    FILE_INFO& Callee_file_info() const { return _callee_node->File_Info();}

    DST_TYPE Caller_file_dst() const { return _caller_node->File_Dst();}
    DST_TYPE Callee_file_dst() const { return _callee_node->File_Dst();}

    DST_IDX Caller_dst() const	{ return _caller_node->Dst_Index();}
    DST_IDX Callee_dst() const	{ return _callee_node->Dst_Index();}

    // ======================================================================
    // IPO_INLINE main functions
    // ======================================================================
    void Process_Op_Code (TREE_ITER& iter, IPO_INLINE_AUX& aux);

    void Reshape_Array (TREE_ITER& iter, PARAMETER_ATTRIBUTES& parm,
			IPO_INLINE_AUX& aux);

    void Process_Formal_ST (TREE_ITER& iter, ST* cp, IPO_INLINE_AUX& aux);

    void Process_ST (TREE_ITER& iter, IPO_INLINE_AUX& aux);

    void Walk_and_Update_Callee (IPO_INLINE_AUX& aux);

    WN * Clone_Callee( BOOL); // Use IPO_CLONE class to clone callee in inlining

    // pre-processing in caller needed BEFORE Process_Callee (eg mp processing)
    void Pre_Process_Caller (LABEL_IDX& return_label);

    // Process_Callee is the main routine: does most of the work
    void Process_Callee (IPO_INLINE_AUX& aux, BOOL same_file);  

    void Process_OPR_REGION(WN* wn, IPA_NODE* caller_node);

    // ======================================================================
    // Process Formal parameters		    
    // ======================================================================

    pair<ST*, WN_OFFSET> Create_Copy_In_Symbol (ST* formal_st);

    void Process_Copy_In (PARM_ITER parm, WN* copy_in_block);

    void Process_Copy_In_Copy_Out (PARM_ITER p, IPO_INLINE_AUX& aux);

    void Process_Barriers (PARM_ITER parm, WN* copy_in_block);

    void Process_Formals (IPO_INLINE_AUX& aux);

    void GenBarrierForEquivActual(WN*, WN*, WN*, BOOL [] );

    // ======================================================================
    

    // ======================================================================
    BOOL SubstituteFormal (ST* , WN* , INT);

    // ======================================================================
    // general purpose routines               
    // ======================================================================
    PREG_NUM Process_Alloca_Preamble();
    void Process_Alloca_Postamble(PREG_NUM);

    // Insert inline pragmas, alloca-code
    void Post_Process_Caller (IPO_INLINE_AUX& aux);
#ifdef KEY
    // Merge callee's exception tables into caller's
    void Merge_EH_Tables (void);
#endif

public:
    // Externally callable functions

    // constructor (IPA, INLINE)

    IPO_INLINE (IPA_NODE *caller_node, IPA_NODE *callee_node, IPA_EDGE *edge);

    //process : do actual inlining           
    void Process();

};

#endif // cxx_ipo_inline_INCLUDED
