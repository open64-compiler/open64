/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


// -*-C++-*-

/**
*** This file has some miscellaneous routines of interest to LNO/C++ people.
***
***	INT32 Dot_Product(const mINT32* v1, mINT32* v2, INT count)
***	INT64 Dot_Product(const mINT64* v1, mINT64* v2, INT count)
***	INT64 Dot_Product(const mINT32* v1, mINT64* v2, INT count)
***	INT64 Dot_Product(const mINT64* v1, mINT32* v2, INT count)
***
***	    Sum(c=0 to c=count-1) (v1[c]*v2[c])
***
***
***	void Dump_WN(WN*, FILE*, INT fancy,
***			INT indent_start = 2, INT indent_inc = 2,
***			ARRAY_DIRECTED_GRAPH16* =NULL,
***			WN** list = NULL, WN* parent = NULL, 
***                     BOOL recursive = TRUE)
***
***	    Dumps the WN node and all its children (unless recursive is 
*** 	    FALSE in which case just dump the node) into the specified file
***	    in a fairly human readable format.  'fancy' of 0 is boring
***	    output.  1 is more concise.  2 do loop info.  3 parents.
***	    'indent_start' indicates how much whitespace
***	    to put before each node, and indent_inc indicates how much more
***	    whitespace to add for each level of structured control flow.
***	    list is a NULL terminated array of WNs.  If one of these is
***	    encountered, the output is annotated with '**%x**' for that
***         node.  If a dependence graph is passed in (non-null), each
***         load/store is looked up in the graph, and if found, a '[v#]'
***         is put next to the opcode.
***
***     inline WN* LWN_Make_Icon(TYPE_ID wtype, INT64 i)
***
***         Make an integer constant whirl node.
***
***     TYPE_ID Max_Wtype(TYPE_ID a, TYPE_ID b)
***
***         Given two indices to be
***         added/subtracted/maxed/mined, what is the type
***         that holds them?  It's not an easy question.  If
***         we have 32 bit signed and 32 bit unsigned, there
***         is no correct answer other than 64 bit signed.
***         Since we are trying not to expand, given that the
***         odds of there being a problem are infinitesimal,
***         we risk a bug by choosing a 32 bit version, but
***         it's the only real choice.  Which 32 bits to
***         choose??  Could be wrong either way.  We prefer
***         the signed, because that's probably the right
***         choice.  Making every new index a signed 64 bit
***         might be closer to correct.
***
***         Thankfully, in FORTRAN it's rarely a problem,
***         since everything's the same type.
***
***     INT Do_Depth(WN* wn, WN** loops = NULL, INT mx = 0)
***
***         How deep is this statement nested within do loops.
***         The outermost loop is loop 0.
***
***     INT Good_Do_Depth(WN* wn, WN** loops = NULL, INT mx = 0)
***
***         How deep is this statement nested within good do loops.
***         The outermost loop is loop 0.  Good dos outside of bad
***	    dos don't count.  The number of dependence components in
***	    an edge must exactly agree with the common nesting of this.
***
***     void Build_Doloop_Stack(WN* wn, DOLOOP_STACK* stack)
***
***         Build a do loop stack. Pass in an empty but non-NULL DOLOOP_STACK
***         when calling.
***
***     void Replace_Symbol(WN* wn, SYMBOL symold, SYMBOL symnew,
***                         WN* alias_wn, WN* ancestor = NULL)
***
***         Walk the tree of wn and replace all instances of old
***         symbol with the new symbol.
***	    The new symbol is a pseudo register iff alias_wn is NULL.
***         Otherwise, alias_wn holds a WN from which we can steal alias info.
***         When one knows that the symbol can only have du chain arcs
***         pointing from that symbol to itself, then one can call with
***         ancestor being non-NULL.  Specifically, Replace_Symbol(wn,...,wn).
***
***     void Replace_Symbols(WN* wn, SYMBOL* sold, SYMBOL* snew, INT count,
***                          WN** alias_wns, WN* ancestors = NULL)
***
***         Walk the tree of wn and replace all instances of old
***         symbols with the corresponding new symbol.
***	    All new symbols are pseudo registers iff alias_wns is NULL.
***         Otherwise, alias_wns[i] holds alias info for snew[i].
***         All ancestors are null if ancestors==NULL; otherwise use
***         ancestors[i].
***
***     BOOL Add_To_Symbol(WN* wn, INT64 i, ST* st,
***                        WN_OFFSET offset, TYPE_ID wtype, BOOL stok = FALSE)
***     BOOL Add_To_Symbol(WN* wn, INT64 i, SYMBOL sym, BOOL stok = FALSE)
***
***         Recursively replace st with st+i.  If stok is true, then a store
***	    is ok, and is ignored.  Typically, stok is false, because when
***	    adding to a symbol, we don't expect to see a store to it.
***	    The return value is TRUE if anything changed that has not been
***	    simplified.  NOTE: The first version is obsolete.
***
***     WN* Replace_Wnexp_With_Exp_Copy(WN* wn, WN* expr, DU_MANAGER* =NULL, 
***	    BOOL* added_cvt=NULL,ARRAY_DIRECTED_GRAPH16 *dep_graph=NULL)
***
***         Replace wn with a copy of expr, and adjust parent pointers.
***	    If DU parameter is not null, then assume that expr's ldids
***	    when copied to wn arise from the same defs.  It will do casts
***         between integer types, and otherwise doesn't cast, so be careful.
***	    Returns a WN* to the copy of the expression.  If *added_cvt != 
***	    NULL, set *added_cvt to TRUE if a convert was added to the 
***	    expression after copying, set *added_cvt to FALSE otherwise.  
***         
***	WN* Replace_Scalar_Store_With_Array_Store(WN* scalar_store, 
***					          WN* array_store, 
***					          DU_MANAGER* =NULL)
***
***	    Replace the 'scalar_store' with the 'array_store' and 
***	    adjust the parent pointers.  The DU parameter is used in the 
***	    same way as in Replace_Wnexp_With_Exp_Copy() above.  
***
***	void Replace_Ldid_With_Exp_Copy(
***		SYMBOL symbol,
***		WN* wn,
***		WN* expr,
***		DU_MANAGER* du = NUL,
***             ARRAY_DIRECTED_GRAPH16 *dep_graph=NULLL
***	    );
***
***	Replace occurances of LDID symbol recursively found in wn
***	with a copy of the given expression.  Adjust parent pointers.
***	If the du/dep_graph is not null, then also update by assuming
***	that the uses in wn that are copied from expr will arise from the
***	same defs as in expr.
***
***	WN* LWN_Integer_Cast(WN* tree, TYPE_ID to, TYPE_ID from)
***
***	    Create a cast which casts 'tree' from type 'from' to type 'to'. 
***  
***	WN* LWN_Integer_Casts(WN* tree, TYPE_ID to, TYPE_ID from)
***
***	    Create a cast which casts 'tree' from type 'from' to type 'to',
***	    producing a CVTL as well as a CVT, if necessary. 
***  
***	SYMBOL Create_Preg_Symbol(const char* name, TYPE_ID type)
***
***	    Makes a new preg symbol with the given name and type.
***
***	SYMBOL Create_Stack_Symbol(const char* name, TYPE_ID type)
***
***	    Makes a new local stack variable symbol with the given name and type.
***
***	ST* Lookup_Function_Name(const char* name);
***
***	    Returns ST for this function, creating a new one if necessary.
***
***      void Reset_Do_Loop_Depths(WN* loop, INT depth)
***
***	    Set this loop to the given depth and loops further in
***	    appropriately.
***
***      void Unmapped_Vertices_Here_In(WN* wn)
***
***	    Indicate in all DO annotations from here in that a vertex may
***	    be unmapped.  NOTE: assumes there exist unmapped vertices.
***
***	TYPE_ID Do_Wtype(WN* wn)
***
***	    The TYPE_ID of the index for the DO_LOOP.
***
***	ACCESS_VECTOR* Difference_Inequality(
***			ACCESS_VECTOR* lb,
***			ACCESS_VECTOR* ub,
***			INT var,
***			DIFFERENCE_KIND code,
***			MEM_POOL* pool
***	);
***
***	    Allocate from pool and compute an access vector
***	    that represents either the inequality
***	    ub - lb < 0, ub - lb <= 1, or ub - lb >= 0, depending on the
***	    DIFFERENCE_KIND, which can be one of DIFFERENCE_EXEC_NEVER,
***	    DIFFERENCE_EXEC_NEVER_OR_ONCE, DIFFERENCE_EXEC_ALWAYS.  The
***	    expression is created for variable var, typically Nest_Depth()-1.
***
***	void Print_Def_Use(WN *func_nd, FILE *fp)
***
***	    Print out all the def-use/use-def chains for things rooted
***	    at func_nd.
***
***	BOOL Statically_Safe_Exp(WN *wn)
***
***	    Is this expression safe to speculate?  Currently, we say it is
***         if it only contains, ldids, INTCONST, CONST, +, -, *, MAX, MIN,
***         ABS,NEG,SQRT,BNOT,LNOT,COMPLEX,EQ,NE,GE,GT,LE,LT,BAND,BIOR,BXOR,
***	    LAND,LIOR,SHL,ASHR,LSHR,SELECT,MADD,MSUB,NMADD,NMSUB.  
***	    Furthermore, for floating point
***	    ops and complex ops, we require that TENV:X >= 2.  If
***	    X >= 3, everything is safe except Loads and expression calls. 
***	    If X >= 4, always return TRUE
***
***	extern BOOL Statically_Safe_Node(WN* wn)
***
***	    Returns TRUE if the node is safe to speculate, FALSE
***	    otherwise.
***
***	void Unrolled_DU_Update(WN **bodies, UINT u, INT loopno,
***		BOOL update_pointers = TRUE, BOOL cross_index=TRUE)
***
***	    Fix the def-use and use-def chains after unrolling.
***	    Loopno is the number of the loop being unrolled u times.
***	    bodies[0..u-1] are identical copies of code
***	    bodies 0 is the original.
***	    Return 0 on error
***
***	    If update_pointers is true, then def_list pointers to loops
***	    that are themselves in the bodies are updated.  This is the
***	    default, and you usually want this, unless you know the do
***	    loops in the boides are placeholders and that really you will
***	    put things (without the loops) back into bodies[0].  This
***	    can be convenient for certain kinds of unrolling.
***
***	    If cross_index is FALSE, we don't put in any cross edges
***	    between bodies[i] and bodies[j] for references to loop variable 
***	    loopno
***
***         THIS ROUTINE REQUIRES THAT LOOPS BE ANNOTATED WITH CORRECT
***         DEPTH INFORMATION.
***
***     ST *Get_ST_Base(WN *load)
***
***	    Find the ST_Base of a memory reference.  If it's an OPR_LDA, just
***	look at it's ST_Base(WN_st(ref)).  Otherwise, follow the def-use
***	chain backwards looking for a single OPR_LDA.  Return NULL if 
***	can't find the base or if there is more than one possible base.
***
***     BOOL Index_Variable_Live_At_Entry(WN* loop)
***     BOOL Index_Variable_Live_At_Exit(WN* loop)
***
***         Use this function to determine whether loop's index variable
***         is possibly live on exit/entry.  Don't use exit routine unless
***         you know that the DU/UD information is up to date.  Entry routine
***         just sees if variable is on start list.
***
***     WN* Outermost_Enclosing_Loop(WN* loop)
***
***          Returns the outermost loop enclosing 'loop', which may be 
***          'loop' itself.
***
***     void Finalize_Index_Variable(WN *wn, BOOL insert_after_loop, 
***	    BOOL try_sink)
***	
***	    Use this function to generate a final value expression for 
***	    the loop 'wn' after or before the loop.  If 'insert_after_loop', 
***	    insert the final value expression after the loop, otherwise put  
***	    it before the loop.  If 'insert_after_loop' and 'try_sink', 
***	    attempt to sink it out of as many loops as possible. 
***
***     void Finalize_Index_Variable_For_Remove_Unity_Trip_Loop(WN *wn, 
***	    BOOL insert_after_loop, BOOL try_sink)
***
***	    Same as the above, but doesn't require Upper_Bound_Standardize()
***	    to work, because the final value is the initial value. 
***	
***     void DU_Sanity_Check(WN* func_nd, FILE* fp, INT fancy)
***
***         I'm not sure how this differs from LNO_Check_Du.  I know it's
***         better, but I don't know if both should exist.
***
***     void LNO_Check_Graph(ARRAY_DIRECTED_GRAPH16* dg);
***
***         More extensive checking than just dg->Check_Graph() -- makes
***         sure the arcs are the appropriate length.
***
***     void LNO_Check_Du(WN* func_nd)
***
***         Defined only when Is_True_On is set.  Check that the function's
***         DU/UD arcs are a superset of those generated by rerunning preopt.
***
***     void IV_Loop_Stmt_Check(WN* wn, MEM_POOL* pool)
***
***         Called from DU_Sanity_Check, but you can call this on your own.
***         Check that loop_stmt() for induction variables point to the right
***         thing.  Print a warning and fix it if not.
***
***     extern WN* WN_prev_executable(WN* wn)
***     extern WN* WN_next_executable(WN* wn)
***
***         Call WN_prev/next, skipping over non-executable wn's.
***
***     void LNO_Erase_Vertices_In_Loop(WN* wn,ARRAY_DIRECTED_GRAPH16* dg);
***
***         Deletes all vertices directly in the DO loop 
***	    surrounding wn (wn itself if wn is a loop).  Don't erase
***	    anything in a deeper loop.
***
***	 BOOL Wn_Is_Inside(WN* inner, const WN* outer);
***	    
***	    Returns TRUE if and only if 'outer' is an ancestor of 'inner'
***	    (or if they are the same.) 
***
***	 BOOL Do_Loop_Is_Unsigned(WN* wn_loop)
***
***	    Returns TRUE if the do loop 'wn_loop' an unsigned index
***	    variable, FALSE otherwise.
***
***      BOOL Upper_Bound_Standardize(WN* ub, BOOL = FALSE)
***
***         Make the upper bound look like i <= bound.  Assertion fails
***         if cannot do this (except if ok_to_fail is set, then just
***         return false).
***
***      INT64 LWN_Get_Linenum(const WN* wn)
***
***         Like WN_Get_Linenum (get the linenumber for the wn), but if
***         WN_Get_Linenum returns zero (presumably because of missing
***         info or because this wn is not a statement), then get the
***         line number of the parent
***
***      BOOL Is_Permutation_Vector(const INT order, INT count)
***
***         Return TRUE if the order[] vector is a permutation of 
***         the vector (0 1 ... nloops-1), FALSE otherwise. 
***
***      BOOL Are_Permutations(const INT* order1, const INT* order2, INT count)
***
***         Return TRUE if the order1[] vector contains no duplicates,
***         likewise for order2[], and the elements they do contain are
***         the same.  FALSE otherwise.
***
***      BOOL Is_Loop_Invariant_Use(WN* wn, WN* outerloop)
***
***         Returns TRUE if use 'wn' is invariant in the 'outerloop'.
***         FALSE if it is unknown or not loop invariant.
***
***      BOOL Is_Loop_Invariant_Exp(WN* wn, WN* outerloop)
***
***         Returns TRUE if all uses in the expression 'wn' are invariant
***         in the 'outerloop'.  FALSE if it at least one of them are either 
***         unknown or not loop invariant.
***
***      HASH_TABLE<WN*,WN*>* Make_Loop_Mapping(WN* orig, WN* copy, MEM_POOL*)
***
***         Create a hash table from the specified pool.  The hash table
***         maps a WN do loop to a do loop.  orig a loop, and so is copy.
***         The original and copy must be identical except possibly for
***         some expression trees (e.g. if header, do step, etc).
***         The loop structure must be identical.  A hash table is returned
***         with a map of the original loops to the corresponding in the
***         copy.  <orig,copy> is in the hash table
***
***      HASH_TABLE<WN*,BOOL>* Find_Loops_Within(WN* orig, MEM_POOL*)
***
***         Create a hash table from the specified pool.  The hash table
***         holds "TRUE" for every WN* that is a loop within orig, including
***         orig itself, if orig is a loop (which it needn't be).
***
***	 BOOL Equivalent_Access_Arrays(ACCESS_ARRAY *array1, 
***         ACCESS_ARRAY *array2, WN *wn1, WN *wn2)
***
***	    Returns TRUE if these two access vectors equivalent, with 
***         neither having symbolic terms varying in the common loops of
***	    the two references.
***	
***	 INT Num_Common_Loops(WN *wn1, WN *wn2)
***
***	    Returns the number of loops that the array references 'wn1'
***	    and 'wn2' share in common.
***
***	 WN* LNO_Common_Loop(WN *wn1, WN *wn2)
***
***	    Returns the innermost common loop.  NULL if none.
***
***	 WN* Enclosing_Do_Loop(WN* wn)
***
***	    Returns the do loop enclosing 'wn', NULL if there is no enclosing
***
***	 WN* Enclosing_Loop(WN* wn)
***
***	    Returns the loop enclosing 'wn', NULL if there is no enclosing
***
***	 WN* Enclosing_Loop_Body(WN* wn)
***
***	    Returns the loop whose body encloses 'wn', NULL if there is no
***	    loop body enclosing 'wn'.
***
***	 BOOL Loop_Is_Trapezoidal(WN* wn_loop,
***   				  ARRAY_DIRECTED_GRAPH16* dg,
***                               DU_MANAGER* du)
***
***	    Returns TRUE if the loop is trapezoidal, FALSE otherwise.
***
***      class LS_IN_LOOP
***	   
***	    LS_IN_LOOP(WN* loop, ARRAY_DIRECTED_GRAPH16*, MEM_POOL* pool,
***		BOOL use_scalars = FALSE);
***
***	      Create a list of the loads and stores in the 'loop', given 
***	      the dependence graph, and using the memory 'pool'.  If 
***	      'use_scalars' is TRUE, put scalar loads and stores in the 
***	      list, otherwise only put arrays in the list. 
*** 
***	    INT In(WN* wn)
***
***	      Return the value of the data field associated with the 
***	      node in the loop list. 
***
***	    Depth 
***
***	      The Do_Depth() of the loop, i.e. the number of do loops 
***	      surrounding this loop, including the loop itself. 
***
***	    Good_Depth
***
***	      The Good_Do_Depth() of the loop, i.e. the Do_Depth()  
***	      minus the number of bad do loops enclosing the loop. 
***
***	    Loop 
***	
***	      The loop itself. 
***
***	 class LS_IN_LOOP_ITER
***
***	    A class of to iterate through the members of the above class. 
***
***
***	 void Remove_Redundant_Stids(WN* wn_start, DU_MANAGER* du)
***
***	   Removes all STIDs in the tree with root 'wn_start' which have 
***	   no uses, as indicated by the DU_MANAGER '*du'.  
***	
*** 	 OPCODE Matching_Load_Opcode(OPCODE store_op)
***
***	   Returns the load opcode which corresponds to the store opcode
***	   'store_op'. 
***
***      WN* Create_ILoad_From_IStore(WN* wn_store, DU_MANAGER* du, 
***	   ARRAY_DIRECTED_GRAPH16* dg)
***
***	   Return an array load patterned after the array store 'wn_store'.
***	   If 'du' is non-null, copy the DU information from 'wn_store' to the
***	   newly created array node. If 'dg' is non-null, copy the dependence
***	   information from 'wn_store' to the newly_created array node. 
***
***      BOOL Is_Local_Array_Reference(WN* array)
***
***        Input must be an OPR_ARRAY, else return FALSE.  Otherwise, output
***        is whether it definitely references a local array.
***
***      extern void FB_Sanity_Check(WN *wn)
***        
***        Check if every WN rooted at wn has frequency count set.
***
***      extern BOOL Is_Mp_Region(WN *wn)
***
***	   Is this an MP OPC_REGION node
***
***	 extern RID * Get_Enclosing_Region_ID(WN *wn)
***
***	   Returns the RID of the region enclosing the node 'wn'. 
***
***      extern void Remark_Depth(WN *wn, ,mUINT8 depth)
***
***        Go through the code to set the depth field as this might
***        have been changed due to dismantling or loop peeling.
***        'depth' is the number of enclosing do loops.
***
***      extern WN*& UBexp(WN* end, BOOL* ne = NULL)
***      extern WN*& UBvar(WN* end)
***
***             Given a do loop, pass WN_end(doloop) as a parameter.
***             The function then returns kid1 (kid0 for UBvar) when end's
***             opr is LE/LT.  If it's GE/GT, return kid0 (kid1).
***             Otherwise error.  If it's GT or LT, then ne is set to TRUE,
***             if it's passed.  Otherwise FALSE.
***
***	extern INT Loop_Depth(WN* stat)
***
***	  Returns the depth of the loop in which 'stat' is nested. 
***       Returns -1 if 'stat' is not nested within any loop. 
***
***	extern INT Block_Loop_Depth(WN* stat)
***
***	  Similar to 'Loop_Depth', except that index expressions for DO 
***	  loops are considered to belong to the next outermost enclosing 
***	  DO loop rather than to the DO loop of which they are a part. 
***
***	extern void Add_Pragma_To_MP_Region (const WN* wn, 
***                                     ST* st, WN_OFFSET offset,
***	                                    WN_PRAGMA_ID pragma_id,
***			                    BOOL make_compiler_generated=FALSE)
***
***	extern void Add_Pragma_To_MP_Region (const WN* wn, 
***                                     ST* st_idx, WN_OFFSET offset,
***	                                    WN_PRAGMA_ID pragma_id,
***			                    BOOL make_compiler_generated=FALSE)
***
***	  Find an MP region containing "wn" (if any). If so,
***	  add a pragma node of the specified kind for the specified ST.
***       Mark the pragma as compiler-generated, if so requested.
***
***     extern void  MP_Sanity_Check_Func(WN *func_nd)
***
***	extern void Update_MP_Local_Var(ST* st, WN_OFFSET offset, WN *wn)
***
***	  Given that we're creating a new local variable, put in on 
***	  the local list of the innermost MP regions surrounding wn
***
***	extern WN* Outer_Tile(WN* wn_loop, DU_MANAGER* du)
***
***	  Returns the outer tile loop corresponding to the inner tile loop 
***	  'wn_loop'.  Returns NULL if this is not an inner tile loop or if
***	  we can't determine what the outer tile loop is.     
*** 
***	  In this function, we define "outer tile" and "inner tile" loops
***	  as a pair of loops of the form: 
***		do ii = LBii, UBii, Sii 
***		  do i = f1(ii), f2(ii), Si 
***
***	extern WN* Return_Node(WN* wn_func_nd)
***
***	  Returns one of the function "wn_func_nd"'s return nodes.
***
***	extern WN* Common_Loop_Ancestor(WN* wn1, WN* wn2)
***
***	  Returns the loop with the greatest depth which is a common
***	  ancestor of both 'wn1' and 'wn2'.  Returns NULL if there is no loop
***	  which is a common ancestor of both.
***
***	extern void Build_Parent_Stack(WN* wn, STACK<WN*>* stack)
***
***	  Push all of the ancestors of 'wn' (including 'wn' itself) onto
***	  the 'stack'. 
***
***	extern WN* Common_Ancestor(WN* wn1, WN* wn2)
***
***	  Returns the node furthest away from the FUNC_ENTRY node which
***	  is a common ancestor of both 'wn1' and 'wn2'.  Returns NULL if
***	  there is no loop which is a common ancestor of both. 
***
***	extern BOOL Is_Lex_Before(WN* wn1, WN* wn2) 
***
***	  Returns TRUE if 'wn1' is lexically before 'wn2', FALSE
***	  otherwise. 
***
***	extern INT Symbol_Count(WN* wn, const SYMBOL& sym)
***
***	  Returns the number of LDIDs which are encountered with symbol 
***	  'sym' in the tree rooted at 'wn'. 
***
***	extern WN* Find_Node(SYMBOL sym, WN* wn_tree)
***
***	  Find the first node the the tree rooted at 'wn_tree' with 
***	  symbol 'sym' and return it.
***
***	extern BOOL Identity_Permutation(INT permutation[], INT nloops) 
***
***	  Returns TRUE if the 'permutation' of length 'nloops' is the
***	  identity permutation, FALSE otherwise.
***
***	extern WN* Trip_Count(WN* wn_loop)
***
***	  Returns the trip count of the loop 'wn_loop'.
***
***	extern INT Reduce_Permutation(INT permutation[],
***                           INT nloops,
***                           INT spermutation[],
***                           INT max_reduction)
***
***	  Let M be the largest integer for which the first M components
***	  of the 'permutation' of length 'nloops' is the identity permutation.
***	  Let N = MIN(M, 'reduction_max').  Return nloops - N and set 'spermu-
***	  tation' to the reduced permutation consisting of the last 'nloops' 
***	  - N components of 'permutation', adjusted so that they are a permu-
***	  tation.
***
***	 BOOL Index_Variable(WN* wn_index)
***
***	  Return TRUE if 'wn_index' is an STID or LDID of an index
***	  variable for some loop enclosing it.  Returns FALSE otherwise.
***
***	BOOL Contains_Dedicated_Preg(WN* wn_tree)
***
***	  Returns TRUE when 'wn_tree' contains a reference to a dedicated
***	  preg, FALSE otherwise.
***
***	INT Factorial(INT n)
***	
***	  Returns the factorial of 'n'. The value 'n' must be a non-
***	  negative integer.
***
***	void Permutation(INT order, INT nloops, INT permutation[])
***	
***	  Sets 'permutation' to the 'order'th permutation of length
***	  'nloops'.
***
***	INT WN_Whirl_Linenum(WN* wn_ref)
***
***	  Returns the best guess at a line number for 'wn_ref'.  This
***	  is the line number of the node itself, if it has one, or that of 
***	  the closest enclosing parent node that has one, or 0, if no 
***	  enclosing parent node has one.
***
***	void Constant_Propogate(WN *stid, INT64 const_val)
***
***	  Use the DU chains to constant propogate the statement
***	  stid = const_val
***
***	WN* Messy_Subscript(WN* wn_array)
***
***	  If 'wn_array' has an OPR_ARRAY ancestor with a Too_Messy sub-
***	  script, return the WN* of that ancestor.  Otherwise, return NULL.
***
***	void Replace_Index_Variable(WN* loop, WN* cp_loop, const char prefix[])
***
***	  Create a new index variable for the loop 'cp_loop' so that
***	  its index variable is not the same as that of 'loop'.  Give the new
***	  index variable a name that starts with 'prefix[]'.
***
*** 	WN* Enclosing_Proper_Do_Loop(WN* wn_ref)
***
***	  Returns the closest loop for which 'wn_ref' is in its WN_do_body(),
***	  if there is such a loop, returns NULL otherwise.
**/

#ifndef lnoutils_INCLUDED
#define lnoutils_INCLUDED "lnoutils.h"

class WN;

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif
#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef stab_INCLUDED
#include "stab.h"
#endif
#ifndef dep_graph_INCLUDED
#include "dep_graph.h"
#endif
#ifndef region_util_INCLUDED
#include "region_util.h"
#endif
#ifndef NAME_DECLARE
#include "name.h"
#endif

/*
 * Input may be any integer
 */

class DU_MANAGER;

class LS_IN_LOOP {
  friend class LS_IN_LOOP_ITER;
 private:
  enum          {HT_ELTS=247};  // not a power of two, when pointer is elt
  HASH_TABLE<WN*,INT>   _ht;
  void          Lexorder(WN* wn, ARRAY_DIRECTED_GRAPH16* dg, INT* lex, 
		  BOOL include_scalars = FALSE);
 public:
  LS_IN_LOOP(WN* loop, ARRAY_DIRECTED_GRAPH16*, MEM_POOL* pool, 
		  BOOL use_scalars = FALSE);
  INT           In(WN* wn) {return _ht.Find(wn);}       // lexcount
  INT           Depth;
  INT           Good_Depth;
  WN*           Loop;
};

class LS_IN_LOOP_ITER {
 private:
  HASH_TABLE_ITER<WN*,INT>      _hti;
 public:
  LS_IN_LOOP_ITER(LS_IN_LOOP* a) : _hti(&a->_ht) {}
  WN* Step() {WN* wn; INT lex; BOOL b = _hti.Step(&wn, &lex);
              return b ? wn : NULL;}
};

extern INT32 Dot_Product(const mINT32* v1, const mINT32* v2, INT count);
extern INT64 Dot_Product(const mINT64* v1, const mINT64* v2, INT count);
extern INT64 Dot_Product(const mINT32* v1, const mINT64* v2, INT count);
extern INT64 Dot_Product(const mINT64* v1, const mINT32* v2, INT count);
extern void Update_MP_Local_Var(ST* st, WN_OFFSET offset, WN *wn);

extern void Dump_WN(WN*, FILE*, INT, INT = 2, INT = 2,
                    ARRAY_DIRECTED_GRAPH16* =NULL, WN** = NULL, WN* = NULL,
		    BOOL recursive = TRUE);
extern void Enum_loops(WN *);

// Good stuff extracted from SNL codes.

extern WN* LWN_Make_Icon(TYPE_ID wtype, INT64 i);

extern TYPE_ID Do_Wtype(WN* wn);

extern TYPE_ID Max_Wtype(TYPE_ID a, TYPE_ID b);

extern INT64 Step_Size(WN* loop,
                       INT64 newstep);

extern INT64 Step_Size(WN* loop);

extern INT Do_Depth(WN* wn, WN** loops = NULL, INT mx = 0);

extern INT Good_Do_Depth(WN* wn, WN** loops = NULL, INT mx = 0);

extern void Build_Doloop_Stack(WN* wn, DOLOOP_STACK* stack);

extern void Replace_Symbol(WN* wn, SYMBOL symold, SYMBOL symnew,
                           WN*, WN* = NULL);

extern void Replace_Symbols(WN* wn, SYMBOL* sold, SYMBOL* snew, INT count,
                            WN**, WN** = NULL);

extern BOOL Add_To_Symbol(WN* wn, INT64 i, ST* st,
			  WN_OFFSET offset, TYPE_ID wtype, BOOL = FALSE);
extern BOOL Add_To_Symbol(WN* wn, INT64 i, SYMBOL symbol, BOOL = FALSE);

extern WN* Replace_Wnexp_With_Exp_Copy(WN* wn, WN* expr, DU_MANAGER* =NULL,
  BOOL* =NULL,ARRAY_DIRECTED_GRAPH16 *dep_graph=NULL);
extern WN* Replace_Scalar_Store_With_Array_Store(WN* scalar_store, 
  WN* array_store, DU_MANAGER* =NULL);

extern void Replace_Ldid_With_Exp_Copy(SYMBOL, WN* wn, WN* expr, 
	DU_MANAGER *du=NULL, ARRAY_DIRECTED_GRAPH16 *dep_graph=NULL);

extern WN* LWN_Integer_Cast(WN* tree, TYPE_ID to, TYPE_ID from); 

extern WN* LWN_Integer_Casts(WN* tree, TYPE_ID to, TYPE_ID from); 

extern SYMBOL Create_Preg_Symbol(const char* name, TYPE_ID type);

extern SYMBOL Create_Stack_Symbol(const char* name, TYPE_ID type);

extern ST* Lookup_Function_Name(const char* name);

extern void Reset_Do_Loop_Depths(WN* loop, INT depth);

enum DIFFERENCE_KIND {
  DIFFERENCE_EXEC_NEVER,
  DIFFERENCE_EXEC_NEVER_OR_ONCE,
  DIFFERENCE_EXEC_ALWAYS
};

ACCESS_VECTOR* Difference_Inequality(ACCESS_VECTOR* lb,
				     ACCESS_VECTOR* ub,
				     INT var,
				     DIFFERENCE_KIND code,
				     MEM_POOL* pool);
extern void Print_Def_Use(WN *func_nd, FILE *fp);
extern BOOL Statically_Safe_Exp(WN *wn);
extern void Unrolled_DU_Update(WN **bodies, UINT u, INT loopno, BOOL = TRUE,
		BOOL cross_index=TRUE);
extern ST *Get_ST_Base(WN *load);
extern BOOL Index_Variable_Live_At_Entry(WN* loop);
extern BOOL Index_Variable_Live_At_Exit(WN* loop);
extern WN* Outermost_Enclosing_Loop(WN* loop); 
extern void Finalize_Index_Variable(WN *wn, BOOL insert_after_loop=TRUE,
  BOOL try_sink=FALSE);
extern void Finalize_Index_Variable_For_Remove_Unity_Trip_Loop(WN *wn, 
  BOOL insert_after_loop=TRUE, BOOL try_sink=FALSE);
extern void LNO_Erase_Vertices_In_Loop(WN* wn,ARRAY_DIRECTED_GRAPH16* dg);
extern void Du_Sanity_Check(WN* wn, FILE* fp=stdout, UINT fancy=2);
extern void IV_Loop_Stmt_Check(WN* wn, MEM_POOL* pool);
extern void FB_Sanity_Check(WN *wn);
extern WN* WN_prev_executable(WN* wn);
extern WN* WN_next_executable(WN* wn);
extern BOOL Wn_Is_Inside(WN* inner, const WN* outer);
extern INT Which_Loop_Inside(WN* ref, const DOLOOP_STACK& stack, INT first);
extern INT Which_Loop_Inside(WN* ref, const DOLOOP_STACK& stack);
extern BOOL Upper_Bound_Standardize(WN* ub, BOOL = FALSE); 
extern INT64 LWN_Get_Linenum(const WN *wn);
extern BOOL Is_Permutation_Vector(const INT order[], INT count);
extern BOOL Are_Permutations(const INT* order1, const INT* order2, INT count);
extern BOOL Is_Loop_Invariant_Use(WN* wn, WN* outerloop);
extern BOOL Is_Loop_Invariant_Exp(WN* wn, WN* outerloop);
extern BOOL Is_Const_Array_Addr(WN *);
extern BOOL Is_Loop_Invariant_Indir(WN *);
extern HASH_TABLE<WN*,WN*>* Make_Loop_Mapping(WN*, WN*, MEM_POOL*);
extern HASH_TABLE<WN*,BOOL>* Find_Loops_Within(WN* orig, MEM_POOL*);
extern INT Num_Common_Loops(WN *wn1, WN *wn2); 
extern WN* LNO_Common_Loop(WN *wn1, WN *wn2); 
extern BOOL Equivalent_Access_Arrays(ACCESS_ARRAY *array1, 
  ACCESS_ARRAY *array2, WN *wn1, WN *wn2);
extern WN* Enclosing_Do_Loop(WN* wn); 
extern WN* Enclosing_Loop(WN* wn); 
extern WN* Enclosing_Loop_Body(WN* wn); 
extern BOOL Loop_Is_Trapezoidal(WN* wn_loop, ARRAY_DIRECTED_GRAPH16* dg,
  DU_MANAGER* du);
extern void Remove_Redundant_Stids(WN* wn_start, DU_MANAGER* du); 
extern OPCODE Matching_Load_Opcode(OPCODE store_op);
extern WN* Create_ILoad_From_IStore(WN* wn_store, DU_MANAGER* du,
  ARRAY_DIRECTED_GRAPH16* dg);
extern BOOL Is_Local_Array_Reference(WN* array);
extern BOOL Is_Global_As_Local(ST *);
extern BOOL Is_Mp_Region(WN *wn);
#ifdef KEY
extern BOOL Is_Eh_Or_Try_Region(WN *wn);
#endif
extern BOOL Do_Loop_Is_Mp(WN *wn);
extern RID* Get_Enclosing_Region_ID(WN *wn); 
extern BOOL Is_Nested_Doacross(WN* wn_loop); 
extern void Remark_Depth(WN *wn, mUINT8 depth);

extern WN* UBexp(WN* end, BOOL* ne = NULL);
extern WN* UBvar(WN* end);

extern BOOL Solve_For(WN* wn_top, const SYMBOL& sym);
extern INT Loop_Depth(WN* stat); 
extern INT Block_Loop_Depth(WN* stat); 
extern BOOL Do_Loop_Is_Unsigned(WN* wn_loop);
extern WN* Outer_Tile(WN* wn_loop, DU_MANAGER* du);

extern void Add_Pragma_To_MP_Region (const WN* wn,
                                     ST* st,
                                     WN_OFFSET offset,
                                     WN_PRAGMA_ID pragma_id,
				     BOOL make_compiler_generated=FALSE);

extern WN* Return_Node(WN* wn_func_nd);
extern WN* Loop_Step(WN* wn_loop);
extern WN* Common_Loop_Ancestor(WN* wn1, WN* wn2);
extern BOOL Statically_Safe_Node(WN* wn); 
extern void Build_Parent_Stack(WN* wn, STACK<WN*>* stack);
extern WN* Common_Ancestor(WN* wn1, WN* wn2);
extern BOOL Is_Lex_Before(WN* wn1, WN* wn2);  
extern INT Symbol_Count(WN* wn, const SYMBOL& sym);
extern WN* Find_Node(SYMBOL sym, WN* wn_tree);
extern BOOL Identity_Permutation(INT permutation[], INT nloops); 
extern WN* Trip_Count(WN* wn_loop); 
extern INT Reduce_Permutation(INT permutation[], INT nloops,
  INT spermutation[], INT max_reduction);
extern BOOL Index_Variable(WN* wn_index);
extern BOOL Contains_Dedicated_Preg(WN* wn_tree); 
extern INT Factorial(INT n);
extern void Permutation(INT order, INT nloops, INT permutation[]);
extern INT WN_Whirl_Linenum(WN* wn); 
extern void Constant_Propogate(WN *stid, INT64 const_val);
extern WN* Messy_Subscript(WN* wn_array);
extern void Replace_Index_Variable(WN* loop, WN* cp_loop, const char prefix[]);
extern WN* Enclosing_Proper_Do_Loop(WN* wn_ref);
extern void Create_Single_Region(WN* wn_parent, WN* wn_single, WN* wn_end); 
extern BOOL Dominates(WN *wn1, WN *wn2);
extern WN * Find_Containing_Store(WN *);

#ifdef Is_True_On
extern void LNO_Check_Du(WN* orig);
extern void LNO_Check_Graph(ARRAY_DIRECTED_GRAPH16* dg);
extern void  MP_Sanity_Check_Func(WN *func_nd);
#endif

#if defined(TARG_X8664) || defined(TARG_IA64) //introduced by bug 10953
extern WN *Inductive_Base_Addr_Const_Stride(WN *array, WN *loop, WN **base, 
               BOOL *inductive_use, BOOL *indirect_base, mINT32 *stride_val);

extern WN *Simple_Invariant_Stride_Access(WN *array, WN *loop, BOOL ck_induc_base,
                                  BOOL *inductive_use, BOOL *indirect_use);
#endif

#ifdef TARG_X8664
extern BOOL Is_Vectorizable_Inner_Loop(WN* loop);
extern BOOL Is_Vectorizable_Outer_Loop(WN* loop);
#endif

#endif // LNOUTILS_DECLARE
