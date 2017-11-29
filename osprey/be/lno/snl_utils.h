/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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
*** Description:
***
***	Code that implements utilities for SNLs.  None of this
***	is "exported" in that all user routines use the interface
***	described in snl.h, and snl.cxx uses these routines for its
***	own purposes.
***
*** Exported types:
***
***	SNL_MONO
***
***	    An enumerated type indicating whether an expression is
***	    constant, increasing, decreasing or ugly in a given symbol.
***	    The values are
***		SNL_MONO_INVARIANT
***		SNL_MONO_INC
***		SNL_MONO_DEC
***		SNL_MONO_OTHER
***
***	Exported functions
***
***	    void Print_Do_Stack(FILE*, const DOLOOP_STACK*)
***
***		Print the indexes of a DOLOOP_STACK.
***
***	    SNL_MONO Mono(
***		WN* wn,
***		SYMBOL symbol,
***		BOOL neg =FALSE
***	    );
***
***		Is wn increasing, decreasing, constant or ugly in symbol?
***		If neg is true, flip SNL_MONO_INC and SNL_MONO_DEC.
***
***	    BOOL Is_Lexpos(DEPV*, INT dims);
***	    BOOL Is_Lexpos(DEPV_ARRAY*);
***
***		If any dependence could possibly be lexneg, returns FALSE;
***		otherwise returns TRUE.  TODO OK: these could go in dep.h.
***
***	    void Increase_By(WN* wn, INT c, WN* parent =NULL, INT kid =-1);
***
***		Increases the wn expression by c.  Parent and kid are such
***		that WN_kid(parent,kid) is wn.
***
***	    WN* LWN_Create_Block_From_Stmts_Above(WN* wn);
***	    WN* LWN_Create_Block_From_Stmts_Below(WN* wn);
***
***		Take all statements on the same block as wn and above/below it,
***		remove them from that block, and return a new block containing
***		only them.
***
***	    INT64 Iterations(WN* loop, MEM_POOL* pool);
***
***		Returns how many times the loop goes, or -1 if not integral
***		or can't decide.
***
***	    WN* Find_Next_Innermost_Do(WN* loop)
***
***		Goes recursively in to find a DO inside the loop.  If the
***		loop contains more than one (not looking inside DOs found),
***		then NULL is returned: it returns a loop only when there
***		is exactly one at the next nest level.
***
***	    DOLOOP_STACK* Copy_Dostack(
***		const DOLOOP_STACK& stack,
***		MEM_POOL*
***	    );
***
***		Put a copy of this stack in the given mempool and return
***		a pointer.
***
***	    void SNL_Sanity_Check_Func(WN* wn)
***	    void SNL_Sanity_Check_Block(WN* wn)
***	    void SNL_Sanity_Check_Loop(WN* wn)
***	    void SNL_Sanity_Check_If(WN* wn)
***	    void SNL_Sanity_Check_Region(SNL_REGION region)
***	    WN* SNL_Sanity_Check_Exp(WN* wn)
***
***		Check if the block's/loop's/if's/region's/expression's
***		DO_LOOP_INFO and IF_INFO information is correct.
***		Assertion failure if not.  Also print a warning if
***		apparently missing DU/UD information.  Returns NULL
***             if there are no load/stores with missing vertices in
***             the dependence graph of Array_Dependence_Graph, and it 
***	        returns a sample one if there is.
***
***	    void Dump_WN(SNL_REGION, FILE*, INT,
***				INT = 2, INT = 2, WN** = NULL, WN* = NULL,
***				ARRAY_DIRECTED_GRAPH16* =NULL);
***
***		Like Dump_WN, but for SNL_REGIONs.
***
***	    void SNL_Add_Du_To_Index_Ldid(WN* loop, WN* code, DU_MANAGER*,
***                                       BOOL code_in_loop)
***
***
***		use = ldid		[so ldid must be an OPR_LDID]
***		def = WN_start(loop)
***		def = WN_step(loop)
***
***		code is any code where you want ldids of the loop index to
***		point to that for du information.  If the code is not in
***             the loop, don't set the loop stmt to be the loop --- set
***             it to null.  Note that in the TRUE case it does the right
***             thing, whereas in the FALSE case it might do the right thing,
***             but the user should think about what the loop_stmt ought
***             to be in that case.
***
***	    void SNL_Change_Du_To_Index_Ldid(WN* loop, WN* code, DU_MANAGER*,
***                                          BOOL code_in_loop)
***
***		same as above, but remove all def/use info first, then
***		add the above.  And also, this recurses through a tree,
***
***	    void SNL_Change_Du_Pointer(WN* oldptr, WN* ptr, WN* body,
***					DU_MANAGER*)
***
***		All deflist pointers that used to point to oldptr in body
***		now point to ptr.
***
***
***	    void SNL_Fix_Index_Pointers(WN* loop, WN* wn);
***
***		The def lists for symbols that are the index are made to
***		point there.
***
***	    void SNL_Print_Ldid_Pointers(WN* wn);
***
***		Print all def_list pointers in wn in a nice way.
***
***	    const DEF_LIST* Find_Def_List_In_Exp(WN* exp, const SYMBOL& sym)
***
***		Return any def_list for this symbol in this expression.
***
***
***	    WN* Find_Use_In_Exp(WN* exp, const SYMBOL& sym)
***
***		Return use for this symbol in this expression.
***
***	    WN* SNL_Copy_Exp(WN* tree)
***
***		Calls LWN_Copy_Tree, copying alias information and du
***		information and dg information.
***
***	    WN*& SNL_UBexp(WN* snl_end, BOOL* ne = NULL)
***	    WN*& SNL_UBvar(WN* snl_end)
***
***		Given a doo loop, pass WN_end(doloop) as a parameter.
***		The function then returns kid1 (kid0 for UBvar) when snl_end's
***		opr is LE/LT.  If it's GE/GT, return kid0 (kid1).
***             Otherwise error.  If it's GT or LT, then ne is set to TRUE,
***             if it's passed.  Otherwise FALSE.
***
***         WN* Good_Do_Next_Innermost(WN* doloop)
***
***             Go in from this doloop.  Return the unique do loop inside.
***             Return NULL if none or more than one.
*** 
***	    BOOL SNL_Is_Non_Varying_Access_Array(ACCESS_ARRAY* aa,
***                                              INT outer_depth)
*** 
***		Returns TRUE if the access array 'aa' is free of problems
***		that make it non-transformable within the loop nest starting   
***		at 'outer_depth', FALSE otherwise.
***	
***	    WN* SNL_Get_Inner_Snl_Loop(WN* outer, INT nloops)
***
***		Returns the innermost loop in the SNL of 'nloops' loops,
*** 		where 'outer' is the outermost loop in the SNL. 
***
***         BOOL SNL_Is_Invariant(DOLOOP_STACK *stack,
***                        INT d,
***                        INT dd)
***
***		Returns TRUE if the the 'dd'th loop in the 'stack' is free
***		of references to the 'd'th loop in the 'stack'.
***		Access vectors are used if they are valid, otherwise, we use
***		UD information.
***	  
***	     INT SNL_Loop_Count(WN* wn_snl)
***
***		Returns the number of loops in the innermost SNL 'wn_snl'.
***
***	     WN* SNL_Innermost_Do(WN* wn_outer)
***
***		Returns the innermost loop in the innermost SNL 'wn_outer'. 
***
***	     INT Is_Inner_SNL(WN* wn_loop)
***
***	        Returns the number of loops in the inner SNL rooted at 
***		'wn_loop', if 'wn_loop' is indeed an inner SNL, otherwise 
***		returns 0.
***
***	     void SNL_Upper_Bound_Standardize(WN* wn_outer, nloops)
***
***		For the SNL wth outermost loop 'wn_outer' consisting of
***		'nloops' loops, attempt to standardize the upper bounds of 
***		each loop in that SNL.
***
***	     BOOL Valid_SNL_Region(SNL_REGION region)
***
***		Returns TRUE if 'region' is a valid SNL region, FALSE 
***		otherwise.
***
***	     BOOL Need_Fix_Array_Deps_On_Index_Variable(WN* wn_loop)
***
***		Returns TRUE if the index variable for loop 'wn_loop' is
***		aliased with an array element, FALSE otherwise.
*** 
***	     BOOL SNL_Fix_Array_Deps_On_Index_Variable(WN* wn_outer, INT nloops)
***
***		Fix all of the index variables in the SNL 'wn_loop' of 
***		'nloops' loops which are aliased to array elements, insofar 
***		as this is possible.  Return FALSE if we ran out of depen-
***		dence edges while attempting this, TRUE otherwise.
***
***	     WN* Next_SNL_Loop(WN* wn_outer)
***
***		Returns the next innermost loop in the SNL 'wn_outer', NULL
***		if there is none.
***
***	     IMAT* Permutation_To_Unimodular(INT permutation[], INT nloops)
***
***		Returns a unimodular matrix equivalent to the 'permutation'
***		of length 'nloops'.
***
***	     INT* Unimodular_To_Permutation(IMAT* unimodular)
***
***		Returns a permutation equivalent to the 'unimodular' matrix,
***		if it is actually a permutation matrix.  Otherwise returns 
***		NULL.
***
***	     void Fix_Do_Du_Info(WN* wn, SNL_TRANS_INDEX_DATA* td,
***		BOOL recursive, WN* loops, INT  only_in_nloops)
***	        
***            wn: The code we are fixing up.
***            td: Pass in NULL to not fix up DU information for non-index
***                variables in the bounds.  Otherwise pass in the td.
***            recursive:If true, fix up information inside the do body as 
***		   well, and recursively inside that.  Otherwise, just operate 
***		   on the bounds of this wn.
***            loops: If NULL, fix up all index variables that appear within
***                WN.  E.g. if inside wn there is a use of "i", and there is
***                some ancestor that is a DO "i", tnen make i's DU information
***                point to that only.  If not null, then it's not the ances-
***		   tors of the reference, but loops and its ancestors.  This 
***		   is useful when wn is not part of the PU but just some where 
***		   we created, but more typically, just pass in NULL.
***            only_in_nloops: How deep may we assume that the index variable 
***		   is only read and written within the loop.  E.g. 0 means 
***		   never assume that.  1 means assume that for wn only.  2 
***		   means assume that for wn and every loop nested 1 inside 
***		   wn ... .  Notice that if this value is non-zero, then this 
***		   code will delete all DU information for each loop it sees, 
***		   and then expect to see every reference to that index 
***		   variable and rebuild the information.  If a DO with uses 
***		   of the index only inside is being renamed, then you pass 
***		   in only_in_loops=1 so that you remove any stale DU infor-
***		   mation and recompute it exactly.
***
***	     INT snl_debug
***
***		    0: print little no debugging information
***		    1: print some basic debugging informations
***		    2: print lots of debugging information
***	    	    3: print yet more debugging information
***
*** Exported Functions for Debugging:
***
***	void SNL_DEBUG0(level, string);
***	void SNL_DEBUG1(level, string, arg1);
***	void SNL_DEBUG2(level, string, arg1, arg2);
***	void SNL_DEBUG3(level, string, arg1, arg2, arg3);
***
***	    if snl_debug >= level, then print an SNL debugging
***	    message to TFILE, unless, and also to TFILE if it exists.
***	    The string is an fprintf string,
***	    and any args following are args to fprintf.
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef snl_utils_INCLUDED
#define snl_utils_INCLUDED "snl_utils.h"

#ifdef _KEEP_RCS_ID
static char *snl_utils_rcs_id = snl_utils_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef mat_INCLUDED
#include "mat.h"
#endif

#ifndef snl_xbounds_INCLUDED
#include "snl_xbounds.h"
#endif

//-----------------------------------------------------------------------
// TODO FIX Compilation hacks
//-----------------------------------------------------------------------

extern BOOL Is_Lexpos(DEPV*, INT dims);
extern BOOL Is_Lexpos(DEPV_ARRAY*);
extern MEM_POOL SNL_local_pool;

//-----------------------------------------------------------------------
// end compilation hacks
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------

#define UCTILE_T  0x1
#define UCTILE_I  0x2
#define UCTILE_O  0x4

struct SNL_REGION {
  WN*	First;
  WN*	Last;
  SNL_REGION(WN* f, WN* l) : First(f), Last(l) {}
  SNL_REGION() : First(NULL), Last(NULL) {}
  BOOL operator == (SNL_REGION r) const
    {return First == r.First && Last == r.Last;}
  BOOL operator != (SNL_REGION r) const
    {return First != r.First || Last != r.Last;}
};

enum SNL_INV_CACHE_BLOCK_REASON {
  SNL_INV_UNDEFINED,
  SNL_INV_TILE_ONLY,
  SNL_INV_SE_ONLY,
  SNL_INV_TILE_SE,
  SNL_INV_LEGO_TILE,
  SNL_INV_MP_TILE,
  SNL_INV_DOACROSS_TILE
};

#define SNL_MAX_LOOPS LNO_MAX_DO_LOOP_DEPTH

class SNL_TILE_INFO {

 public:

  SNL_TILE_INFO(WN** loop, const IMAT& L,
                const INT* striplevel, MEM_POOL* pool);   // unimplemented

  SNL_TILE_INFO(INT nloops, INT strips,
                const INT* iloop, const INT* stripsz, const INT* striplevel,
                SNL_INV_CACHE_BLOCK_REASON reason[], MEM_POOL* pool);

  ~SNL_TILE_INFO() {}

  MEM_POOL*     Pool() const {return _pool;}
  BOOL          Rectangular() const {return _rectangular;}
  void          Print(FILE*) const;
  const IMAT&   L() const {return _l;}
  const IMAT&   KHT() const {return _kht;}
  INT           K() const {return _k;}
  INT           Striplevel(INT i) const {return _striplevel[i];}

  // these are only interesting if Rectangular() is TRUE.

  INT           Iloop(INT i) const {return _iloop[i];}
  INT           Stripsz(INT i) const {return _stripsz[i];}
  INT           Nloops() const {return _l.Rows();}
  INT           Strips() const {return _l.Cols();}
  SNL_INV_CACHE_BLOCK_REASON Reason(INT i) const {return _reason[i];}

 private:

  // these are undefined

  SNL_TILE_INFO();
  SNL_TILE_INFO& operator = (SNL_TILE_INFO&);
  SNL_TILE_INFO(const SNL_TILE_INFO&);

  MEM_POOL*     _pool;
  BOOL          _rectangular;

  IMAT          _l;
  IMAT          _kht;
  INT           _k;
  INT           _striplevel[SNL_MAX_LOOPS];

  // only interesting when _rectangular is true

  INT           _iloop[SNL_MAX_LOOPS];
  INT           _stripsz[SNL_MAX_LOOPS];
  SNL_INV_CACHE_BLOCK_REASON _reason[SNL_MAX_LOOPS];
};


// The SNL_TRANS_INDEX_DATA holds information about the loop index variables
// before and after transformation.  It assumes optional unimodular transf
// followed by optional tiling transf.  It's in a nice simple array format.
// It is meant to augment SNL_BOUNDS_INFO putting the sym_list in a nicer
// format with more data.

class SNL_TRANS_INDEX_DATA {

 public:

  struct TDATA {
    SYMBOL              symbol;         // symbol of the tile loop index
    WN*                 alias_wn;       // NULL initialized but set by
                                        // U_Ctiling before used.
  };
  struct IDATA {
    SYMBOL              pre_symbol;     // symbol of the index, before ut
    WN*                 pre_alias_wn;   // use an ldid of the symbol before
                                        // transformation
    SYMBOL              post_symbol;    // symbol of the index, after ut
    WN*                 post_alias_wn;  // wn with the same alias information
    WN*                 newcode;        // pre_symbol->newcode, unparentized
    INT                 max_used_depth; // depth of deepest index in newcode
    WN*                 lbtest;         // index==lb, in transformed space
    WN*                 ubtest;         // index==ub, in transformed space

  };
  struct ODATA {
    SYMBOL              symbol;         // symbol of the symbolic constant
    WN*                 alias_wn;       // a fake ldid with with correct
                                        // alias and DU information
  };

 public:

  SNL_TRANS_INDEX_DATA(const IMAT* u, const IMAT* uinv,
                       const IMAT* kht, const SNL_BOUNDS_INFO* bi,
                       DOLOOP_STACK* stack, INT first_in_stack,
                       SNL_TILE_INFO* ti, MEM_POOL* p);
  ~SNL_TRANS_INDEX_DATA();

 public:

  INT           t_nloops;       // number of tile loops
  INT           i_nloops;       // original loop nest depth
  INT           o_nloops;       // symbolic variable count

  TDATA*        tdata;          // tile data
  IDATA*        idata;          // index data
  ODATA*        odata;          // other symbolic variable data

  MEM_POOL*     pool;           // from where this data structure was allocated

  void          Print(FILE*) const;
};

extern void SNL_Sanity_Check_Func(WN* wn);

#ifdef Is_True_On
extern void SNL_Sanity_Check_Block(WN* wn);
extern void SNL_Sanity_Check_Loop(WN* wn);
extern void SNL_Sanity_Check_If(WN* wn);
extern void SNL_Sanity_Check_Region(SNL_REGION region);
extern WN*  SNL_Sanity_Check_Exp(WN* wn);
#endif

//-----------------------------------------------------------------------
// SNL-specific stuff
//-----------------------------------------------------------------------
enum SNL_MONO {
  SNL_MONO_INVARIANT,
  SNL_MONO_INC,
  SNL_MONO_DEC,
  SNL_MONO_OTHER
};

extern SNL_MONO	Mono(WN* wn, SYMBOL symbol, BOOL neg =FALSE);
void		Print_Do_Stack(FILE* f, const DOLOOP_STACK *);

//------------------------------------------------------------------------
// External Functions
//------------------------------------------------------------------------

class DEF_LIST;

extern void Increase_By(WN* wn, INT c, WN* parent =NULL, INT kid = -1);

extern WN* LWN_Create_Block_From_Stmts_Above(WN* wn);
extern WN* LWN_Create_Block_From_Stmts_Below(WN* wn);
extern INT64	Iterations(WN* loop, MEM_POOL* pool); 
extern WN* Find_Next_Innermost_Do(WN* loop);
extern DOLOOP_STACK* Copy_Dostack(const DOLOOP_STACK& stack, MEM_POOL*);

extern MEM_POOL SNL_local_pool;
extern INT Renumber_Loops(WN* first, WN* last, ARRAY_DIRECTED_GRAPH16* =NULL);

extern void Dump_WN(SNL_REGION, FILE*, INT,
		    INT = 2, INT = 2, WN** = NULL, WN* = NULL,
		    ARRAY_DIRECTED_GRAPH16* =NULL);

extern void SNL_Add_Du_To_Index_Ldid(WN* loop, WN* ldid, DU_MANAGER*, BOOL);
extern void SNL_Change_Du_To_Index_Ldid(WN* loop, WN* code, DU_MANAGER*, BOOL);
extern void SNL_Change_Du_Pointer(WN* oldptr, WN* ptr, WN* body, DU_MANAGER*);
extern void SNL_Print_Ldid_Pointers(WN* wn);

const DEF_LIST* Find_Def_List_In_Exp(WN* exp, const SYMBOL& sym);
WN* Find_Use_In_Exp(WN* exp, const SYMBOL& sym);
WN* SNL_Copy_Exp(WN*);
void SNL_Fix_Index_Pointers(WN* loop, WN* wn);

extern WN*& SNL_UBexp(WN* snl_end, BOOL* ne = NULL);
extern WN*& SNL_UBvar(WN* snl_end);
extern WN* Good_Do_Next_Innermost(WN* doloop);
extern void SNL_Optimize_Bounds(SNL_REGION region);

extern BOOL SNL_Is_Non_Varying_Access_Array(ACCESS_ARRAY* aa,
                                            INT outer_depth);

extern WN* SNL_Get_Inner_Snl_Loop(WN* outer, INT nloops);

extern BOOL SNL_Compare_Logic(WN *bound_exp,
                        WN *bound_var,
                        WN *loop,
                        WN *stmt,
                        BOOL is_first);

extern BOOL SNL_Is_Invariant(DOLOOP_STACK *stack,
                        INT d,
                        INT dd);

extern INT SNL_Loop_Count(WN* wn_snl); 

extern WN* SNL_Innermost_Do(WN* wn_outer); 

extern INT Is_Inner_SNL(WN* wn_loop);

extern void SNL_Upper_Bound_Standardize(WN* wn_outer, INT nloops);

extern BOOL Valid_SNL_Region(SNL_REGION region); 

extern BOOL Need_Fix_Array_Deps_On_Index_Variable(WN* wn_loop); 

extern BOOL SNL_Fix_Array_Deps_On_Index_Variable(WN* wn_outer, INT nloops); 

extern WN* Next_SNL_Loop(WN* wn_outer); 

extern IMAT* Permutation_To_Unimodular(INT permutation[], INT nloops);

extern INT* Unimodular_To_Permutation(IMAT* unimodular);

extern WN* generate_tree_from_row(const mINT32* m, SNL_TRANS_INDEX_DATA* td,
  INT64 c, TYPE_ID wtype, INT part);

extern WN* generate_tree_from_bounds_info_row(const mINT32* row,
  mINT64 con, BOOL le, const SNL_BOUNDS_SYMBOL_LIST*vi);

extern void Fix_Do_Du_Info(WN* wn, SNL_TRANS_INDEX_DATA* td,
                           BOOL recursive, WN* loops,
                           INT  only_in_nloops);

extern void SNL_Rebuild_Access_Arrays(WN* wn_outerloop);

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// Debugging utilities
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

#define SNL_DEBUG0(level, string)			\
    ((snl_debug < level) ? 0 :			\
	 (fprintf(TFile, "SNL DEBUG: "),\
	  fprintf(TFile, string),\
	  fprintf(TFile, "\n")))

#define SNL_DEBUG1(level, string, v1)			\
    ((snl_debug < level) ? 0 :			\
	 (fprintf(TFile, "SNL DEBUG: "),\
	  fprintf(TFile, string, v1),\
	  fprintf(TFile, "\n")))

#define SNL_DEBUG2(level, string, v1, v2)		\
    ((snl_debug < level) ? 0 :			\
	 (fprintf(TFile, "SNL DEBUG: "),\
	  fprintf(TFile, string, v1, v2),\
	  fprintf(TFile, "\n")))

#define SNL_DEBUG3(level, string, v1, v2, v3)		\
    ((snl_debug < level) ? 0 :			\
	 (fprintf(TFile, "SNL DEBUG: "),\
	  fprintf(TFile, string, v1, v2, v3),\
	  fprintf(TFile, "\n")))

#endif

