/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
*** These routines examine ifs and dos controlling this statement to see
*** whether it is executed.  
***
*** COND_If(WN*) takes a WHIRL IF node as input and returns if the THEN
*** part must be exectuted, the ELSE part, or it could go either way.
*** COND_Do(WN*) takes a WHIRL DO LOOP node as input and returns whether
*** the body is definitely executed at least once, definitely not executed,
*** or not sure.  Note that these return values are under the assumption
*** that the IF or DO themselves are executed.
***
*** Eliminate_Dead_SCF that eliminates
*** all IFs and DOs (and their descendant WNs) that can't be executed.
*** While this routine could be writen given the above primatives, we
*** instead include it here for efficiency reasons.  We don't want to gather
*** parent constrains too many times.
***
*** Guard_Dos places a guard "IF_THEN" in front of every DO_LOOP that
*** we can not prove executes
***
*** Guard_Dos also sets up the OPC_LOOP_INFO for the do loop
***
*** Reserved Prefix:
***
***	COND_
***
*** Exported Types
***
***	COND_IF_INFO
***
***	    COND_IF_THEN_ONLY, COND_IF_ELSE_ONLY, COND_IF_NOT_SURE
***
***	COND_DO_INFO
***
***	    COND_DO_AT_LEAST_ONCE, COND_DO_NEVER, COND_DO_MAYBE
***
*** Exported Functions:
***
***	COND_IF_INFO COND_If_Info(WN* wn_if, MEM_POOL*)
***	COND_DO_INFO COND_Do_Info(WN* wn_do, MEM_POOL*)
***
***	    Get the information for this do or if loop.  The mem_pool
***	    is just for temporary storage, defaulting to LNO_local_pool.
***
***	void COND_Test(WN*)
***
***	    Recursively print information about the DOs and IFs inside.
***
***     BOOL Eliminate_Dead_SCF(WN *wn, void Remove_Region(WN *)) 
***
***	   Call Remove_Region on OPC_BLOCKs descendended from 
***	   wn that can't be executed.  We require passing the function
***	   Remove_Region rather than providing one so that this routine
***	   can be called at different phases of LNO.  If called early,
***	   Remove_Region should just delete WNs.  If called later, it might
***        also have to remove things from the dependence graph, for example.
***	   Return TRUE if something was eliminated.
***
***	void Guard_Dos(WN *func_nd)
***
***	   Guard the Do loop in this subtree to make sure they
***	   are not zero-trip count.  Since
***
***	BOOL Is_Consistent_Condition(ACCESS_VECTOR *av, WN *expr)
***
***	   Given a constraint access vector, where all the symbols
***	   in the access_vector appear inside expr, is it possible
***	   for av to be consistent right before the statement enclosing expr
***
***     BOOL Hoist_Conditionals(WN *func_nd)
***
***	   Given code such as
***	   do ...
***	     if ...
***
***	   If the 'if' is loop invariant, interchange the if and do and move
***        the if statement as high as possible.
***	   Return TRUE if something was hoisted.
***
***	WN* Guard_A_Do(WN *do_wn)
***
***	  Create and return a guard for the do loop 'do_wn'.  
***
***	BOOL Redundant_Condition(COND_BOUNDS_INFO* info, WN* wn_cond,
***       WN* wn_if)
***
***       Accumulate information about the 'wn_cond' in 'info' and return
***       TRUE if 'wn_cond' is redundant with respect to the condition
***       already present under 'wn_if'.
***
***	void Update_Guarded_Do_FB(WN *if_wn, WN *do_wn, FEEDBACK *feedback)
***
***	  Update feedback to reflect the fact that if_wn guards do_wn.
**/

/** $Revision: 1.6 $
*** $Date: 05/05/04 09:52:28-07:00 $
*** $Author: gautam@eng-27.pathscale.com $
*** $Source: ../../be/lno/SCCS/s.cond.h $
**/

#ifndef _defs_INCLUDED
#include "defs.h"
#endif

#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif

#ifndef soe_INCLUDED
#include "soe.h"
#endif

#ifndef fb_whirl_INCLUDED
#include "fb_whirl.h"
#endif

#ifndef cond_INCLUDED
#define cond_INCLUDED "cond.h"

class WN;
typedef struct mem_pool MEM_POOL;

#ifdef _KEEP_RCS_ID
static char *cond_rcs_id = cond_INCLUDED "$Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

enum COND_IF_INFO {COND_IF_THEN_ONLY, COND_IF_ELSE_ONLY, COND_IF_NOT_SURE};
enum COND_DO_INFO {COND_DO_AT_LEAST_ONCE, COND_DO_NEVER, COND_DO_MAYBE};

extern COND_IF_INFO COND_If_Info(WN* wn_if, MEM_POOL* =0);
extern COND_DO_INFO COND_Do_Info(WN* wn_do, MEM_POOL* = 0);

extern void COND_Test(WN*);
extern BOOL Eliminate_Dead_SCF(WN *wn, void Remove_Region(WN *));
void Guard_Dos(WN *func_nd);
BOOL Hoist_Conditionals(WN *func_nd);
BOOL Is_Consistent_Condition(ACCESS_VECTOR *av, WN *expr);
extern WN* Guard_A_Do(WN *do_wn); 
extern void Canonicalize_Unsigned_Loops(WN* func_nd);
extern void Update_Guarded_Do_FB(WN *if_wn, WN *do_wn, FEEDBACK *feedback);
#ifdef KEY
extern BOOL Loop_Unswitch_SCF(WN *wn);
#endif

/**
*** The data structure used here (no longer private to cond.cxx) is 
*** A COND_BOUNDS_INFO.  It is nothing more than a system of equations
*** representing the loop bounds and further outer if conditions, and
*** a stack of symbols indicating which column in the system of equations
*** corresponds to which variable.  Associate with each symbol is the
*** outermost region in which each symbol is known to be invariant.
*** For example, if we have
***
***     if (n > 0)
***        ...
***
*** then when we see the if, we associate the if node with the symbol n.
*** If further in we have a use of n in some conditional expression, and
*** the writes that could alter that n (from the use-def chains) could
*** have happened inside the above if, we know that the n's are different.
*** Collect_Outer_Info works, and must work, from the outside in.  It
*** calls Kill_Written_Symbols.  That code makes sure that all symbols
*** used are from definitions outside of all other conditions that use
*** the same symbol, so that the same values are being reused.  If that
*** condition might not be true, we discard all equations involving that
*** symbol.
**/

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// COND_BOUNDS_INFO
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

class COND_SYMBOL_INFO {

 public:

  SYMBOL        Symbol;
  WN*           Outer_Nondef;
  COND_SYMBOL_INFO(SYMBOL s, WN* o) : Symbol(s), Outer_Nondef(o) {}
  COND_SYMBOL_INFO() : Symbol(), Outer_Nondef(NULL) {}
  COND_SYMBOL_INFO& operator = (const COND_SYMBOL_INFO& a)
    {Symbol = a.Symbol; Outer_Nondef = a.Outer_Nondef; return *this;}
  ~COND_SYMBOL_INFO() {}
  void Print(FILE*) const;
};

class COND_BOUNDS_INFO {

 public:

  COND_BOUNDS_INFO(MEM_POOL*);
  ~COND_BOUNDS_INFO();

  void          Collect_Outer_Info(WN* parent, WN* child = NULL);
  void          Collect_Do_Info(WN* wn_do);
  void          Collect_If_Info(WN* wn_if, BOOL);

  // all information known about the bounds.  This includes the outer
  // loops and conditionals.  All facts.

  SYSTEM_OF_EQUATIONS&          Bounds() {return _bounds;}
  const SYSTEM_OF_EQUATIONS&    Bounds() const {return _bounds;}

  STACK<COND_SYMBOL_INFO>&      Symbol_Info() {return _symbol_info;}
  const STACK<COND_SYMBOL_INFO>&Symbol_Info() const {return _symbol_info;}

  MEM_POOL*                     Pool() const {return _pool;}

  // Given the access array or vector, add the equations expressed.

  INT           Add_Access(ACCESS_VECTOR* av, WN* code, WN* control);
  INT           Add_Access(ACCESS_ARRAY* ai, WN* code, WN* control);

  void          Reset_Bounds_To(INT, INT, INT, DYN_ARRAY<WN*>*);
  void          Print(FILE*) const;

 private:

  void          Reset_Varcount_To(INT cols);


  // Add_Access helper function
  INT           Lookup_Entry(SYMBOL, WN*);
  void          Kill_Written_Symbols(ACCESS_VECTOR*, WN* code, WN* control);

  // undefined
  COND_BOUNDS_INFO();
  COND_BOUNDS_INFO&             operator = (const COND_BOUNDS_INFO&);
  COND_BOUNDS_INFO(const COND_BOUNDS_INFO&);

  SYSTEM_OF_EQUATIONS           _bounds;
  STACK<COND_SYMBOL_INFO>       _symbol_info;
  MEM_POOL*                     _pool;
};

extern BOOL Redundant_Condition(COND_BOUNDS_INFO* info, WN* wn_cond,
  WN* wn_if);

#endif
