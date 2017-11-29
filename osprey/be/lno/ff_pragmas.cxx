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


//-*-c++-*-

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "defs.h"
#include "wn.h"
#include "wn_pragmas.h"
#include "cxx_memory.h"
#include "lnopt_main.h"
#include "lwn_util.h"
#include "fission.h"
#include "fusion.h"
#include "errors.h"
#include "erbe.h"
#include "erglob.h"
#include "lnoutils.h"

static MEM_POOL FF_PRAGMA_default_pool;

// BOOL FF_Pragma_Seen_Before(WN* pragma)
//
// If the pragma id and line of this WN has been used as a parameter to
// this routine *ever* in this compilation (e.g. in this or another PU),
// then return TRUE.  Otherwise, return FALSE but store in a database
// this pragma id and line pair, so that the next time this routine is
// called with that pair, it can return TRUE.
// 
// Purpose: Pragmas that apply to a global scope get replicated, one per PU.
// For example, suppose we have C$ UNROLL at the global scope.  Then
// each PU will have one of these.  If we wish to warn that we are ignoring
// this pragma, say, then we will print such a warning the first time we
// see it, but don't want to print it subsequent times.

class FF_PRAGMA_WARNING_INFO {
  mUINT64          _entry;
 public:
  operator UINT64() {return _entry;}      // so HASH_TABLE hashing works
  FF_PRAGMA_WARNING_INFO(INT32 line, WN_PRAGMA_ID pragma) :
    _entry((UINT64(line) << 32) | UINT64(pragma&0xFFFFFFFF)) {}
  INT32    Line() const {return _entry >> 32;}
};
typedef HASH_TABLE<FF_PRAGMA_WARNING_INFO,BOOL> FF_PRAGMA_WARNING_TABLE;

BOOL FF_Pragma_Seen_Before(WN* wn)
{
  INT32 line = WN_Get_Linenum(wn);

  if (line == 0)
    return FALSE;

  static FF_PRAGMA_WARNING_TABLE* FF_Pragma_Warning_Table = NULL;
  WN_PRAGMA_ID                    pragma = (WN_PRAGMA_ID)WN_pragma(wn);
  FF_PRAGMA_WARNING_INFO          info(line, pragma);

  if (FF_Pragma_Warning_Table == NULL)
    FF_Pragma_Warning_Table =
      CXX_NEW(FF_PRAGMA_WARNING_TABLE(71, Malloc_Mem_Pool), Malloc_Mem_Pool);

  if (FF_Pragma_Warning_Table->Find(info))
    return TRUE;

  FF_Pragma_Warning_Table->Enter(info,1);
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Pragma_Set_No_Interchange
// FUNCTION: Set 'Cannot_Interchange' on all DO LOOPs in the tree rooted 
//   at 'wn_ref'. 
//-----------------------------------------------------------------------

static void Pragma_Set_No_Interchange(WN* wn_ref)
{
  if (WN_opcode(wn_ref) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_ref); 
    dli->Cannot_Interchange = TRUE; 
    if (dli->Is_Inner)
      return; 
  }
  if (WN_opcode(wn_ref) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_ref); wn != NULL; wn = WN_next(wn)) 
      Pragma_Set_No_Interchange(wn); 
  } else {  
    for (INT i = 0; i < WN_kid_count(wn_ref); i++) 
       Pragma_Set_No_Interchange(WN_kid(wn_ref, i)); 
  }
}

// Scan statements before 'wn' to see if a pragma with 'pragma_id' exists
static BOOL Find_Preceeding_Pragma(WN* wn, WN_PRAGMA_ID pragma_id) {
    WN* prev_pragma=WN_prev(wn);
    while (prev_pragma &&
           (WN_opcode(prev_pragma)==OPC_PRAGMA ||
           WN_opcode(prev_pragma)==OPC_XPRAGMA)) {
      if (WN_pragma(prev_pragma)==pragma_id)
        return TRUE;
      prev_pragma=WN_prev(prev_pragma);
    }
    return FALSE;
}

static WN* Find_Loop_N_Inside(INT n, WN* wn)
{
  INT count=1;
  LWN_ITER* itr = LWN_WALK_SCFIter(wn);
  for ( ; itr; itr = LWN_WALK_SCFNext(itr)) {
    WN*     w = itr->wn;
    if (WN_opcode(w) == OPC_DO_LOOP && count++ == n) {
      LWN_WALK_Abort(itr);
      return w;
    }
  }
  return NULL;
}


static void LWN_Process_FF_Pragmas_Walk_r(WN* wn)
{
  static BOOL ignoring_interchange = FALSE;
  static BOOL ignoring_blockable = FALSE;

  OPCODE      opc=WN_opcode(wn);

  if (opc == OPC_DO_LOOP) {
    ignoring_interchange = FALSE;
    ignoring_blockable = FALSE;
  }

  if (opc==OPC_PRAGMA || opc==OPC_XPRAGMA) {
    BOOL   remove = FALSE;

    WN_PRAGMA_ID pragma_id=(WN_PRAGMA_ID)WN_pragma(wn);
    INT64 pragma_value=WN_const_val(wn);
    INT32 prag_arg1=WN_pragma_arg1(wn);
    INT32 prag_arg2=WN_pragma_arg2(wn);
    DO_LOOP_INFO* dli;
    DO_LOOP_INFO* dli1;

    WN* next_non_prag_stid = WN_next(wn);
    while (next_non_prag_stid) {
      OPCODE op = WN_opcode(next_non_prag_stid);
      if (op != OPC_PRAGMA && op != OPC_XPRAGMA &&
          OPCODE_operator(op) != OPR_STID)
        break;
      next_non_prag_stid = WN_next(next_non_prag_stid);
    }

    switch (pragma_id) {
     case WN_PRAGMA_INLINE_BODY_START:
     case WN_PRAGMA_INLINE_BODY_END: 
      remove = TRUE; 
      break;       

      // code for interchange and blocking almost the same: combine them
     case WN_PRAGMA_BLOCKABLE:
     case WN_PRAGMA_INTERCHANGE:
      remove = TRUE;
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      {
        BOOL ipragma = pragma_id == WN_PRAGMA_INTERCHANGE;
        if ((!ignoring_interchange && ipragma) ||
            (!ignoring_blockable && !ipragma)) {
          INT   interchange_vector[LNO_MAX_DO_LOOP_DEPTH];
          INT   count = 0;
          BOOL  error = FALSE;

          if (ipragma) ignoring_interchange = TRUE;
          else         ignoring_blockable = TRUE;

          for (INT i = 0; i < LNO_MAX_DO_LOOP_DEPTH; i++)
            interchange_vector[i] = -1;

          WN* p = 0;
          for (p = wn;
               WN_opcode(p) == OPC_PRAGMA || WN_opcode(p) == OPC_XPRAGMA;
               p = WN_next(p)) {
            WN_PRAGMA_ID pragma_id = (WN_PRAGMA_ID)WN_pragma(p);

            if ((ipragma && (pragma_id == WN_PRAGMA_INTERCHANGE)) ||
                (!ipragma && (pragma_id == WN_PRAGMA_BLOCKABLE))) {
	        WN* sdo = Find_Loop_N_Inside(WN_pragma_arg1(p),
	       					next_non_prag_stid);
	      if (sdo) {
	        INT sdo_no = Do_Depth(sdo) - Do_Depth(next_non_prag_stid);
	        interchange_vector[count++] = sdo_no;
	      } else {
	        ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(p),
	                    WN_pragmas[pragma_id].name,"loop must follow");
	        error = TRUE;
	        break;
	      }
	    }
          }
          if (error == FALSE) {
            if (!Is_Permutation_Vector(interchange_vector, count)) {
              ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(p),
                           WN_pragmas[pragma_id].name,
                           "incomplete index specification");
            }
            else {
              dli = Get_Do_Loop_Info(next_non_prag_stid);
              if (ipragma) {
		dli->Permutation_Spec_Count = count; 
                dli->Permutation_Spec_Array= CXX_NEW_ARRAY(INT, count,
                                                               dli->Pool());
                for (INT i = 0; i < count; i++)
                  dli->Permutation_Spec_Array[i] = interchange_vector[i];
              }
              else
                dli->Blockable_Specification = count;
            }
          }
        }
      }
      break;
      
     case WN_PRAGMA_NO_INTERCHANGE:
      remove = TRUE;
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      Pragma_Set_No_Interchange(next_non_prag_stid); 
      break;
      
     case WN_PRAGMA_BLOCKING_SIZE:
      remove = TRUE;
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      dli=Get_Do_Loop_Info(next_non_prag_stid);
      if (prag_arg1 >= -1)
        dli->Required_Blocksize[0] = prag_arg1;
      else if (!FF_Pragma_Seen_Before(wn))
	ErrMsgSrcpos(EC_LNO_Bad_Pragma_Int, WN_Get_Linenum(wn),
		     WN_pragmas[pragma_id].name, prag_arg1);
      if (prag_arg1 >= -1)
        dli->Required_Blocksize[1] = prag_arg2;
      else if (!FF_Pragma_Seen_Before(wn))
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_Int,  WN_Get_Linenum(wn),
                     WN_pragmas[pragma_id].name, prag_arg2);
      break;

     case WN_PRAGMA_NO_BLOCKING:
      remove = TRUE;
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      dli=Get_Do_Loop_Info(next_non_prag_stid);
      dli->Cannot_Block = TRUE;
      break;
      
     case WN_PRAGMA_UNROLL:
      remove = TRUE;
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      if (prag_arg1 > 0)
        Get_Do_Loop_Info(next_non_prag_stid)->Required_Unroll = prag_arg1;
      else if (prag_arg1 == 0)
        ;  // no warning.  the meaning is to use the default unrolling
      else if (!FF_Pragma_Seen_Before(wn))
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_Int, WN_Get_Linenum(wn),
                     WN_pragmas[pragma_id].name, prag_arg1);
      break;
      
     case WN_PRAGMA_AGGRESSIVE_INNER_LOOP_FISSION:
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      remove = TRUE;
      dli=Get_Do_Loop_Info(next_non_prag_stid);
      dli->Aggressive_Inner_Fission=TRUE;
      break;

     case WN_PRAGMA_FISSION:	/* fission the surrounding l loops here	*/
      {

        INT level=prag_arg1;	// TODO: may need to be changed

        remove = TRUE;
        if (Good_Do_Depth(wn)+1 < level) {
	    ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		         WN_pragmas[pragma_id].name,
		         "does not have enough enclosing loops");
          break;
        }
        WN* parent_loop=Enclosing_Do_Loop(wn);

        // see if FISISONABLE pragma exists which eliminates the
        // legality test
        BOOL fissionable=
          Find_Preceeding_Pragma(next_non_prag_stid,WN_PRAGMA_FISSIONABLE);
        FISSION_FUSION_STATUS fission_status=Failed;

        if (fissionable) { // no fission legality check is needed

          MEM_POOL_Push(&FF_PRAGMA_default_pool);

          DYN_ARRAY<FF_STMT_LIST> loops(&FF_PRAGMA_default_pool);
          loops.Newidx();
          loops.Newidx();  // create two stmt lists
	  loops[0].Clear();
	  loops[1].Clear();

          // the first list consists of stmts up to the current pragma
          WN* stmt=WN_first(WN_do_body(parent_loop));
          while (stmt!=wn) {
            loops[0].Append(stmt,&FF_PRAGMA_default_pool);
            stmt=WN_next(stmt);
          }
          loops[0].Append(wn,&FF_PRAGMA_default_pool);
  
          // the second list consists of stmts after the current pragma
          stmt=WN_next(wn);
          while (stmt) {
            loops[1].Append(stmt,&FF_PRAGMA_default_pool);
            stmt=WN_next(stmt);
          }
  
          // separate the loop and update the dependences info
          Separate_And_Update(parent_loop, loops, level);
          fission_status=Succeeded;
  
          MEM_POOL_Pop(&FF_PRAGMA_default_pool);
  
        } else if (WN_prev(wn)!=NULL) {
          // fission legality check is needed
          fission_status=Fission(parent_loop, WN_prev(wn), level);
        }
        dli=Get_Do_Loop_Info(parent_loop);
        dli->No_Fusion=TRUE;	// do not fuse these two fissioned loops
        if (fission_status==Succeeded) {
          dli1=Get_Do_Loop_Info(WN_next(parent_loop));
          dli1->No_Fusion=TRUE;
        }
      }
      break;

     case WN_PRAGMA_FISSIONABLE:  /* fission the surrounding l loops here */
      remove = TRUE;
      break;

     case WN_PRAGMA_FUSE:	  /* fuse the next n loops for l levels	*/

      {
        remove = TRUE;
        UINT number_of_loops=prag_arg1;	// TODO: may have to be changed
        UINT number_of_levels=prag_arg2;

        // see if FUSEABLE pragma exists which implies that no legality
        // check is needed
        BOOL fuseable=
          Find_Preceeding_Pragma(next_non_prag_stid,WN_PRAGMA_FUSEABLE);
  
        WN* first_loop=next_non_prag_stid;
  
        if (number_of_levels==0) {
	  INT max_level=LNO_MAX_DO_LOOP_DEPTH;
	  WN* loop=first_loop;
          for (INT i=0; i<number_of_loops; i++) {
#ifdef KEY
	    if (!loop)
	      break;
#endif
	    if (WN_opcode(loop)!=OPC_DO_LOOP) {
	      max_level=0;
	      break;
	    }
	    INT level=1;
	    WN* inner_loop=loop;
	    while (inner_loop=Get_Only_Loop_Inside(inner_loop,FALSE))
	      level++;
	    if (level<max_level) max_level=level;
	    loop=WN_next(loop);
	  }
	  number_of_levels=max_level;
        }
  
        // the strategy is to look at one level at a time
        // we fuse all 'n' loops at the same level before we dive into
        // nest to fuse 'n' inner loops
  
        for (INT j=0; j<number_of_levels; j++) {
  
          BOOL failed=FALSE;
  
          FmtAssert(WN_opcode(first_loop)==OPC_DO_LOOP,
        ("FUSION pragma has to be followed by loops with sufficient nesting"));
  
          WN* next_first_loop=Get_Only_Loop_Inside(first_loop,FALSE);
          WN* block=LWN_Get_Parent(first_loop);
          WN* stmt=WN_next(first_loop);
  
          for (INT i=1; i<number_of_loops; i++) {
  
            do {
              if (stmt &&
                  (WN_opcode(stmt)==OPC_PRAGMA ||
                   WN_opcode(stmt)==OPC_XPRAGMA)) {
                // move pragmas in between loops to before the first loop
                LWN_Extract_From_Block(wn, block);
                LWN_Insert_Block_Before(wn, block, first_loop);
              } else
                break;
              stmt=WN_next(stmt);
            } while (1);
  
            WN* next_stmt=WN_next(stmt); // remembers next stmt
  
            FmtAssert(WN_opcode(stmt)==OPC_DO_LOOP,
              ("Not enough loops following a FUSION pragma"));
  
            FISSION_FUSION_STATUS status;
            if (failed) {
              // if fusion had failed before reaching this loop, do nothing
            } if (fuseable) {
  
              // if FUSEABLE pragma is set then all we check is if the
              // bounds and steps match
  
              dli=Get_Do_Loop_Info(first_loop);
              dli1=Get_Do_Loop_Info(stmt);
  
              FmtAssert(!dli->LB->Too_Messy,
                ("FUSIONABLE pragma requires lower bounds to be simple"));
  
              FmtAssert(!dli->UB->Too_Messy,
                ("FUSIONABLE pragma requires upper bounds to be simple"));
  
              FmtAssert(dli->LB==dli1->LB,
                ("FUSIONABLE pragma requires lower bounds to be the same"));
  
              FmtAssert(dli->UB==dli1->UB,
                ("FUSIONABLE pragma requires upper bounds to be the same"));
  
              FmtAssert(dli->Step==dli1->Step,
                ("FUSIONABLE pragma requires steps to be the same"));
  
              // ask fusion routine to fuse two loops with no peeling
              // and no alignment, therefore no prolog and epilog
              UINT64 prolog=0;
              UINT64 epilog=0;
              WN* epilog_loop=NULL;
              mINT32 offset[1];
              offset[0]=0;
              status=Fuse(first_loop, stmt, 1, 0, TRUE,
                       &prolog, &epilog, &epilog_loop, offset);
              if (status!=Succeeded && 
		  status!=Succeeded_and_Inner_Loop_Removed)
                failed=TRUE;

            } else {
              status=Fuse(first_loop, stmt, 1, LNO_Fusion_Peeling_Limit, TRUE);
              if (status!=Succeeded && 
		  status!=Succeeded_and_Inner_Loop_Removed)
                failed=TRUE;
            }
            stmt=next_stmt;
          }
          //if (failed)
            //break;
          //else {
            // successfully fused 'n' loops at this level
            // mark the fused loop un-fissionable
            ((DO_LOOP_INFO*)Get_Do_Loop_Info(first_loop))->No_Fission=TRUE;
            first_loop=next_first_loop;
          //}
        }

      }
      break;
      
     case WN_PRAGMA_FUSEABLE:	/* fuse the next n loops for l levels	*/
      remove = TRUE;
      break;
      
     case WN_PRAGMA_NO_FISSION:	/* do not fission the next n loops	*/
      remove = TRUE;
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      dli=Get_Do_Loop_Info(next_non_prag_stid);
      dli->No_Fission=TRUE;
      break;
      
     case WN_PRAGMA_NO_FUSION:	/* do not fuse the next n loops		*/
      remove = TRUE;
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      dli=Get_Do_Loop_Info(next_non_prag_stid);
      dli->No_Fusion=TRUE;
      break;

     case WN_PRAGMA_NEXT_SCALAR: 
      remove = TRUE; 
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      dli = Get_Do_Loop_Info(next_non_prag_stid); 
      dli->Pragma_Cannot_Concurrentize = TRUE; 
      break; 

     case WN_PRAGMA_KAP_ASSERT_DO: 
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      if (WN_pragma_arg1(wn) == ASSERT_DO_CONCURRENT) { 
        remove = TRUE; 
        dli = Get_Do_Loop_Info(next_non_prag_stid);
        dli->Pragma_Prefer_Concurrentize = TRUE; 
      } else if (WN_pragma_arg1(wn) == ASSERT_DO_SERIAL) {
	remove = TRUE; 
	dli = Get_Do_Loop_Info(next_non_prag_stid); 
	dli->Pragma_Cannot_Concurrentize = TRUE; 
      } 
      break; 

      case WN_PRAGMA_KAP_ASSERT_DOPREFER: 
      if (next_non_prag_stid == NULL ||
          WN_opcode(next_non_prag_stid) != OPC_DO_LOOP) {
	if (!FF_Pragma_Seen_Before(wn))
	  ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(wn),
		       WN_pragmas[pragma_id].name,
		       "not followed by a loop, ignored");
	break;
      }
      if (WN_pragma_arg1(wn) == ASSERT_DO_CONCURRENT) { 
        remove = TRUE; 
        dli = Get_Do_Loop_Info(next_non_prag_stid);
        dli->Pragma_Prefer_Concurrentize = TRUE; 
      } else if (WN_pragma_arg1(wn) == ASSERT_DO_SERIAL) { 
	remove = TRUE;
        dli = Get_Do_Loop_Info(next_non_prag_stid);
        dli->Pragma_Cannot_Concurrentize = TRUE;
      } 
      break;

    }

    if (remove) {
      LWN_Delete_From_Block(LWN_Get_Parent(wn), wn);
    }
    
    return;
  }

  WN* kid;
  WN* nkid = NULL;

  if (opc==OPC_BLOCK) {
    for (kid=WN_first(wn); kid; ) {
      WN* this_kid = kid;
      kid = WN_next(kid);   // so that the walk routine can remove it
      LWN_Process_FF_Pragmas_Walk_r(this_kid);
    }
    return;
  }

  for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid=WN_kid(wn,kidno);
    if (!OPCODE_is_expression(WN_opcode(kid)))
      LWN_Process_FF_Pragmas_Walk_r(kid);
  }
}


extern void LWN_Process_FF_Pragmas(WN* func_nd) {

  MEM_POOL_Initialize(&FF_PRAGMA_default_pool,"FF_PRAGMA_default_pool",FALSE);

  LWN_Process_FF_Pragmas_Walk_r(func_nd);
  
  MEM_POOL_Delete(&FF_PRAGMA_default_pool);
}


extern void  LNO_Insert_Pragmas(WN* wn)
{
  OPCODE  opc = WN_opcode(wn);
  
  if (opc == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (dli->Is_Inner) {
      if (dli->Required_Unroll > 0) {
        WN* pragma = WN_CreatePragma(WN_PRAGMA_UNROLL, (ST_IDX) NULL,
	                             dli->Required_Unroll, 0);
        WN_set_pragma_compiler_generated(pragma);
	LWN_Insert_Block_Before(LWN_Get_Parent(wn), wn, pragma);
      }
      return;				// no need to go inside
    }
  }

  if (opc==OPC_BLOCK) {
    for (WN* kid = WN_first(wn); kid; kid = WN_next(kid)) {
      LNO_Insert_Pragmas(kid);
    }
    return;
  }

  for (UINT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    WN* kid=WN_kid(wn,kidno);
    if (!OPCODE_is_expression(WN_opcode(kid)))
      LNO_Insert_Pragmas(kid);
  }
}
