/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include "tracing.h"
#include "error.h"

#include "mempool_allocator.h" /* for class mempool_allocator */
#include "cxx_memory.h"        /* for CXX_NEW() */

#include "op.h"
#include "bb.h"
#include "bb_set.h"

#include "gtn_universe.h"    /* for GTN_SET_xxx() */
#include "gtn_set.h"
#include "register.h"
#include "gra_live.h"

#include "dominate.h"   
#include "cgtarget.h"   
#include "cg_dep_graph.h" 
#include "cg_loop.h"

#include "gcm_licm.h"
#include "gcm.h"
#include "loop_dce.h"
#include "tag.h"

#include <list>

extern BOOL LOOP_DCE_Skip_Op_Binary_Search( int );

class LOOP_DCE
{
private:
  MEM_POOL *_pool;
  LOOP_DESCR *_loop;

  BB *_epilog;
  BB *_prolog;
  BB *_head;
  BB *_tail;
  BB_SET *_loop_bbs;

  BOOL _unsuitable;     // whether it's good the do DCE for this loop
  BOOL _trace;          //

public:
  LOOP_DCE( LOOP_DESCR *, MEM_POOL * );
  ~LOOP_DCE();

  void Find_Epilog();
  void Find_Prolog();

  void Dead_Code_Elimination();
};

LOOP_DCE::LOOP_DCE( LOOP_DESCR *l, MEM_POOL *p) : 
  _loop(l), _pool(p), 
  _unsuitable(FALSE),
  _epilog(NULL),
  _prolog(NULL),
  _head(NULL),
  _tail(NULL)
{
  _head = LOOP_DESCR_loophead(_loop);
  _loop_bbs = LOOP_DESCR_bbset(_loop);
  
  _trace = Get_Trace (TP_GCM, GCM_TRACE_DCE );

  Find_Prolog();
  Find_Epilog();
}

LOOP_DCE::~LOOP_DCE()
{
}

//================================================================
// LOOP_DCE::Find_Prolog
//
//================================================================
void LOOP_DCE::Find_Prolog()
{
  _prolog = Loop_Is_Zdl( _loop );
  if( _prolog ){
    if( _trace )
      fprintf( TFile, "this is zero delay loop\n" );
    return;
  }else{
    if( _trace )
      fprintf( TFile, "this is NOT zero delay loop\n" );
  }

  // Now we need to find the prolog for NON-ZDL loop
  BBLIST* p;
  FOR_ALL_BB_PREDS( _head, p ){
    BB* pred = BBLIST_item (p);
    if( BB_SET_MemberP( _loop_bbs, pred ) ) 
      continue; 
    if( _prolog ){
      _prolog = NULL; 
      break;
    }else{
      _prolog = pred;
    }
  }

  if( _prolog )
    Set_BB_gra_spill(_prolog);

  if( _prolog && BB_prev(_head) != _prolog ){
    DevWarn( "prolog BB:%d not fall through to BB:%d. Ugly due to cflow",
             BB_id(_prolog), BB_id(_head) );
    DevWarn( "I don't do LICM for it" );
    _unsuitable = TRUE;
    return;
  }else{
    if( !_prolog ){
      if( _trace )
        fprintf( TFile, "Create a new :\n" );
      _prolog = CG_LOOP_Gen_And_Prepend_To_Prolog( _head,_loop );

      Set_BB_dom_set( _prolog, BS_Create_Empty( 2+PU_BB_Count+1, _pool ) );
      BS_UnionD( BB_dom_set(_prolog), BB_dom_set(_head), _pool );
      BS_Union1D( BB_dom_set(_prolog), BB_id(_prolog), _pool );
      BS_Difference1D( BB_dom_set(_prolog), BB_id(_head) );

      Set_BB_pdom_set( _prolog, BS_Create_Empty( 2+PU_BB_Count+1, _pool ) );
      BS_UnionD( BB_pdom_set(_prolog), BB_pdom_set(_head), _pool );
      BS_Union1D( BB_pdom_set(_prolog), BB_id(_prolog), _pool );
    }
    if( _trace ){
      fprintf( TFile, "Prolog is :\n" );
      Print_BB_No_Srclines( _prolog );
    }

    return;
  }
}

//================================================================
// LOOP_DCE::Find_Epilog
//
//================================================================
void LOOP_DCE::Find_Epilog()
{
  // Find the epilog
  BB *bb;
  FOR_ALL_BB_SET_members(_loop_bbs, bb) {
    BBLIST *succs;
    FOR_ALL_BB_SUCCS(bb, succs){
      BB *succ = BBLIST_item(succs);
      if( !BB_SET_MemberP(_loop_bbs, succ) ){
        if( _epilog && succ != _epilog ){
          _epilog  = NULL;
          if( _trace )
            fprintf(TFile, "There are more than one epilogs");
          break;
        }
        _epilog = succ;
        _tail = bb;
      }
    }
  }

  if( _epilog == NULL ){
    if( _trace )
      fprintf( TFile, "cannot find epilog" );
    _unsuitable = TRUE;
  }else{
    if( _trace )
      fprintf( TFile, "epilog is BB:%i\n", BB_id(_epilog) );
  }

  return;
}
  

//================================================================
// LOOP_DCE::Dead_Code_Elimination
//
// This is the main procedure of LOOP_DCE
// The algorithm is not the same as Muchnick's book, which uses UD
// and DU chain. I just maintain some working_list and go through 
// all the loop BBs bottom-up.
//
// I start from the epilog, assuming that before LOOP_DCE the liveness
// has been computed correctly.
//================================================================
static inline BOOL BB_not_processed( BB *bb, std::list<BB*> processed_list )
{
  std::list<BB*>::const_iterator cit = processed_list.begin();
  for( ; cit != processed_list.end(); cit++ ){
    if( bb == *cit )
      return FALSE;
  }
  return TRUE;
}

static inline BOOL OP_cannot_remove( OP *op )
{
  if( OP_xfer(op) || OP_is_loop(op) || OP_volatile(op) ||
      OP_store(op) || OP_side_effects(op) ||
      TOP_is_c2_store(OP_code(op)) ||
      TOP_is_c3_store(OP_code(op)) )
    return TRUE;
  else
    return FALSE;
}

static inline BOOL OP_not_live( OP *op, std::list<OP*> live_ops )
{
  std::list<OP*>::const_iterator cit = live_ops.begin();
  for( ; cit != live_ops.end(); cit++ ){
    if( op == *cit )
      return FALSE;
  } 
  return TRUE;
}

void LOOP_DCE::Dead_Code_Elimination()
{
  TN_SET *live_tns;
  std::list< OP* > live_ops;
  std::list< BB* > processed_bbs;
  std::list< BB* > workinglist;

  if( _unsuitable )
    return;

  if( _trace )
    fprintf( TFile, "==== begin GCM DCE ====\n" );

  Is_True( _epilog, ("no epilog") );

  // NOTE: GTN uses GTN_Universe, a map, to save memory space, since 
  //       GTNs are sparse in the TN world. So I need to transform 
  //       GTN_SET to TN_SET
  GTN_SET *epilog_live_in = BB_live_in(_epilog);  
  live_tns = TN_SET_Create_Empty( Last_TN + 1, _pool );
  for( TN *tn = GTN_SET_Choose(epilog_live_in);
       tn != GTN_SET_CHOOSE_FAILURE;
       tn = GTN_SET_Choose_Next(epilog_live_in,tn))
  {
    FmtAssert(TN_is_global_reg(tn),("TN%d is not global",TN_number(tn)));
    live_tns = TN_SET_Union1D(live_tns, tn, _pool);
  }

  if( _trace ){
    fprintf( TFile, "the bbs in loop are:" );
    BB_SET_Print( _loop_bbs, TFile );

    fprintf( TFile, "the live in TNs of epilog:" );
    TN_SET_Print( live_tns, TFile );
    fprintf( TFile, "\n" );
  }

  
  // There is one situation, for example:
  // BB0: (prolog)
  //     ...
  // BB1: (head)
  //     GTN420 :- add GTN433 ...
  //     br.nez GTN400 BB4 
  // BB2:
  //     ...
  // BB3:
  //     ...
  // BB4: (tail)
  //     GTN433 :- alu xxx, xxx  <--- problematic OP
  //     br.nez GTNxxx BB1
  // BB5: (epilog)
  // 
  // GTN433 is not live_in of BB5, while GTN420 is live_in of BB5. If iterate
  // in reverse top order only once, GTN433 will think as dead code, and the
  // problematic OP will be deleted. This error comes because we haven't check
  // the liveness completely before we do delete, and we add _tail mandantorily
  // and it's succ (the loop head) is not proceesed yet.
  // 
  // The solution is to add a mechanism to ensure that the OPs to be removed
  // is REALLY dead after we compute the liveness of the whole loop, by checking
  // the result TNs.

  BB *bb = NULL;
  TN_SET *old_live_tns;
  BOOL changed = TRUE;
 
  // There are some CFGs that are not suitable to get (Reverse) Topological
  // so we need to ignore this kind of loops.
  BOOL suitable = TRUE; 
  
  while( changed && suitable ){
    old_live_tns = TN_SET_Copy( live_tns, _pool );
    workinglist.push_back( _tail );
    processed_bbs.clear();

    while( !workinglist.empty() ){
      // get the first BB in working list
      bb = workinglist.front();
      workinglist.pop_front();
      if( _trace ){
        fprintf( TFile, "..DCEing BB:%i\n", BB_id(bb) );
      }
  
      // need to put bb to processed_bbs before we add its preds, since there
      // there may be single-BB loop.
      processed_bbs.push_back(bb);
  
      // add all its pred to working list, if possible
      // NOTE: The BBs added to working list must be in Reverse Topoplogical
      //       order.
      BBLIST *pred_list;
      FOR_ALL_BB_PREDS( bb, pred_list ){
        BB *pred;
        pred = BBLIST_item( pred_list );
        if( BB_SET_MemberP(_loop_bbs, pred) &&
            BB_not_processed(pred, processed_bbs) ){
          // Decide if all pred's succs are processed
          // (1) if itself is a succ, then we ignore it.
          // (2) if the edge from pred to its succ is back edge, then ignore it
          BOOL all_succs_done = TRUE;
          BBLIST *succ_list;
          FOR_ALL_BB_SUCCS( pred, succ_list ){
            BB *s = BBLIST_item(succ_list);
            if( s != pred &&
                BB_SET_MemberP(_loop_bbs, s) &&
                BB_not_processed(s, processed_bbs) &&
                ( s != BB_loop_head_bb(pred) ) ){
              all_succs_done = FALSE;
              break;
            } 
          }
          if( all_succs_done )
            workinglist.push_back( pred );
        }
      }
  
      // iterate all the OPs in bb, from bottom to up, to see which
      // OP can be removed. 
      OP *op;
      FOR_ALL_BB_OPs_REV(bb, op){
        BOOL removable = TRUE;
        int i = 0;
        for( i=0; i < OP_results(op); i++ ){
          TN *tn = OP_result(op, i);
          if( TN_is_label(tn) || TN_is_constant(tn) )
            continue;
          if( TN_SET_MemberP(live_tns, tn) || TN_is_dedicated(tn) ){
            removable = FALSE;
            break;
          }
        }
        // add op to live_ops, and its opnd TN to live_tns
        if( !removable || OP_cannot_remove(op) ){
          live_ops.push_back(op);
          for( i=0 ; i < OP_opnds(op); i++ ){
            TN *tn = OP_opnd(op, i);
            if( TN_is_label(tn) || TN_is_constant(tn) )
              continue;
            live_tns = TN_SET_Union1D( live_tns, tn, _pool );
          } 
        } 
      }
    } // while( !workinglist.empty() )
    changed = !TN_SET_EqualP( old_live_tns, live_tns );
    suitable = ( BB_SET_Size(_loop_bbs) == processed_bbs.size() );
  } // while( !changed )

  if( !suitable ){
    fprintf( TFile, "this loop is not suitable for DCE, since it cannot be done with reverse topological sorting\n" );
    fprintf( TFile, "==== end GCM DCE ====\n" );
    return;
  }

  // Remove OPs which are not in live_ops
  bb = NULL;
  INT32 del_num = 0;
  FOR_ALL_BB_SET_members( _loop_bbs, bb ){
    if( _trace ){
      fprintf( TFile, "--before delete OPs--\n" );
      Print_BB_No_Srclines(bb);
    }

    if( _trace )
      fprintf( TFile, "delete following OP from BB:%i", BB_id(bb) );

    OP *op = NULL;
    FOR_ALL_BB_OPs_REV(bb, op){
      if( OP_not_live(op, live_ops) ){
        if( !LOOP_DCE_Skip_Op_Binary_Search(del_num) ){
          if( _trace )
            Print_OP_No_SrcLine(op);
          BB_Remove_Op(bb, op);
          del_num++;
        }
      }
    }    

    if( _trace ){
      fprintf( TFile, "--after delete OPs--\n" );
      Print_BB_No_Srclines(bb);
    }
  }

  if( _trace ){
    fprintf( TFile, " Total delete OPs: %i\n", del_num );
    fprintf( TFile, "==== end GCM DCE ====\n" );
  }
}

//================================================================
// LOOP_DCE::Dead_Code_Elimination
//
// This is the external interface of GCM DCE
//================================================================
void GCM_Dead_Code_Elimination( LOOP_DESCR *loop, MEM_POOL *pool )
{
  LOOP_DCE loop_dce(loop, pool);
  loop_dce.Dead_Code_Elimination();
}
