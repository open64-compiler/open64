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

//================================================================
// Redundant_Copy_Elimination
//
//.Lt_0_6
//[  20] GTN418 :- c3_mvfacc GTN271($acc0) (0x0) ;
//[  21] GTN419 :- or GTN437 TN1($0) ; copy
//[  22] GTN420 :- or GTN438 TN1($0) ; copy
//[  23] TN426 :- setlt GTN424 GTN422 ;
//[  24] GTN271($acc0) :- c3.mvtacc GTN418 (0x0) ;
//[  25] GTN275($ar0) :- c3.mvtadd GTN419 (0x0) ;
//[  26] GTN276($ar1) :- c3.mvtadd GTN420 (0x0) ;
//[  27] GTN271($acc0) GTN275($ar0) GTN276($ar1) :- c3.mac.a (0x3) GTN275($ar0)<defopnd> (0x2) GTN276($ar1)<defopnd> (0x2) (0x0) GTN271($acc0)<defopnd> ;
//[  28] GTN437 :- c3_mvfadd GTN275($ar0) (0x0) ;
//[  29] GTN438 :- c3_mvfadd GTN276($ar1) (0x0) ;
//[  30] GTN424 :- addu GTN423 GTN424<defopnd> ;
//[  31] :- br.ne TN426 TN1($0) (lab:.Lt_0_6) ;
//
// The motivation is from some C3 instructions, c3_mvfxxx/c3_mvtxxx. In
// the above example, if GTN437/419/275 arent live after the loop, the OPs
// related to it can be removed.
//
// We do this Elimination after DCE, so there are no dead code, we only
// need to find out the copies.
//
// The algorithm I used contains following steps:
// (1) topo traverse of all the BBs, and find out those copy OPs at the 
//     bottom of loop which are not killed. Or in another word, this is
//     to find the downward exposed copy OPs
// (2) reverse topo traverse, to find the upward exposed copy OPs
// (3) build _chains of copies that are in the form like:
//        TN2 <-- TN1
//        ...ops define TN2 ...
//        TN1 <-- TN2
//     the two copies form a chain
// (4) remove the chain
// (5) copy the upward exposed copy OPs to the prolog
// (6) often the copying to prolog are redundant, try to remove them.
//================================================================

// =============================================================
//  class TOPO_ITER
//
//  Iterator used to topologically traverse loop.
//  My strategy is: Resort all the BBs when doing constructor, and then
//                  the requests can be done directly.
//
// The usage of this iter is:
//   TOPO_ITER lit(loop,mempool);
//   BB *bb = lit.begin();
//   for( ; bb != lit.end(); lit++ ){
//     ...
//     bb = *lit;
//   }
// =============================================================
typedef enum{
  TOPO,
  REV_TOPO
}DIRECTION;

class TOPO_ITER {
private:
    MEM_POOL*       _mp;
    DIRECTION       _dir;

    BB_SET*         _visited; /* _visited: BB -> boolean */

    LOOP_DESCR*     _loop;  /* the loop in question */
    BB*             _head;
    BB*             _tail;
    BB*             _prolog;
    BB*             _epilog;

    BB*             _cur;   /* currently visited node */
    std::list<BB*>  _seq;   /* visit sequence */
    BB*             _begin;

    BOOL            _well_formed; // some loop cannot be topo traversed
    BOOL            _sequential;  // if loop has internal branch inside.
public:
    TOPO_ITER (LOOP_DESCR* l, DIRECTION dir, MEM_POOL* mp);
    ~TOPO_ITER (void) {};

    void topo_sort();
    void rev_topo_sort();

    void set_begin() { _cur = _begin; }
    BB* begin() const { return _begin; }
    BB* end() const { return NULL; }

    BB*  operator*(void) const { return _cur; }
    TOPO_ITER& operator++ (void);  // prefix inc
    // TOPO_ITER& operator++ (int);   // postfix inc

    BOOL well_formed(){ return _well_formed; }
    BOOL sequential(){ return _sequential; }
};

TOPO_ITER :: TOPO_ITER (LOOP_DESCR* l, DIRECTION dir, MEM_POOL* mp) 
{
  _mp = mp;
  _dir = dir;
  _loop = l;

  _cur = NULL;
  _seq.clear();

  _visited = BB_SET_Create_Empty (252, _mp);
  _well_formed = TRUE;
  _sequential = TRUE;

  if( _dir == TOPO )
    topo_sort();
  else
    rev_topo_sort();

}

void TOPO_ITER::topo_sort() 
{

  // get the loop tail BB
  BB_SET *body = LOOP_DESCR_bbset(_loop);
  BB *epilog = NULL;
  BB *tail;
  BB *bb;
  FOR_ALL_BB_SET_members(body, bb) {
    BBLIST *succs;
    FOR_ALL_BB_SUCCS(bb, succs){
      BB *succ = BBLIST_item(succs);
      if( !BB_SET_MemberP(body, succ) ){
        if( epilog && succ != epilog ){
          _well_formed = FALSE;
          return;
        }else{
          epilog = succ;
          tail = bb;
        }
      }
    }
  }

  BB* head =  LOOP_DESCR_loophead(_loop);

  std::list<BB*> working_list;
  working_list.push_back(head);
  _seq.push_back(head);
  _visited = BB_SET_Union1D (_visited, head, _mp);

  while( !working_list.empty() ){
    _cur = working_list.front();
    working_list.pop_front();

    BBLIST *s,*p;
    FOR_ALL_BB_SUCCS(_cur, s){
      if( (BBlist_Len(s) > 1) && (_cur != tail) )
        _sequential = FALSE;
        
      BB* succ = BBLIST_item(s);
      if(!BB_SET_MemberP (body,succ)) 
        continue;
      if(BB_SET_MemberP(_visited,succ))
        continue; 
  
      BOOL all_pred_visited = TRUE;
      if(succ != head){
        FOR_ALL_BB_PREDS (succ, p) {
          BB* pred = BBLIST_item (p);
          if( (pred != _cur) && !BB_SET_MemberP(_visited, pred) ) {
            all_pred_visited = FALSE; 
            break;
          }
        }
      }
  
      if(all_pred_visited){ 
        _visited = BB_SET_Union1D (_visited, succ, _mp);
        _seq.push_back(succ); 
        working_list.push_back(succ);
      }
    }
  }

  _well_formed = ( _seq.size() == BB_SET_Size(LOOP_DESCR_bbset(_loop)) );
  
  _cur = *_seq.begin();
  _begin = *_seq.begin();

  return;
}


void TOPO_ITER::rev_topo_sort() 
{
  // get the loop tail BB
  BB *head = LOOP_DESCR_loophead(_loop);
  BB_SET *body = LOOP_DESCR_bbset(_loop);
  BB *epilog = NULL;
  BB *tail;
  BB *bb;
  FOR_ALL_BB_SET_members(body, bb) {
    BBLIST *succs;
    FOR_ALL_BB_SUCCS(bb, succs){
      BB *succ = BBLIST_item(succs);
      if( !BB_SET_MemberP(body, succ) ){
        if( epilog && succ != epilog ){
          epilog = NULL;
          _well_formed = FALSE;
          return;
        }
        epilog = succ;
        tail = bb;
      }
    }
  }

  //put the tail in the _seq at first
  _seq.push_back(tail);

  // add all the other BBs into the _seq, in reverse topo order
  std::list<BB*> working_list;
  working_list.push_back(tail);
  _visited = BB_SET_Union1D (_visited, tail, _mp);

  while( !working_list.empty() ){
    _cur = working_list.front();
    working_list.pop_front();

    BBLIST *s,*p;
    FOR_ALL_BB_PREDS(_cur, s){
      if( (BBlist_Len(s) > 1) && (_cur != head) )
        _sequential = FALSE;

      BB* pred = BBLIST_item(s);
      if(!BB_SET_MemberP (body,pred)) 
        continue;
      if(BB_SET_MemberP(_visited,pred))
        continue; 
  

      BOOL all_succ_visited = TRUE;
      if( pred != tail ){
        FOR_ALL_BB_SUCCS (pred, p) {
          BB* succ = BBLIST_item (p);
          if( (succ != _cur) && !BB_SET_MemberP(_visited, succ)) {
            all_succ_visited = FALSE; 
            break;
          }
        }
      }
  
      if(all_succ_visited){
        _seq.push_back(pred); 
        _visited = BB_SET_Union1D (_visited, pred, _mp);
        working_list.push_back(pred);
      }
    }
  }

  _well_formed = (_seq.size() == BB_SET_Size(body));

  // set the first element
  _cur = *_seq.begin ();
  _begin = *_seq.begin ();

  return;
}

TOPO_ITER & 
TOPO_ITER::operator++()
{
  std::list< BB* >::const_iterator cit = _seq.begin();
  for( ; cit != _seq.end(); cit++ )
    if( _cur == *cit )
      break;

  if( cit == _seq.end() )
    Is_True( 0, ("no post++ allowed for end()") );
  
  cit++;
  if( cit == _seq.end() ){
    _cur = NULL;
  }else{
    _cur = *cit;
  }

  return *this;
}

// =============================================================
//  class LOOP_RCE
//
//  The main class of DCE
// =============================================================
typedef struct {
  TN *dest;
  TN *src;
  OP *op; 
  BOOL up_copy; // whether its an up exposed copy
}CopyRec;

class LOOP_RCE
{
private:
  MEM_POOL *_pool;
  LOOP_DESCR *_loop;

  BB *_epilog;
  BB *_prolog;
  BB *_head;
  BB *_tail;
  BB_SET *_loop_bbs;

  BOOL _well_formed;     // whether it's good the do RCE for this loop
  BOOL _trace;          

  std::list< OP* > _up_copies;
  std::list< OP* > _down_copies;

  INT32 _chain_num;
  std::vector< std::vector<CopyRec> > _chains;
  std::vector<BOOL> _circular_chains;

  INT32 _del_num;        // number of total deleted OPs
private:
  void Find_Epilog();
  void Find_Prolog();

  void Find_Chain();

  void Move_To_Prolog(INT32);
  void Move_To_Epilog(INT32);

public:
  LOOP_RCE( LOOP_DESCR *, MEM_POOL * );
  ~LOOP_RCE();

  void Redundant_Copy_Elimination();
};

LOOP_RCE::LOOP_RCE( LOOP_DESCR *l, MEM_POOL *p) : 
  _loop(l), _pool(p), 
  _well_formed(TRUE),
  _epilog(NULL),
  _prolog(NULL),
  _head(NULL),
  _tail(NULL),
  _chain_num(0),
  _del_num(0)
{
  _head = LOOP_DESCR_loophead(_loop);
  _loop_bbs = LOOP_DESCR_bbset(_loop);
  
  _trace = Get_Trace (TP_GCM, GCM_TRACE_RCE );

  Find_Prolog();
  Find_Epilog();
}

LOOP_RCE::~LOOP_RCE()
{
}


//================================================================
// LOOP_RCE::Find_Prolog
//
//================================================================
void LOOP_RCE::Find_Prolog()
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

  if( _prolog && (BB_prev(_head) != _prolog) ){
    DevWarn( "prolog BB:%d not fall through to BB:%d. Ugly due to cflow",
             BB_id(_prolog), BB_id(_head) );
    DevWarn( "I don't do RCE for it" );
    _well_formed = FALSE;
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
// LOOP_RCE::Find_Epilog
//
//================================================================
void LOOP_RCE::Find_Epilog()
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
    _well_formed = FALSE;
  }else{
    if( _trace )
      fprintf( TFile, "epilog is BB:%i\n", BB_id(_epilog) );
  }

  return;
}
  

//================================================================
// Following is a list of auxiliary functions:
//   OP_cannot_remove
//================================================================
static inline BOOL OP_not_in_list( OP *op, std::list<OP*> live_ops )
{
  std::list<OP*>::const_iterator cit = live_ops.begin();
  for( ; cit != live_ops.end(); cit++ ){
    if( op == *cit )
      return FALSE;
  } 
  return TRUE;
}


//================================================================
// OP_is_copy
//================================================================
static inline BOOL OP_is_copy( OP *op )
{
  BOOL ret = OP_copy(op);
#ifdef TARG_SL
  if( (OP_code(op)==TOP_c3_mvfacc) || (OP_code(op)==TOP_c3_mvtacc) ||
      (OP_code(op)==TOP_c3_mvfaddr) || (OP_code(op)==TOP_c3_mvtaddr) ){
    Is_True( OP_opnds(op)==2, ("opnd number is not 2, incorrect") );
    TN *shift_tn = OP_opnd(op, 1);
    if( TN_is_constant(shift_tn) && TN_value(shift_tn)==0 )
      ret |= TRUE;
  } 
#endif
  return ret;
}

//================================================================
// Do_Non_Copy
// 
// For topo order traverse, we search for downward exposed copy OPs.  
// A downward exposed copy OP is one whose opnd&result are not referenced
// or defined in later non-copy OPs
//
// For rev-topo order traverse, we search for upward exposed copy OPs.
// A upward exposed copy OP is one whose opnd&result are not referenced
// or defined in former non-copy OPs
//
// A problem about removing copy OPs when it's not good candidate
//
// In a copy list, if one copy OP is determined to be a bad candidate (some 
// non OP uses/defs its TN), then the related copy OPs in the copy list
// need NOT to be removed. For example:
//
// .lable
//   tn3 = tn2
//   .. alus
//   tn2 = tn3
//   tn1 = tn2
//   tn1++
//   br label
//================================================================
static void Do_Non_Copy( OP *op, std::list<OP*> *copies )
{

  // I cannot use std::list<OP*>::iterator with std::list.remove() at the
  // the same time, so I use the outermost loop trickliy. Meanwhile some 
  // OPs will be traversed several times, so I use a counter to forbide this
  
  INT32 last_trip = 0;  // how many exposed copies are determined to be good
                        // candidates in the list so far.
  BOOL removed = TRUE;
  while( removed ){
    removed = FALSE;
    std::list<OP*>::iterator it = copies->begin();
    for(INT32 i=0; i<last_trip; i++)
      it++;
    for( ; (it != copies->end()) ; it++ ){
      INT16 i=0;
      OP *copy = *it;
      for( i=0; i < OP_opnds(copy); i++ ){
        TN *tn = OP_opnd(copy, i);
        if( (tn != Zero_TN) &&
            !TN_is_constant(tn) &&
            (OP_Refs_TN(op, tn) || OP_Defs_TN(op,tn)) ){
          copies->remove(copy);
          removed = TRUE;
          break;
        }
      }
      for( i=0; !removed && (i < OP_results(copy)); i++ ){
        TN *tn = OP_result(copy, i);
        if( (tn != Zero_TN) &&
            !TN_is_constant(tn) &&
            ( OP_Refs_TN(op, tn) || OP_Defs_TN(op,tn) ) ){
          copies->remove(copy);
          removed = TRUE;
          break;
        }
      }
      if( removed )
        break;
      last_trip++;
    }
  }
  return;
}

//================================================================
//
// return TRUE : if op_a is before op_b
//
// this is limited to one BB region, but it's fine since my focus is 
// just single BB loop
// 
// If op_a==op_b, it also returns FALSE.
//================================================================
static BOOL OP_Before_OP( OP *op_a, OP *op_b )
{
  OP *op = OP_prev(op_b); 
  while( op ){
    if( op == op_a )
      return TRUE;
    op = OP_prev(op);
  }
  return FALSE;
}

//================================================================
// LOOP_RCE::Find_Chain
//
// Find the chains consisting of copies. Each chain forms a circle
//================================================================
void LOOP_RCE::Find_Chain()
{
  if( _trace ){
    fprintf(TFile, " the up exposed copies are (bottom up): \n" );
  }

  //bottom up go through _up_copies
  std::list<OP*>::const_iterator cit = _up_copies.begin();
  for( ; cit != _up_copies.end(); cit++ ){
    if( _trace )
      Print_OP_No_SrcLine(*cit);

    CopyRec rec;
    rec.dest = OP_result(*cit, 0);
    rec.src = OP_opnd(*cit, 0);
    rec.op = *cit;
    rec.up_copy = TRUE;

    INT chain_id = 0;
    BOOL appended = FALSE;
    for( ; chain_id < _chain_num; chain_id++ ){
      INT size = _chains[chain_id].size();
      if( size == 0 )
        continue;
      Is_True( !TN_is_constant(rec.dest) && !TN_is_constant(rec.src) &&
               !TN_is_constant(_chains[chain_id][size-1].dest) &&
               !TN_is_constant(_chains[chain_id][size-1].src),
               ("TN is constant in a copy OP") ); 
      if( TN_number(rec.dest) == TN_number(_chains[chain_id][size-1].src) ){
        _chains[chain_id].push_back(rec);
        appended = TRUE;
      }
    }
   
    if( !appended ){
      _chain_num++;
      _chains.resize(_chain_num);
      _chains[_chain_num-1].push_back(rec);
    }
  } 

  //bottom up go through _down_copies. and we need use reverse iterator
  //since we built it top down
  if( _trace ){
    fprintf(TFile, " the down exposed copies are (bottom up): \n" );
  }

  std::list<OP*>::const_reverse_iterator rcit;
  rcit = _down_copies.rbegin();
  INT32 i = _down_copies.size();
  // There is no operator != for std::list.rend(), so I use it size to count
  for( ; i > 0; i--, rcit++ ){
    if( _trace )
      Print_OP_No_SrcLine(*rcit);

    CopyRec rec;
    rec.dest = OP_result(*rcit, 0);
    rec.src = OP_opnd(*rcit, 0);
    rec.op = *rcit;
    rec.up_copy = FALSE;

    INT chain_id = 0;
    BOOL appended = FALSE;
    for( ; chain_id < _chain_num; chain_id++ ){
      INT size = _chains[chain_id].size();
      if( size == 0 )
        continue;
      Is_True( !TN_is_constant(rec.dest) && !TN_is_constant(rec.src) &&
               !TN_is_constant(_chains[chain_id][size-1].dest) &&
               !TN_is_constant(_chains[chain_id][size-1].src),
               ("TN is constant in a copy OP") ); 
      if( TN_number(rec.dest) == TN_number(_chains[chain_id][size-1].src) ){
        _chains[chain_id].push_back(rec);
        appended = TRUE;
      }
    }
   
    if( !appended ){
      _chain_num++;
      _chains.resize(_chain_num);
      _chains[_chain_num-1].push_back(rec);
    }
  } 

  // Need to figure out if the chain forms a circle
  INT32 j = 0; 

  for( i=0; i < _chain_num; i++ ){
    j = _chains[i].size();
    Is_True( j>0, ("chain size == 0") );
    OP *first_op = _chains[i][0].op;
    OP *last_op = _chains[i][j-1].op;
    TN *dest_tn = OP_result(first_op, 0);
    TN *src_tn = OP_opnd(last_op, 0);

    // Need to make sure the up exposed OPs are before the down exposed OPs
    if( TN_number(dest_tn) == TN_number(src_tn) &&
        OP_Before_OP(first_op, last_op) ){
      _circular_chains.push_back(TRUE);
    }else
      _circular_chains.push_back(FALSE);
  }

  if( _trace ){
    fprintf( TFile, " --- The Chains are : --- \n" );
    for( i=0 ; i < _chain_num; i++ ){
      fprintf( TFile, " the %ith chain : \n", i );
      for(j=0 ; j < _chains[i].size(); j++ ){
        fprintf( TFile, "%s,", 
                 _chains[i][j].up_copy ? "up exposed" : "down exposed" );
        Print_OP_No_SrcLine( _chains[i][j].op );
      }
      if( _circular_chains[i] )
        fprintf(TFile, " is circular \n" );
      else
        fprintf(TFile, " is NOT circular \n" );
    }  
  }
}

//================================================================
// Update_Liveness
//
// Update liveness after moving op from src_bb to tgt_bb
//================================================================
static void Update_Liveness( BB *src_bb, BB *tgt_bb, OP *op, BOOL upward )
{
  INT32 i;
  for( i=0; i < OP_opnds(op); i++ ){
    TN *tn = OP_opnd(op,i);
    if( TN_is_constant(tn) )
      continue;
    if( !TN_is_global_reg(tn) )
      GTN_UNIVERSE_Add_TN(tn);

    if( upward ){
      GRA_LIVE_Add_Live_Use_GTN(tgt_bb, tn);
      GRA_LIVE_Add_Live_Out_GTN(tgt_bb, tn);
    }else{
      GRA_LIVE_Add_Live_In_GTN(tgt_bb, tn);
      GRA_LIVE_Add_Live_Use_GTN(tgt_bb, tn);
      GRA_LIVE_Add_Defreach_In_GTN(tgt_bb, tn);

      GRA_LIVE_Add_Live_Out_GTN(src_bb, tn);
      GRA_LIVE_Add_Defreach_Out_GTN(src_bb, tn);
    }
  }

  for( i=0; i < OP_results(op); i++ ){
    TN *tn = OP_result(op,i);
    if( TN_is_constant(tn) )
      continue;
    if( !TN_is_global_reg(tn) )
      GTN_UNIVERSE_Add_TN(tn);

    if( upward ){
      GRA_LIVE_Add_Defreach_Out_GTN(tgt_bb, tn);
      GRA_LIVE_Add_Defreach_In_GTN(src_bb, tn);
    }else{
    }
  }
}

//================================================================
// LOOP_RCE::Move_To_Prolog
//================================================================
void LOOP_RCE::Move_To_Prolog(INT32 chain_id)
{
  INT32 size = _chains[chain_id].size();
  INT32 i = size-1;
  for( ; i >= 0; i-- ){
    OP *op = _chains[chain_id][i].op;
    if( _chains[chain_id][i].up_copy ){
      Is_True( !BB_zdl_body(_prolog), ("prolog is tail BB of a ZDL") );
      BB *src_bb = OP_bb(op);
      BB_Remove_Op( src_bb, op );
   
      // if the _prolog is of ZDL, then it has TOP_loop at the end
      OP *loop_op = BB_loop_op(_prolog);
      if( loop_op )
        BB_Insert_Op_Before( _prolog, loop_op, op );
      else
        BB_Append_Op( _prolog, op );

      Update_Liveness( src_bb, _prolog, op, TRUE ); 
      _del_num++;
      if( _trace ){
        fprintf( TFile, "move following OP to prolog:\n" );
        Print_OP_No_SrcLine(op);
      }
    }
  } 
}

//================================================================
// LOOP_RCE::Move_To_Epilog
//================================================================
void LOOP_RCE::Move_To_Epilog(INT32 chain_id)
{
  INT32 size = _chains[chain_id].size();
  INT32 i = 0;
  for( ; i < size; i++ ){
    OP *op = _chains[chain_id][i].op;
    if( !_chains[chain_id][i].up_copy ){
      BB *src_bb = OP_bb(op);
      BB_Remove_Op( src_bb, op );
      BB_Prepend_Op( _epilog, op );
      Update_Liveness( src_bb, _epilog, op, FALSE ); 
      _del_num++;
      if( _trace ){
        fprintf( TFile, "move following OP to epilog:\n" );
        Print_OP_No_SrcLine(op);
      }
    }
  } 
}

//================================================================
// LOOP_RCE::Redundant_Copy_Elimination
//================================================================
void LOOP_RCE::Redundant_Copy_Elimination()
{
  if( !_well_formed )
    return;

  TOPO_ITER topo_it(_loop, TOPO, _pool);
  TOPO_ITER r_topo_it(_loop, REV_TOPO, _pool);

  if( !topo_it.well_formed() || !topo_it.sequential() )
    return;

  if( _trace )
    fprintf( TFile, "==== begin LOOP Redundant Copy Elimination ====\n" );

  // NOTE: GTN uses GTN_Universe, a map, to save memory space, since 
  //       GTNs are sparse in the TN world. So I need to transform 
  //       GTN_SET to TN_SET
  TN_SET *live_tns;
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

    fprintf( TFile, "\nthe live in TNs of epilog:" );
    TN_SET_Print( live_tns, TFile );
    fprintf( TFile, "\n" );
  }

  // Decide the up/down exposed copy OPs. There are some problems need care:
  // (1) Using following algorithm, an OP can be both up exposed and down exposed
  //     but if I check down-expose op should be behind up-expose op, then this
  //     problem can be avoided

  // find the downward exposed copy OPs
  BB *bb = topo_it.begin();
  topo_it.set_begin();
  for( ; *topo_it != topo_it.end(); ++topo_it ){
    bb = *topo_it;
    OP *op;
    FOR_ALL_BB_OPs(bb, op){
      if( OP_is_copy(op) ){
        _down_copies.push_back(op);
      }else
        Do_Non_Copy(op, &_down_copies);
    }
  } 

  // find the upward exposed copy OPs
  bb = r_topo_it.begin();
  r_topo_it.set_begin();
  for( ; *r_topo_it != r_topo_it.end(); ++r_topo_it ){

    bb = *r_topo_it;
    OP *op;
    FOR_ALL_BB_OPs_REV(bb, op){
      if( OP_is_copy(op) )
        _up_copies.push_back(op);
      else
        Do_Non_Copy(op, &_up_copies);
    }
  } 

  // find the chain, consisting of _up_copies and _down_copies
  Find_Chain();

  // Some of the chain may consist of only OPs from _up_copies or down
  // exposed copies. This doesnt matter.

  INT32 chain_id = 0;
  for( ; chain_id < _chain_num; chain_id++ ){
    if( !_circular_chains[chain_id] )
      continue;
    
    // Copy up exposed copies to prolog
    // There should be some dead code after this motion, we leave the cleaning
    // work in later LOOP_DCE::Dead_Code_Elimination phase
    Move_To_Prolog( chain_id );

    // Copy down exposed copies to epilog
    Move_To_Epilog( chain_id );
  }

  if( _trace ){
    fprintf( TFile, " Total delete OPs: %i\n", _del_num );
    fprintf( TFile, "==== end LOOP Redundant Copy Elimination ====\n" );
  }
}

//================================================================
// This is the external interface of GCM DCE
//================================================================
void LOOP_Redundant_Copy_Elimination( LOOP_DESCR *loop, MEM_POOL *pool )
{
  LOOP_RCE loop_rce(loop, pool);
  loop_rce.Redundant_Copy_Elimination();
}
