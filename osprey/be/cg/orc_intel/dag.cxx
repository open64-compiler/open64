/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without 
  modification,are permitted provided that the following conditions are 
  met:
  
  Redistributions of source code must retain the above copyright notice, 
  this list of conditions and the following disclaimer. 
  
  Redistributions in binary form must reproduce the above copyright notice, 
  this listof conditions and the following disclaimer in the documentation 
  and/or other materials provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used 
  to endorse or promote products derived from this software without specific 
  prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
  AND ANY EXPRESS ORIMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
  ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY 
  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//-*-c++-*-

//*********************************************************************
//
// Module: dag.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/dag.cxx,v $
//
// Description:
//
// DAG (dependence graph) construction related utilities.
//
//*********************************************************************

#include <set>
#include <utility>
#include <ext/hash_map>
#include "defs.h"
#include "tracing.h"
#include "cgtarget.h"
#include "cg_dep_graph.h"
#include "w2op.h"
#include "targ_proc_properties.h"
#include "ipfec_defs.h"
#include "region_bb_util.h"
#include "dag.h"
#include "prdb.h"
#include "region_bb_util.h"
#include "op_targ.h"



    
    
// -----------------------------------------------------------------------
// OP_Shadowed_By_Prev_OPs
// Placeholder routine to exclude unnecessary dependencies.
// -----------------------------------------------------------------------

extern MEM_POOL dep_nz_pool;
extern TN_LIST *same_reg[REGISTER_MAX+1][ISA_REGISTER_CLASS_MAX+1];

TN *
set_representative_tn (TN* tn) {
    if (!TN_is_register(tn) || TN_register(tn) == REGISTER_UNDEFINED) {
        return tn ;
    }

    REGISTER rnum = TN_register(tn);
    ISA_REGISTER_CLASS rclass = TN_register_class(tn);
    TN_LIST *tns = same_reg[rnum][rclass]; 
    
    if (tns) { return TN_LIST_first(tns); }
    
    same_reg[rnum][rclass] = TN_LIST_Push(tn, same_reg[rnum][rclass],
                    &dep_nz_pool);
    
    return tn ;
}

TN *
get_representative_tn (TN *tn) {
    if (!TN_is_register (tn)) return tn ;
    REGISTER rnum = TN_register(tn);
    ISA_REGISTER_CLASS rclass = TN_register_class(tn);
    TN_LIST *tns;

    for (tns = same_reg[rnum][rclass]; tns; tns = TN_LIST_rest(tns)) {
        return TN_LIST_first(tns) ;
    }

    return tn ;
}

INT16 
get_opnd_idx (OP *op , TN* tn) {
    tn = set_representative_tn(tn);
    for (INT16 idx = OP_opnds (op) - 1 ; idx >= 0 ; idx--) {
        TN * opnd = OP_opnd(op,idx); 
        opnd = set_representative_tn (opnd) ;
        if (opnd == tn) return idx ;
    }

    return -1;
}

void
adjust_reganti_latency (ARC *arc) {
    if (ARC_kind (arc) != CG_DEP_REGANTI) return ;

    OP * pred = ARC_pred (arc) ;
    TN * tn = OP_opnd(pred,ARC_opnd(arc)) ;   

    if (TN_is_register(tn) && TN_register_class(tn) == 
        ISA_REGISTER_CLASS_predicate) {
      return ;    
    }
    
    arc->latency = 0 ;
}

void
adjust_reganti_latency (BB *bb) {
    
    OP * op ;
    FOR_ALL_BB_OPs (bb, op) {
        for (ARC_LIST* arcs = OP_succs(op); arcs != NULL; 
            arcs = ARC_LIST_rest(arcs)) {
            ARC *arc = ARC_LIST_first(arcs) ;
            if (ARC_kind(arc) != CG_DEP_REGANTI) continue ;
            adjust_reganti_latency (arc) ;       
        }
    }
}

inline mINT32 DAG_BUILDER::Get_Table_Index(OP* op){
// get the index of op's BB in _BB_Bitset_Table_Index
  return _BB_Bitset_Table_Index[BB_id(OP_bb(op))];
}

inline mINT32 DAG_BUILDER::Get_Table_Index(BB* bb){
// get the bb's index in _BB_Bitset_Table_Index
  return _BB_Bitset_Table_Index[BB_id(bb)];
}


inline mINT32 DAG_BUILDER::Get_Table_Index(TN* tn){
// get the tn's index in _TN_Bitset_Table_Index
  return _TN_Bitset_Table_Index[TN_number(tn)];
}


BOOL
DAG_BUILDER::OP_Shadowed_By_Prev_OPs(OP* defop,
                                         OPs& prev_def_ops, 
                                         COMPARE_FUNCTION comp_func)
{
  // Place holder for further enhancement.
  return FALSE;
}

DAG_BUILDER::OPs&
DAG_BUILDER::Get_Def_Use_OPs(OP *op, UINT8 res, CG_DEP_KIND arc_kind)
{
  TN *tn;
  if (arc_kind == CG_DEP_REGIN) {
    tn = OP_opnd(op, res);
  } else if (arc_kind == CG_DEP_REGOUT ||
             arc_kind == CG_DEP_REGANTI) {
    tn = OP_result(op, res);
  }
  
  tn = set_representative_tn (tn);

  if (!TN_is_register(tn) || TN_is_true_pred(tn)) {
    return _empty_set;
  }
  
  if (arc_kind == CG_DEP_REGIN) {
    return _bb_def_info_map[OP_bb(op)][tn];
  } else if (arc_kind == CG_DEP_REGOUT) {
    return _bb_def_info_map[OP_bb(op)][tn];
  } else if (arc_kind == CG_DEP_REGANTI) {
    return _bb_use_info_map[OP_bb(op)][tn];
  } else {
    Is_True(FALSE, ("Shouldn't come here"));
  }
}

void
DAG_BUILDER::Get_Define_OPs(OP *op, UINT8 res, CG_DEP_KIND arc_kind)
{
  _Define_OPs.clear();
  
  TN *tn;
  if (arc_kind == CG_DEP_REGIN ||
      arc_kind == CG_DEP_REGANTI) { 
    tn = OP_opnd(op, res);
  } 
  else if (arc_kind == CG_DEP_REGOUT ) {
    tn = OP_result(op, res);
  }
  
  tn = set_representative_tn (tn);

  if (!TN_is_register(tn) || TN_is_true_pred(tn)) {
    return;  // return _empty_set;
  }
  
  // start dealing with arc_kind
  if (arc_kind == CG_DEP_REGIN ||    // RAW 
      arc_kind == CG_DEP_REGOUT ) { // WAW

    // if there is some latest ops inside this BB ,which define tn, 
    // then return the latest ops. 
    // else push_back  [B].in into vect--def_ops 
    BS* def_ops = BS_Create_Empty (_OP_Count, &_mem_pool);
    BS_CopyD( def_ops , 
             _BB_Bitset_Table[Get_Table_Index(op)].gen ,
             &_mem_pool );
    BS_IntersectionD( def_ops ,
                    _TN_Bitset_Table[Get_Table_Index(tn)].def );
         // [B].gen & _TN_Bitset_Table[].def
    
    // find the latest ops inside OP_bb(op) , which def tn
    OP* latest_op=NULL;  // the latest op which def tn

    BS_ELT elt;
    for ( elt = BS_Choose( def_ops );
          elt != BS_CHOOSE_FAILURE && elt < OP_To_Gid(op);// ascending order
          elt = BS_Choose_Next( def_ops, elt ) ){
      OP* tmp = Gid_To_OP(elt);
      if (!OP_has_disjoint_predicate(tmp, op)) {
        latest_op=Gid_To_OP(elt);
        _Define_OPs.push_back(latest_op);
      }
    }
        
    // push back the relative ops into _Define_OPs
    if(latest_op == NULL){   // there is no op inside this bb, which define tn.
                             // then return in[B]
      BS_CopyD( def_ops , 
                _BB_Bitset_Table[Get_Table_Index(op)].in, 
                &_mem_pool );
      BS_IntersectionD( def_ops , 
                      _TN_Bitset_Table[Get_Table_Index(tn)].def );
                             // [B].in & _TN_Bitset_Table[].def
      for ( elt = BS_Choose( def_ops );
            elt != BS_CHOOSE_FAILURE ;
            elt = BS_Choose_Next( def_ops, elt ) ){
        _Define_OPs.push_back( Gid_To_OP(elt) ); 
      }
    }

  }

  else if (arc_kind == CG_DEP_REGANTI) {

    // get def info of this tn in reverse topological order  
    BS* def_ops = BS_Create_Empty (_OP_Count, &_mem_pool);
    BS_CopyD( def_ops , 
              _BB_Bitset_Table[Get_Table_Index(op)].gen , 
              &_mem_pool );

    BS_IntersectionD( def_ops , 
                    _TN_Bitset_Table[Get_Table_Index(tn)].def );
                             // [B].gen & _TN_Bitset_Table[].def
    


    // find the nearest op inside OP_bb(op) and after op , which def tn
    OP* nearest_op=NULL;  // the nearest op which def tn

    BS_ELT elt;
    for ( elt = OP_To_Gid(op) , elt = BS_Choose_Next( def_ops, elt ) ; 
          // bypass case : i=i+1
          elt != BS_CHOOSE_FAILURE ;
          elt = BS_Choose_Next( def_ops, elt ) ){
      OP* tmp = Gid_To_OP(elt);
      if (!OP_has_disjoint_predicate(tmp, op)) {
        nearest_op=Gid_To_OP(elt);
        _Define_OPs.push_back(nearest_op);
      }
    }


    // push back the relative ops into _Define_OPs
    if(nearest_op == NULL){// there is no op inside this bb, which define tn . 
                           //then return reverse_in[B]
      BS_CopyD( def_ops , 
                _BB_Bitset_Table[ Get_Table_Index(op)].reverse_in , 
                &_mem_pool );
      BS_IntersectionD( def_ops , 
                      _TN_Bitset_Table[Get_Table_Index(tn)].def );
                          // reverse_in[B] & _TN_Bitset_Table[].def

      for ( elt = BS_Choose( def_ops );
            elt != BS_CHOOSE_FAILURE ;
            elt = BS_Choose_Next( def_ops, elt ) ){
        _Define_OPs.push_back( Gid_To_OP(elt) ); 
      }
    }

  } 
  else {// arc_kind is unknown
    Is_True(FALSE, ("Shouldn't come here"));
  }
}


inline mUINT16 DAG_BUILDER::OP_To_Gid(OP* op){
// return the OP's global Id
  return op->order;
}

inline OP* DAG_BUILDER::Gid_To_OP(mINT32 gid){
// get the OP pointer ,whose <order> is gid ,from OP_Table 
  return _OP_Table[gid];
}

void
DAG_BUILDER::Init_TN_BB_Bitset_Table(void)
{
  // Initialize  _TN_Bitset_Table_Index[] ;
  mINT32 i;
  _TN_Bitset_Table_Index.resize(_Max_TN_number+1); // 0.._Max_TN_number

  for ( i = 0; i < _Max_TN_number+1; i++)
    _TN_Bitset_Table_Index[i] = -1;

  // Initialize  _BB_Bitset_Table_Index[] and _BB_Bitset_Table[];
  _BB_Bitset_Table_Index.resize(_Max_BB_id+1); // 0.._Max_BB_id
  for ( i = 0; i < _Max_BB_id+1; i++)
    _BB_Bitset_Table_Index[i] = -1;
    
  _BB_Bitset_Table.resize(_BB_Count);
  for ( i = 0; i < _BB_Count; i++){
    _BB_Bitset_Table[i].in = BS_Create_Empty (_OP_Count, &_mem_pool);
    _BB_Bitset_Table[i].out = BS_Create_Empty (_OP_Count, &_mem_pool);
    _BB_Bitset_Table[i].kill = BS_Create_Empty (_OP_Count, &_mem_pool);
    _BB_Bitset_Table[i].gen = BS_Create_Empty (_OP_Count, &_mem_pool);
    _BB_Bitset_Table[i].reverse_in = BS_Create_Empty (_OP_Count, &_mem_pool);
    _BB_Bitset_Table[i].reverse_out = BS_Create_Empty (_OP_Count, &_mem_pool);
    
  }
}

BS*
DAG_BUILDER::Union_Of_Preds(BB* bb)
{

  // Return the union of <out> of all bb's preds
  _out = BS_Create_Empty (_OP_Count, &_mem_pool);

  BB_VECTOR bbv(&_mem_pool);

  Find_Ancestor_BB (bb, &bbv);

  for (BB_VECTOR_ITER iter = bbv.begin () ;
       iter != bbv.end() ; 
       iter ++) {
  
      BB* pred = *iter ;
      BS_UnionD(_out , 
                _BB_Bitset_Table[Get_Table_Index(pred)].out , 
                &_mem_pool);// _out+ out
  }
  return _out;

  
}

BS*
DAG_BUILDER::Union_Of_Succs(BB* bb)
{

  // Return the union of <out> of all bb's succs
  _reverse_out = BS_Create_Empty (_OP_Count, &_mem_pool);

  BB_VECTOR bbv(&_mem_pool);

  Find_Successor_BB (bb, &bbv);

  for (BB_VECTOR_ITER iter = bbv.begin () ;
       iter != bbv.end() ; 
       iter ++) {
  
      BB* succ = *iter ;
      BS_UnionD(_reverse_out , 
                _BB_Bitset_Table[Get_Table_Index(succ)].reverse_out , 
                &_mem_pool);// reverse_out+out
  }
  return _reverse_out;
  
}

void
DAG_BUILDER::Set_TN_BB_Bitset_Table(void)
{
  mINT32 TN_Table_Tail=0,BB_Table_Tail=0;

  // Initialize _TN_Bitset_Table[0]
  TN_BITSET_TABLE_ENTRY  Blank_Entry ;
  Blank_Entry.def = BS_Create_Empty (_OP_Count, &_mem_pool);
  Blank_Entry.use = BS_Create_Empty (_OP_Count, &_mem_pool);
  _TN_Bitset_Table.push_back(Blank_Entry); 
  
  // Traverse each op , and stroe op's Gid into  def & use  in the TN_Table 
  for (TOPOLOGICAL_REGIONAL_CFG_ITER cfg_iter(_region->Regional_Cfg());
         cfg_iter != 0;
         ++cfg_iter) {

    if (!(*cfg_iter)->Is_Region()) {
      if ( BB_entry((*cfg_iter)->BB_Node()) || BB_exit((*cfg_iter)->BB_Node()))
        continue;
       
      BB* bb = (*cfg_iter)->BB_Node();  
      // store the bb pointer
      _BB_Bitset_Table[BB_Table_Tail].bb=bb;

      // traverse each op, store the TN def&use info into _TN_Bitset_Table
      OP *op;
      FOR_ALL_BB_OPs(bb, op) {
        INT i;
        for ( i = 0; i < OP_results(op); i++) {
          TN *result_tn = OP_result(op,i);
          result_tn = set_representative_tn (result_tn); 
          // if TN is register TN and is not constant register TN
          if (TN_is_register(result_tn)&&!TN_is_true_pred(result_tn)) {
          // store the OP's Gid which defines result_tn ; 
          // modify _TN_Bitset_Table_Index

          if (_TN_Bitset_Table_Index[ TN_number(result_tn) ] == -1) { 
                // result_tn not exists 
          BS_Union1D(_TN_Bitset_Table[TN_Table_Tail].def,
                     OP_To_Gid(op),
                     &_mem_pool);
          _TN_Bitset_Table_Index[ TN_number(result_tn) ] = TN_Table_Tail++;
          // TN_Table_Tail always point to the newest blank element
  
          // Initialize _TN_Bitset_Table[TN_Table_Tail]
          TN_BITSET_TABLE_ENTRY  Blank_Entry ;
          Blank_Entry.def = BS_Create_Empty (_OP_Count, &_mem_pool);
          Blank_Entry.use = BS_Create_Empty (_OP_Count, &_mem_pool);
          _TN_Bitset_Table.push_back(Blank_Entry);  
        
          }
          else{// result_tn has been registered,TN_Table_Tail not increse
            BS_Union1D(_TN_Bitset_Table[Get_Table_Index(result_tn)].def,
                       OP_To_Gid(op),
                       &_mem_pool);               
          }
          
          }
        }// for
        for ( i = 0; i < OP_opnds(op); i++) {
          TN *opnd_tn = OP_opnd(op,i);
          opnd_tn = set_representative_tn (opnd_tn);
          // if TN is register TN and is not constant register TN
          if (TN_is_register(opnd_tn)&&!TN_is_true_pred(opnd_tn)) {
          // store the OP's Gid which uses opnd_tn ; 
          // modify _TN_Bitset_Table_Index

          if (_TN_Bitset_Table_Index[ TN_number(opnd_tn) ] == -1) {
               // opnd_tn not exists 
            BS_Union1D(_TN_Bitset_Table[TN_Table_Tail].use, 
                       OP_To_Gid(op),
                       &_mem_pool);
            _TN_Bitset_Table_Index[ TN_number(opnd_tn) ] = TN_Table_Tail++;
            // TN_Table_Tail always point to the newest blank element
  
            // Initialize _TN_Bitset_Table[TN_Table_Tail]
            TN_BITSET_TABLE_ENTRY  Blank_Entry ;
            Blank_Entry.def = BS_Create_Empty (_OP_Count, &_mem_pool);
            Blank_Entry.use = BS_Create_Empty (_OP_Count, &_mem_pool);
            _TN_Bitset_Table.push_back(Blank_Entry);  
        
          }
          else{  // opnd_tn has been registered,TN_Table_Tail not increse
              BS_Union1D(_TN_Bitset_Table[Get_Table_Index(opnd_tn)].use,
                         OP_To_Gid(op),
                         &_mem_pool);               
          }
          
          }
        }// for
          
        // record this op into _BB_Bitset_Table
        BS_Union1D(_BB_Bitset_Table[BB_Table_Tail].gen,
                   OP_To_Gid(op),
                   &_mem_pool);
      }// FOR_ALL_BB_OPs(bb, op)
      // Update _BB_Bitset_Table_Index
      _BB_Bitset_Table_Index[BB_id(bb)]=BB_Table_Tail++;
      // BB_Table_Tail always point to the newest blank element
    }
  }

  // calculate the field <kill> of _BB_Bitset_Table[]
  for(mINT32 Current_BB=0; Current_BB<_BB_Count; Current_BB++){
    
    BS* gen = BS_Create_Empty (_OP_Count, &_mem_pool);
    BS_CopyD( gen , _BB_Bitset_Table[Current_BB].gen , &_mem_pool );
    // gen=_BB_Bitset_Table[Current_BB].gen;   
    
    // Initialize [B].out and [B].reverse_out for the next step(calculate 
    // def and use info)
    BS_CopyD(_BB_Bitset_Table[Current_BB].out , gen ,  &_mem_pool );
    BS_CopyD(_BB_Bitset_Table[Current_BB].reverse_out , gen ,  &_mem_pool );

    BS_ELT elt;
    for ( elt = BS_Choose( gen );
         elt != BS_CHOOSE_FAILURE;
         elt = BS_Choose_Next( gen, elt ) ){

        OP* op=Gid_To_OP(elt);
        for ( mINT32 i = 0; i < OP_results(op); i++) {
          TN *result_tn = OP_result(op,i);
          result_tn = set_representative_tn (result_tn); 
          // if TN is register TN and is not constant register TN
          if (TN_is_register(result_tn)&&!TN_is_true_pred(result_tn)) {
          BS_UnionD(_BB_Bitset_Table[Current_BB].kill ,
                    _TN_Bitset_Table[Get_Table_Index(result_tn)].def , 
                            // ops which define this result_tn
                   &_mem_pool);// kill+def
          // bypass ops which are inside this bb
          BS_DifferenceD(_BB_Bitset_Table[Current_BB].kill , gen);// kill-gen

          }
        }// for      
    }
  }

  // calculate the def and use info of _BB_Bitset_Table[].
  // originally, in[B]=null , out[B] = gen[B] 
  BOOL change=TRUE;
  while (change){

    change=FALSE;
    for(mINT32 current_bb=0; current_bb<_BB_Count; current_bb++){
      BS_CopyD(_BB_Bitset_Table[current_bb].in , 
               Union_Of_Preds( _BB_Bitset_Table[current_bb].bb ) ,&_mem_pool );

      BS* oldout = BS_Create_Empty (_OP_Count, &_mem_pool);
      BS_CopyD(oldout , _BB_Bitset_Table[current_bb].out ,&_mem_pool);
      // oldout=_BB_Bitset_Table[current_bb].out

      BS* tmp_set = BS_Create_Empty (_OP_Count, &_mem_pool);
      BS_CopyD(tmp_set , _BB_Bitset_Table[current_bb].in ,&_mem_pool);
      // tmp_set=_BB_Bitset_Table[current_bb].in

      // BS_DifferenceD(tmp_set , _BB_Bitset_Table[current_bb].kill); 
      //in[B]-kill[B]
      BS_UnionD(tmp_set , _BB_Bitset_Table[current_bb].gen , &_mem_pool);
      // gen[B]+(in[B]-kill[B])
      BS_CopyD(_BB_Bitset_Table[current_bb].out , tmp_set , &_mem_pool); 
      // _BB_Bitset_Table[current_bb].out = tmp_set;

      if (!BS_EqualP(tmp_set , oldout))  change = TRUE;
    }
      
  }

  // calculate the reverse_in and reverse_out of _BB_Bitset_Table[]
  // originally , reverse_in[B] = null , reverse_out[B] = gen [B]
  BOOL changeR=TRUE;
  while (changeR) {

    changeR=FALSE;
    for(mINT32 current_bb=0; current_bb<_BB_Count; current_bb++) {
          
      BS_CopyD(_BB_Bitset_Table[current_bb].reverse_in , 
               Union_Of_Succs( _BB_Bitset_Table[current_bb].bb ) ,&_mem_pool );

      BS* oldoutR = BS_Create_Empty (_OP_Count, &_mem_pool);
      BS_CopyD(oldoutR , _BB_Bitset_Table[current_bb].reverse_out ,&_mem_pool);
      // oldoutR=_BB_Bitset_Table[current_bb].reverse_out

      BS* tmp_set = BS_Create_Empty (_OP_Count, &_mem_pool);
      BS_CopyD(tmp_set , _BB_Bitset_Table[current_bb].reverse_in ,&_mem_pool);
      // tmp_set=_BB_Bitset_Table[current_bb].reverse_in

      BS_UnionD(tmp_set , _BB_Bitset_Table[current_bb].gen , &_mem_pool);
      // gen[B]+ reverse_in[B]
      BS_CopyD(_BB_Bitset_Table[current_bb].reverse_out , tmp_set , &_mem_pool); 
      // _BB_Bitset_Table[current_bb].reverse_out = tmp_set;

      if (!BS_EqualP(tmp_set , oldoutR))  changeR = TRUE;
    }
      
  }

}

void
DAG_BUILDER::Update_Max_BB_id(BB* bb)
{
  _Max_BB_id=(BB_id(bb) > _Max_BB_id) ?
              BB_id(bb) :  
              _Max_BB_id ;
}


void
DAG_BUILDER::Set_Def_Use_OPs(OP *op)
{
  INT i;
  for (i = 0; i < OP_results(op); i++) {
    TN *result_tn = OP_result(op,i);
    result_tn = set_representative_tn (result_tn);

    Is_True(TN_is_register(result_tn), ("result is not register.\n"));
    if (!TN_is_true_pred(result_tn) &&
        !OP_Shadowed_By_Prev_OPs(op,
                                 _bb_def_info_map[OP_bb(op)][result_tn], 
                                 OP_has_subset_predicate)) {
      _bb_def_info_map[OP_bb(op)][result_tn].insert(op);
    }
  }

  for (i = 0; i < OP_opnds(op); i++) {
    TN *opnd_tn = OP_opnd(op,i);
    opnd_tn = set_representative_tn (opnd_tn);

    if (TN_is_register(opnd_tn) &&
        !TN_is_true_pred(opnd_tn) &&
        !OP_Shadowed_By_Prev_OPs(op,
                                 _bb_use_info_map[OP_bb(op)][opnd_tn], 
                                 OP_has_subset_predicate)) {
      _bb_use_info_map[OP_bb(op)][opnd_tn].insert(op);
    }
  }
}

void
DAG_BUILDER::Update_Max_TN_number(OP *op)
{   
  INT i;
  for ( i = 0; i < OP_results(op); i++) {
    TN *result_tn = OP_result(op,i);
    // if TN is register TN and is not constant register TN
    if (TN_is_register(result_tn)&&!TN_is_true_pred(result_tn)) {
      _Max_TN_number=(TN_number(result_tn) > _Max_TN_number) ?
                       TN_number(result_tn) :  
                       _Max_TN_number ;
    }
  }
  for ( i = 0; i < OP_opnds(op); i++) {
    TN *opnd_tn = OP_opnd(op,i);
    // if TN is register TN and is not constant register TN
    if (TN_is_register(opnd_tn)&&!TN_is_true_pred(opnd_tn)) {
      _Max_TN_number=(TN_number(opnd_tn) > _Max_TN_number) ?
                       TN_number(opnd_tn) :  
                       _Max_TN_number ;
    }
  }

}

inline 
void
DAG_BUILDER::Set_OP_Order(OP* op)
{
  op->order=_OP_Count;
}

void
DAG_BUILDER::Compute_Defs_Uses_In(BB* bb)
{
  BB_VECTOR bbv(&_mem_pool);

  Find_Ancestor_BB (bb, &bbv);

  for (BB_VECTOR_ITER iter = bbv.begin () ;
       iter != bbv.end() ; 
       iter ++) {

    BB* pred = *iter ;

    for (TN_OPs_MAP_ITER iter = _bb_def_info_map[pred].begin();
         iter != _bb_def_info_map[pred].end();
         iter++) {
      TN * tn = set_representative_tn (iter->first) ;
      _bb_def_info_map[bb][tn].insert(iter->second.begin(),
                                  iter->second.end());
    }
      
    for (TN_OPs_MAP_ITER iter = _bb_use_info_map[pred].begin();
           iter != _bb_use_info_map[pred].end();
           iter++) {
      TN * tn = set_representative_tn (iter->first) ;
      _bb_use_info_map[bb][tn].insert(iter->second.begin(),
                                  iter->second.end());
    }
  }
}

void
DAG_BUILDER::Compute_OPs_In(BB* bb)
{
  BB_VECTOR bbv(&_mem_pool);

  Find_Ancestor_BB (bb, &bbv);

  for (BB_VECTOR_ITER iter = bbv.begin () ;
       iter != bbv.end() ; 
       iter ++) {
  
      BB* pred = *iter ;

      _bb_ops_map[bb].insert(_bb_ops_map[pred].begin(),
                             _bb_ops_map[pred].end());
  }
}


BOOL
DAG_BUILDER::Is_Control_Speculative(OP* pred, OP* succ)
{
  // Handle branch op only.
  if (OP_br((OP *) pred)) {
    TOP opcode = OP_code(succ);
    if (OP_xfer(succ) || OP_volatile(succ) || OP_like_store(succ) ||
        TOP_is_ftrap(opcode) || TOP_is_itrap(opcode) ||
        TOP_is_fdiv(opcode)) {
      return FALSE;
    } else {
      return TRUE;
    }
  }

  return FALSE;
}

INT16
DAG_BUILDER::Find_Ancestor_BB (BB * bb, BB_VECTOR *bbv) {

    bbv->clear ();

    typedef mempool_allocator<REGIONAL_CFG_NODE *>  Node_ALLOC;
    typedef std::list<REGIONAL_CFG_NODE*,Node_ALLOC>    NODE_LIST;
    typedef NODE_LIST::iterator                    NODE_LIST_ITER;

    NODE_LIST nodes (&_mem_pool) ;
    nodes.push_back (Regional_Cfg_Node(bb));

    BS * visited_rgns = BS_Create_Empty (32/* arbitrary value */, &_mem_pool);
    for (CFG_PRED_NODE_ITER preds(Regional_Cfg_Node(bb));
         preds != 0; ++preds) {

         if ((*preds)->Is_Region ()) {
            nodes.push_back (*preds);
         } else {
            BB * b = (*preds)->BB_Node () ;
            if (find (bbv->begin () , bbv->end (), b) == bbv->end() &&
                !BB_entry (b) &&
                !BB_exit  (b)) {
                bbv->push_back (b);
            }
        }
    }

    while (!nodes.empty ()) {

        REGIONAL_CFG_NODE * n = *(nodes.begin ());
        nodes.erase (nodes.begin ());

        if (n->Is_Region ()) {

            REGION * r = n->Region_Node ();
            if (BS_MemberP (visited_rgns, r->Id())) {
               continue ; 
            } else {

                visited_rgns = BS_Union1D (visited_rgns, 
                                           r->Id (), 
                                           &_mem_pool);
                for (CFG_PRED_NODE_ITER ps(n); ps != 0; ++ps) {
                    
                    if ((*ps)->Is_Region ()) {
                        //bug fix for OSP_198
                        if (BS_MemberP (visited_rgns, (*ps)->Region_Node()->Id())) continue ;
                        nodes.push_back (*ps); 

                    } else {

                        BB * b = (*ps)->BB_Node ();
                        if (find (bbv->begin (), bbv->end (), b) == bbv->end() && 
                            !BB_entry (b) && 
                            !BB_exit  (b)) {
                           bbv->push_back (b);  
                        }
                    }
                } /* end of for (CFG_PRED...ps...) */

            } /* end of else clause */

        } /* end of if */

    } /* end of while */

    return bbv->size ();
}

INT16
DAG_BUILDER::Find_Successor_BB (BB * bb, BB_VECTOR *bbv) {

    bbv->clear ();

    typedef mempool_allocator<REGIONAL_CFG_NODE *>  Node_ALLOC;
    typedef std::list<REGIONAL_CFG_NODE*,Node_ALLOC>    NODE_LIST;
    typedef NODE_LIST::iterator                    NODE_LIST_ITER;

    NODE_LIST nodes (&_mem_pool) ;
    nodes.push_back (Regional_Cfg_Node(bb));

    BS * visited_rgns = BS_Create_Empty (32/* arbitrary value */, &_mem_pool);
    for (CFG_SUCC_NODE_ITER succs(Regional_Cfg_Node(bb));
         succs != 0; ++succs) {

         if ((*succs)->Is_Region ()) {
            nodes.push_back (*succs);
         } else {
            BB * b = (*succs)->BB_Node () ;
            if (find (bbv->begin () , bbv->end (), b) == bbv->end() &&
                !BB_entry (b) &&
                !BB_exit  (b)) {
                bbv->push_back (b);
            }
        }
    }

    while (!nodes.empty ()) {

        REGIONAL_CFG_NODE * n = *(nodes.begin ());
        nodes.erase (nodes.begin ());

        if (n->Is_Region ()) {

            REGION * r = n->Region_Node ();
            if (BS_MemberP (visited_rgns, r->Id())) {
               continue ; 
            } else {

                visited_rgns = BS_Union1D (visited_rgns, 
                                           r->Id (), 
                                           &_mem_pool);
                for (CFG_SUCC_NODE_ITER ps(n); ps != 0; ++ps) {
                    
                    if ((*ps)->Is_Region ()) {
                        // bug fix for OSP_198
                        if (BS_MemberP (visited_rgns, (*ps)->Region_Node()->Id())) continue ;
                        nodes.push_back (*ps); 

                    } else {

                        BB * b = (*ps)->BB_Node ();
                        if (find (bbv->begin (), bbv->end (), b) == bbv->end() && 
                            !BB_entry (b) && 
                            !BB_exit  (b)) {
                           bbv->push_back (b);  
                        }
                    }
                } /* end of for (CFG_SUCC...ps...) */

            } /* end of else clause */

        } /* end of if */

    } /* end of while */

    return bbv->size ();
}

// ====================================================================
// Computes the dependence graph for a SEME region.
//
// In addition to REG* and MEM* arcs, PREBR and POSBR dependence arcs
// are also inserted to preserve dependences across branch instructions.
// ====================================================================
void
DAG_BUILDER::Build_DAG()
{
  
  
  _OP_Count=0; _BB_Count=0;_Max_TN_number=0;_Max_BB_id=0;
  
  if (_region) {
    for (TOPOLOGICAL_REGIONAL_CFG_ITER cfg_iter(_region->Regional_Cfg());
         cfg_iter != 0;
         ++cfg_iter) {

      // Region node will become barrier of code motion, at least for now.
      if (!(*cfg_iter)->Is_Region()) {
        if ( BB_entry((*cfg_iter)->BB_Node()) || BB_exit((*cfg_iter)->BB_Node()))
          continue;
       
        BB* bb = (*cfg_iter)->BB_Node();  

        // _BB_Count only calculate those BB who are not Entry BB or 
        // Exit BB here
        _BB_Count++;
        Update_Max_BB_id(bb);// Update the _Max_BB_id

        BB_OP_MAP omap = BB_OP_MAP_Create(bb, &_mem_pool);
        BB_MAP_Set(_cg_dep_op_info, bb, omap);

        OP *op;
        FOR_ALL_BB_OPs(bb, op) {
          BB_OP_MAP_Set(omap, op, new_op_info());

          // update the <order> field in op
          Set_OP_Order(op);
          // stroe op's pointer into _OP_Table
          _OP_Table.push_back(op);
          // Increase the _Max_TN_number
          Update_Max_TN_number(op);
          _OP_Count++;
        }
      }
    }

    
#ifdef   DAG_BITSET_SWITCH_ON  // new version switch defined in dag.h
    // Initialize  _TN_Bitset_Table_Index , _BB_Bitset_Table_Index[] and 
    // _BB_Bitset_Table[]
    Init_TN_BB_Bitset_Table();
        
    // The second foward pass fills the _TN_Bitset_Table and _BB_Bitset_Table.
    // Traverse each op , and stroe op's Gid into  <def> & <use>  in the 
    // TN_Table ; at the same time ,modify the _BB_Bitset_Table[].gen and 
    // _BB_Bitset_Table_Index[];
    Set_TN_BB_Bitset_Table();
#endif

    for (TOPOLOGICAL_REGIONAL_CFG_ITER cfg_iter(_region->Regional_Cfg());
         cfg_iter != 0;
         ++cfg_iter) {

      // Region node will become barrier of code motion, at least for now.
      if (!(*cfg_iter)->Is_Region()) {
        if ( BB_entry((*cfg_iter)->BB_Node()) ||BB_exit((*cfg_iter)->BB_Node()))
          continue;

        BB* bb = (*cfg_iter)->BB_Node();

        // Compute the reaching definitions and uses in the head of bb.

#ifndef   DAG_BITSET_SWITCH_ON
        Compute_Defs_Uses_In(bb);
#endif
        
        // Compute the reaching memory OPs in the head of bb.
        Compute_OPs_In(bb);

        OP *op;
        // add an arc from every stack memory op to the SP adjustment op.
        if (BB_exit(bb)) {
          OP *exit_sp_adj = BB_exit_sp_adj_op(bb);
          for (op = exit_sp_adj; op != NULL; op = OP_prev(op)) {
            maybe_add_exit_sp_adj_arc (op, exit_sp_adj);
          }
        }

        FOR_ALL_BB_OPs(bb, op) {
          Build_Reg_Arcs(op);

          if (OP_load(op) || OP_like_store(op)) {
            Build_Mem_Arcs(op);
          }

          Build_Misc_Arcs(op);

          // generate all PREBR and POSTBR dependence arcs.
          if (_include_control_arcs) {
            Build_Branch_Arcs(op, TRUE);
          }

          // Push this op.
          _bb_ops_map[OP_bb(op)].insert(op);

          // Push this op into the sets of definitions and uses
#ifndef   DAG_BITSET_SWITCH_ON
          Set_Def_Use_OPs(op);
#endif
        }
      }
    }
  } else if (_bb) {
      CG_DEP_Compute_Graph (
          _bb,
          INCLUDE_ASSIGNED_REG_DEPS,
          NON_CYCLIC,
          INCLUDE_MEMREAD_ARCS,
          INCLUDE_MEMIN_ARCS,
          INCLUDE_CONTROL_ARCS,
          NULL);
      adjust_reganti_latency (_bb);
  }
}

// ====================================================================
// Constructor and Destructor.
// ====================================================================
DAG_BUILDER::DAG_BUILDER(REGION* region, 
                                 PRDB_GEN *prdb,
                                 BOOL    assigned_regs,
                                 BOOL    memread_arcs,
                                 BOOL    memin_arcs,
                                 BOOL    control_arcs) :
      _cyclic(FALSE),
      _prdb(prdb),
      _include_assigned_registers(assigned_regs),
      _include_memread_arcs(memread_arcs),
      _include_memin_arcs(memin_arcs),
      _include_control_arcs(control_arcs),
      _num_data_spec_arcs( 0 ),
      _num_cntl_spec_arcs( 0 ),
      _bb ( NULL ),
      _region( region ),
      _bb_ops_map(100, ptr_hash<BB*>(), std::equal_to<BB*>(),
                       BB_OPs_ALLOC(&_mem_pool)),
      _bb_def_info_map(100, ptr_hash<BB*>(), std::equal_to<BB*>(),
                       BB_DEF_USE_ALLOC(&_mem_pool)),
      _bb_use_info_map(100, ptr_hash<BB*>(), std::equal_to<BB*>(),
                       BB_DEF_USE_ALLOC(&_mem_pool))
{
  Invoke_Init_Routines();
}

DAG_BUILDER::DAG_BUILDER(BB* bb,PRDB_GEN *prdb) :
      _prdb(prdb),
      _num_data_spec_arcs( 0 ),
      _num_cntl_spec_arcs( 0 ),
      _bb( bb ),
      _region( NULL )
{}

DAG_BUILDER::~DAG_BUILDER()
{
  // Delete the DAG.
  if (_region) CG_DEP_Delete_DAG();
}

