/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 
  
  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//-*-c++-*-
//=============================================================================
//=============================================================================
//
//  Module :  cggrp_microsched.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/cggrp_microsched.cxx,v $
//
//  Description:
//  ============
//
//  Implementation of micro-scheduler.
//
//=============================================================================
//=============================================================================

#include <map>
#include <vector>

#include "errors.h"
#include "op_map.h"
#include "timing.h"
#include "tracing.h"

#include "op.h"
#include "dag.h"

#include "targ_isa_bundle.h"
#include "ipfec_options.h"
#include "msched_util.h"
#include "cggrp_microsched.h"
#include "cggrp_ptn_table.h"


#define CYCLE_MAX_LENGTH (ISA_MAX_SLOTS * ISA_MAX_ISSUE_BUNDLES)
typedef mINT16 OP_IDX;

enum { invalid_op_idx = -1, invalid_ptn_idx = -1 };
typedef enum {
    ptn1_better_than_ptn2,
    ptn1_ptn2_is_tie,
    ptn1_less_than_ptn2
} PTN_PRIORITY;


// ==================================================================
//
//   Class Name: CYCLE_STATE
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//      Each CYCLE_STATE object simulates the status of a cycle,
//      including the operations currently issued in this cycle,
//      issue ports assignment and group pattern selected.
//      We also provide simple sanity check when adding new 
//      operation.
//
//   Note:
//
// ==================================================================

enum CYCLE_FLAGS{
    CYCLE_FILL_NOP_A,  // When negotiate fail, insert nop after 
                       // the last stop bit
    CYCLE_FILL_NOP_B,  // Insert nop before the first stop bit
    CYCLE_STRICT_ORDER,// Operations in cycle can not change order
    CYCLE_HAS_BRANCH   // Branch op is in cycle
};

class CYCLE_STATE{
protected:
    BOOL  _valid;
    BOOL  _fRequire; // there's an op require to be the first in group
    BOOL  _lRequire; // there's an op require to be the last in group
    BOOL  _dependency_exist; // Inner cycle dependency exist
    BOOL _cyclic;    // used in swp bundling

    mINT16  _flags;  // Some flags of CYCLE; In direction: <---
                     // each bit is depicted in CYCLE_FLAGS.
       
    OP_IDX   _num_ops;     // Operations issued in this cycle;
    mINT16   _extra_slots; // OPs occupies more than one slot.
    GROUP_ASSEMBLE group_assemble; // Object for assemble in group;

    // Data structure is used to trace status of every op in cycle
    struct OP_ISSUE_STATE{
        OP   *op;
        ISSUE_PORT op_fu; // issue port assigned to this op
        PORT_SET set_op_fu;
	std::vector<OP_IDX> preds;  // predecessors within this cycle
    }     _ops[CYCLE_MAX_LENGTH];

    // function unit's owner op; _fu_owner[ip_M1] = 1 means that
    // ops[1].op has been issued to M1 
    std::vector <OP_IDX>   _fu_owner;
                                

    PORT_SET _reserve; // The bit vector used by current resource
    INT      _ptn;     // The current selected group pattern index

    OP_ISSUE_STATE& _OP_State(OP_IDX i)
    {
        Is_True(i<_num_ops, ("Invalid _ops[%d] access in cycle!", i));
        return _ops[i];
    }

    const OP_ISSUE_STATE& _const_OP_State(OP_IDX i) const
    {
        Is_True(i<_num_ops, ("Invalid _ops[%d] access in cycle!", i));
        return _ops[i];
    }
    void Init_Mem(void);

public:
    CYCLE_STATE(void);      // Constructor
    ~CYCLE_STATE(void);      // Destructor

    CYCLE_STATE& operator = (const CYCLE_STATE);    // Copyer

    // Over all status
    OP_IDX  Sum_OP(void) const { return _num_ops; }
    BOOL Empty(void) const { return Sum_OP() == 0; }
    BOOL Full(void)  const
    { return (Sum_OP()+_extra_slots) >= CYCLE_MAX_LENGTH;}

    BOOL Valid(void) const { return _valid; }
    BOOL Valid(BOOL val) { return _valid = val; }

    BOOL Risky(void) const
    {
        // Only for itanium:
        // IF the instruction issued in M0 dependend on others
        // eg. M0 depend on I0
        // perhaps there is only one partial pattern which fits this state.
        // eg. MM_I,MMI, later part of the stop bit is fitting the state
        // and no complete pattern does.
        if (PROCESSOR_Version == 2 && Strict_Order()) return FALSE;
        OP_IDX  op_index = OP_In_Port(ip_M0);
        if ( (op_index!=invalid_op_idx) && ((OP_Preds(op_index)).size()) ){
            return TRUE;
        }
        return FALSE;
    }

    BOOL Cyclic(void) const {return _cyclic;}
    BOOL Cyclic(BOOL val) { return _cyclic = val; }
    BOOL Require_First(void) const { return _fRequire; }
    BOOL Require_Last(void)  const { return _lRequire; }
    BOOL Is_Dependency(void) const { return _dependency_exist; }    
    BOOL Fill_After(void)    const { return _flags & (1 << CYCLE_FILL_NOP_A); }
    BOOL Fill_Before(void)   const { return _flags & (1 << CYCLE_FILL_NOP_B); }
    BOOL Strict_Order(void)  const { return _flags & (1 << CYCLE_STRICT_ORDER); }
    BOOL Branch_Exist(void)  const { return _flags & (1 << CYCLE_HAS_BRANCH); }
    BOOL Set_Flags(enum CYCLE_FLAGS order) {return _flags |= (1<<order); }

    BOOL Legality_Chk_Needed(void) const
    { return  _dependency_exist|| _fRequire|| _lRequire||(_extra_slots>0)
              ||Branch_Exist();}
    BOOL Legality_Chk(const PATTERN_TYPE ptn);

    PORT_SET  Reserve(void) const { return _reserve; }

    INT Current_Pattern(void) const { return _ptn; }
    INT Current_Pattern(INT p) { return _ptn = p; }

    GROUP_ASSEMBLE Assemble(void) {return group_assemble;}

    // Return this state's bundle template
    const PATTERN_TYPE Current_Pattern_Type(void) 
    { return Patterns()[Current_Pattern()]; }

    void Clear(void);

    // Issue ports or OP status
    OP_IDX  OP_In_Port(ISSUE_PORT port) const
    {   // return the op_idx issued to this port
        Is_True((port < ip_invalid),("Invalid Issue Port"));
        return _fu_owner[port];
    }

    // Given an op, return the issue port
    ISSUE_PORT OP_Issue_Port(OP_IDX op_index) const
    { return _const_OP_State(op_index).op_fu;}

    // Get_Issue_Port:Call by CGGRP_Get_Issue_Port()
    ISSUE_PORT Get_Issue_Port (OP * inst) {
        for (OP_IDX i=0;i<_num_ops;i++) {
            if (_ops[i].op == inst) { 
                return OP_Issue_Port(i);
            }
        }
    }

    // Set_Issue_Port:Call by CGGRP_Set_Issue_Port()
    void Set_Issue_Port (OP *inst,PORT_SET set_op_fu) {
        for (OP_IDX i=0; i<_num_ops; i++) {
            if (_ops[i].op == inst) {
                _ops[i].set_op_fu = _ops[i].set_op_fu - set_op_fu;
            }
        }
    }

    // Set 'op_index' issued in 'port'
    inline void OP_Issue_Port(OP_IDX op_index, ISSUE_PORT port);
    inline void Clear_OP_Issue_Port(INT op_index);

    OP * operator [](OP_IDX op_idx) const
    { return (_const_OP_State(op_idx)).op; }
    const std::vector<OP_IDX> OP_Preds(OP_IDX i) const
    { return (_const_OP_State(i)).preds; }

    OP_IDX Add_OP(OP * inst, ISSUE_PORT issue_port = ip_invalid);

    //functions are used to query the table provided by offline model
    BOOL Has_Valid_Patterns(void) const
    { return PTN_table.Is_Valid(_reserve.Body()); }
    const PTN_TABLE_LINE& Patterns(void) const
    { return PTN_table[_reserve.Body()]; }

    INT Get_B_Unit_Num(void);
    void Polish_Tail(void);
    void Do_Bundling(void);
    OP * Reorder_Branch_OP(void);

    // Give best issue order, failed return false.
    BOOL Best_Issue_Order(INT *issue_op_list,PATTERN_TYPE pattern);;
    BOOL Best_Issue_Order(INT *issue_op_list);;
    void Set_M_Unit (void);

    void Dump (FILE *f=stderr) ;  // Debug dump routine
#ifdef Is_True_On
    void gdb_dump(void) ;
#endif

};


///////////////////////////////////////////////////////
// Constructor
CYCLE_STATE::CYCLE_STATE(void)
{
    Init_Mem();
    Clear();
}

void CYCLE_STATE::Init_Mem(void)
{
    _fu_owner.clear();    
    for (INT j=0; j<ip_number; j++) {
        _fu_owner.push_back(invalid_op_idx);
    }
}
CYCLE_STATE::~CYCLE_STATE(void)
{
    _fu_owner.clear();
}
void CYCLE_STATE::Clear(void)
{
    _valid = FALSE;
    _fRequire = FALSE;
    _lRequire = FALSE;
    _cyclic = FALSE;
    _dependency_exist = FALSE;
    _flags = 0;

    _num_ops = 0;
    _extra_slots = 0;

    _reserve =0;

    for (OP_IDX i=0; i< CYCLE_MAX_LENGTH; i++){
        _ops[i].op = NULL;
        _ops[i].op_fu = ip_invalid;
        _ops[i].set_op_fu = 0;
        _ops[i].preds.clear() ;
    }
    

    for (INT j=0; j<ip_number; j++) {
        _fu_owner[j] = invalid_op_idx;
    }
}

// Copy Operator
CYCLE_STATE& CYCLE_STATE::operator=(const CYCLE_STATE cs)
{
    _valid = cs._valid;
    _fRequire = cs._fRequire;
    _lRequire = cs._lRequire;
    _cyclic = cs._cyclic;
    _dependency_exist = cs._dependency_exist;
    _num_ops = cs._num_ops;
    _extra_slots = cs._extra_slots;
    _flags = cs._flags;

    _reserve = cs._reserve;
    _ptn = cs._ptn;

    for (OP_IDX i=0; i< CYCLE_MAX_LENGTH; i++){
        _ops[i].op = cs._ops[i].op;
        _ops[i].op_fu = cs._ops[i].op_fu;
        _ops[i].set_op_fu = cs._ops[i].set_op_fu;
        _ops[i].preds = cs._ops[i].preds;
    }

    for (INT j=0; j<ip_invalid; j++){
        _fu_owner[j]= cs._fu_owner[j];
    }
    return *this; 
}

inline void CYCLE_STATE::OP_Issue_Port(OP_IDX op_index, ISSUE_PORT port)
{
    Is_True((port < ip_invalid),("Invalid Issue Port"));
    _OP_State(op_index).op_fu = port;
    _fu_owner[port] = op_index;
    _reserve = _reserve + port;

    if (Long_Instruction((*this)[op_index])){
        // Hacker on itanium MLX issue rule
        Is_True( F_PORTS.In(port), ("LX only issued to FI in Itanium!"));
        ISSUE_PORT extra_i = (ISSUE_PORT)(ip_I0 + (port-ip_F0));
        Is_True( !_reserve.In(extra_i),
                ("LX occupies Fx and Ix at the same time in Itanium"));
        _reserve = _reserve + extra_i;
    }
}

inline void CYCLE_STATE::Clear_OP_Issue_Port(INT op_index)
{
    ISSUE_PORT port = _OP_State(op_index).op_fu;
    if (port == ip_invalid){
        return;
    }

    _OP_State(op_index).op_fu = ip_invalid;
    _fu_owner[port]= invalid_op_idx;
    _reserve = _reserve - port;

    if (Long_Instruction((*this)[op_index])){
        // Hacker on itanium MLX issue rule
        Is_True( F_PORTS.In(port), ("LX only issued to FI in Itanium!"));
        ISSUE_PORT extra_i = (ISSUE_PORT)(ip_I0 + (port-ip_F0));
        Is_True(_fu_owner[extra_i] == invalid_op_idx,
               ("LX should occupies F0 and I0 at the same time in Itanium"));
        _reserve = _reserve - extra_i;
    }
}
/////////////////////////////////////////////////////////
//
//   Given a valid CYCLE STATE and the pattern it used, 
//   and the CYLCE has decided each op's issue port.   
//   Return the best postion of each op. issue_op_list is 
//   an array to record each slot is occupied by which op.
//
BOOL CYCLE_STATE::Best_Issue_Order(INT *issue_op_list)
{
   const PATTERN_TYPE ptn = Current_Pattern_Type();
   return Best_Issue_Order(issue_op_list, ptn);
}
BOOL CYCLE_STATE::Best_Issue_Order(INT *issue_op_list, PATTERN_TYPE ptn)
{
    ISSUE_PORT issue_port;
    BOOL is_first_pos=true; 
    BOOL group_start=false;
    BOOL issued[CYCLE_MAX_LENGTH] = {false, false};

    const DISPERSAL_TARG  *ports_vector = dispersal_table.Query(&ptn);
    for (INT bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++)
    {
        INT template_index = ptn[bundle];
        if (template_index == ISA_MAX_BUNDLES){
            // It's an invalid one
            break;
        } 
        for (INT slot=0; slot<ISA_MAX_SLOTS; slot++)
        {
            INT idx = bundle * ISA_MAX_SLOTS + slot; 
            issue_op_list[idx] = invalid_op_idx;
            if (ptn.start_in_bundle && !group_start && 
                ISA_EXEC_Stop(template_index, slot)) {
                group_start = true;
                continue;
            }
            if (!group_start && ptn.start_in_bundle) continue;

            // how to choose a suitable function unit is very important
            // to improve performance. We should choose a suitable
            // function unit for each slot. There is a rule that
            // the fontier slot is more compact than the follow slot.

            PORT_SET ports = ports_vector->Ports(bundle, slot);

            ISSUE_PORT issue_port = ports.First_IP();
            for (INT ip=issue_port; ip <= ip_invalid; ip++)
            {
                issue_port = (ISSUE_PORT)ip;
                if (issue_port == ip_invalid) break;
                if (!ports.In(issue_port)) continue;
                OP_IDX op_index = OP_In_Port(issue_port);
                if (op_index != invalid_op_idx) {
                if (_fRequire && is_first_pos
                    && !TOP_is_f_group(OP_code((*this)[op_index])) )
                    continue;
  
                is_first_pos = false;
                // try next available ip and not issued 
                if (issued[op_index] != true) {
                    // try issue op of which predecessor ops
                    // has been issued 
                    BOOL pred_issued = true;
                    for (INT m=0; m<(OP_Preds(op_index)).size(); m++){
                        OP_IDX pred = OP_Preds(op_index)[m];
                        if (!issued[pred]){
                            // One of the predecessors NOT issued
                            pred_issued = false;
                            break;
                        }
                    }
                    if (pred_issued) { break;}
                 }
             }
          }
          if (issue_port == ip_invalid) {
              issue_op_list[idx] = invalid_op_idx;
          }else {
              // Assign wrong issue port for the existed op
              // because of dependence
              if (!_reserve.In(issue_port)) {return false;}
              OP_IDX op_idx = OP_In_Port(issue_port);
              issue_op_list[idx] = op_idx;
              issued[op_idx] = true;
          }
      }
   }
   return true;
}
///////////////////////////////////////////////////////
// AddOp: Add a new instruction into this cycle.
// Do an integrity check before adding op.
// Return the index of op which newly added.


OP_IDX CYCLE_STATE::Add_OP( OP * inst,
                        ISSUE_PORT issue_port)
{
    // Record all this inst's predecessors in cur cycle
    std::vector<OP_IDX> preds_vector;

    Is_True(_num_ops < CYCLE_MAX_LENGTH,
        ("Adding too many operations in a cycle!"));

    if (OP_xfer(inst)) {
        if (Branch_Exist()) {
            // TODO: do some special check for multi_branch in global scheduler 
            // DevWarn("multiple issue op in BB %d", BB_id(OP_bb(inst)));
        } else {
            Set_Flags(CYCLE_HAS_BRANCH);
        }
    }
    if (IPFEC_sched_care_machine == Sched_care_bundle){
        if (TOP_is_f_group(OP_code(inst))){
            if (Require_First()){
                //Already there has one op required to be the first in a group
                return invalid_op_idx;
            }
            else{
                _fRequire = TRUE;
            }
        }
        if (TOP_is_l_group(OP_code(inst))) { 
            if (Require_Last()){
                //Already there has one op required to be the last in a group
                return invalid_op_idx;
            }
            else{
                _lRequire = TRUE;
            }
        }

        if (!Empty()) { //This is NOT the first op
            //Check the hazard and dependency between new ops and existing ones.
            //Set the flags accordingly
            if (Strict_Order()) {
                INT last_idx=0;
                for (OP_IDX op_idx=0; op_idx< Sum_OP(); op_idx++){
                     OP *exist_op = (*this)[op_idx];
                     if (Dependence_Between(inst,exist_op) && 
                         issue_port == ip_invalid)
                         return invalid_op_idx;
                     
                     // when non-branch op is added to cycle
                      // we should allow it skip branch op
                     if ((!OP_xfer(inst) || OP_chk(inst)) &&
                         (OP_xfer(exist_op) && !OP_chk(exist_op)) &&
                          OP_has_disjoint_predicate(inst,exist_op)) continue; 

                     preds_vector.push_back(op_idx);
                }
                _dependency_exist =true;

            } else {
                for (ARC_LIST *al = OP_preds(inst); al; al=ARC_LIST_rest(al)){
                    // For all the predcessors of this inst
                    ARC *arc = ARC_LIST_first(al);

                    // for SWP bundling
                    if (_cyclic && ARC_omega(arc)!=0) continue;
             
                    if (PostBranchArc(arc)) {
                        if (!OP_xfer(inst) || OP_chk(inst)) continue ; // br op also should mantain order
                    }

                    OP  *pred = ARC_pred(arc);
                    for (OP_IDX op_idx=0; op_idx< Sum_OP(); op_idx++){
                        OP *exist_op = (*this)[op_idx];
                        if (pred == exist_op){

                            if (!Dependence_Between(inst, exist_op)
                                || ARC_latency(arc)==0 || 
                                (_cyclic && (OP_scycle(inst)!=OP_scycle(exist_op)))) {
                                // not in one stage in SWP, can be put into one cycle
                                // Dependency Tolerated in a Cycle
                                _dependency_exist = true;
                                preds_vector.push_back(op_idx);
                            }
                            else{
                                return invalid_op_idx;
                            }
                        }
                    }
                }
            }
        }
    }

    // After checking passed, start insertion
    _ops[_num_ops].op = inst;
    _ops[_num_ops].op_fu = issue_port;
    _ops[_num_ops].preds = preds_vector;
    _ops[_num_ops].set_op_fu = TSI_Issue_Ports(OP_code(inst));
    _num_ops++;
    _extra_slots += Instruction_Length(inst) - 1;
    if (issue_port!= ip_invalid){
        OP_Issue_Port(_num_ops-1, issue_port);
    }
    return _num_ops-1;
}

/////////////////////////////////////////////////////////////////////
// Legality_Chk: Given a group pattern, check whether the operations
//     in this state can be successfully issued in this pattern without
//     violating dependency or other rules.
// Rough steps:
//   For each slot in the given pattern
//      If this is the first slot,
//          make sure that op issued here is the one that must be issued
//              first, if any.
//      Find out which issue port this slot should be issued to
//      Find out which op should be issued in this port,
//          which is the op that should be issued in this slot.
//      Find out whether all his predecessors have been issued
//          before this slot. If not, fail;
//   Make sure op last issued is the one that must be issued last,
//       if any
//   Success
//
// Parameters:
//   ptn:     the given group pattern.
BOOL CYCLE_STATE::Legality_Chk(const PATTERN_TYPE ptn)
{
    // use_tail: in the current ptn, we use the slots after the stop bit
    //           in a compressed template
    BOOL use_tail = ptn.start_in_bundle;

    BOOL issued[CYCLE_MAX_LENGTH]= {false, false};
    BOOL first_stop_hit = false;
    BOOL last_stop_hit = false;
    BOOL is_first_pos = true;
    BOOL has_issue_br_op = false;
    OP_IDX  op_index;
    ISSUE_PORT issue_port;
    const DISPERSAL_TARG  *ports_vector = dispersal_table.Query(&ptn);

#ifdef Is_True_On
    if (use_tail){
        //if use_tail, the first bundle must be a compressed one
        Is_True(ISA_EXEC_Stop_Mask(ptn[0])!=0,
            ("Invalid Compressed Template!"));
    }
#endif
    
    INT issue_op_list[CYCLE_MAX_LENGTH];
    if (!Best_Issue_Order(issue_op_list,ptn)) {
      //  printf("failed found issue order!");
        return FALSE;
    }
    for (INT bundle=0; bundle < ISA_MAX_ISSUE_BUNDLES; bundle++){
        INT template_index = ptn[bundle];
        if (template_index == ISA_MAX_BUNDLES){
            // It's an invalid one
            break;
        }
        for (INT slot=0; slot < ISA_MAX_SLOTS; slot++){
            if (use_tail && !first_stop_hit){
                //if use_tail, slots before stop bit should not be used.
                first_stop_hit = ISA_EXEC_Stop(template_index, slot);
            }
            else{
                ISA_EXEC_UNIT_PROPERTY slot_prop =
                       ISA_EXEC_Slot_Prop(template_index, slot);

                //Find out which op should be issued in this slot 
                op_index = issue_op_list[ bundle * ISA_MAX_SLOTS + slot ];
      
                if (op_index == invalid_op_idx){
                    // An NOP should be issued here
                    is_first_pos = FALSE;
                    last_stop_hit = ISA_EXEC_Stop(template_index, slot);
                    if (last_stop_hit) {
                        break;
                    } 
                    continue;
                } else {

                    issue_port = OP_Issue_Port(op_index); 
                    if (!Long_Instruction((*this)[op_index]) && 
                        !_ops[op_index].set_op_fu.In(issue_port)) { 
                        return FALSE;
                    }
                }

                Is_True( issued[op_index] == false,
                    ("OP issued twice in a cycle!"));

                if (_fRequire && !is_first_pos
                    && TOP_is_f_group(OP_code((*this)[op_index])) ){
                    //this op should have been the first in group
                    return false;
                }
                is_first_pos = false;

                if (Long_Instruction((*this)[op_index])){
                    // this is a LX slot op which occupies TWO slots
                    if (slot_prop != ISA_EXEC_PROPERTY_L_Unit){
                        // This op should have been issued in L slot!
                        return FALSE;
                    }
                }
                else {
                    if (slot_prop == ISA_EXEC_PROPERTY_L_Unit){
                    // This is NOT a long LX op and should NOT
                    // be issued to an L slot!
                        return FALSE;
                    }
                }

                // Now we have found the op that has been issued in this slot
                // Find out whether all his predecessors
                // have been issued before this slot;
                for (INT m=0; m<(OP_Preds(op_index)).size(); m++){
                    OP_IDX pred = OP_Preds(op_index)[m];
                    if (!issued[pred]){
                        // One of the predecessors NOT issued
                        return false;
                    }
                }
                issued[op_index] = TRUE;

                if (slot_prop == ISA_EXEC_PROPERTY_L_Unit){
                    // Processing an LX op, occupis TWO slots!
                    slot ++;
                    Is_True( slot < ISA_MAX_SLOTS,
                        ("MAX slots exceeded in Legality_Chk!"));
                    //Find out issue port of the next slot
                    issue_port = ports_vector->Port(bundle, slot);
                    Is_True(issue_port!=ip_invalid,
                        ("Invalid Pattern Found in Valid Pattern Table!"));

                    // Make sure this is the dumy X slot op which
                    // should NOT be occupied by any one!!!
                    if (OP_In_Port(issue_port) != invalid_op_idx){
                        // some one has occupied the X slot, so Fail!
                        return false;
                    }
                    issued[op_index] = TRUE;
                }

                // once meet branch op, after that we should issue other
                // except branch op or nop;
                if (Branch_Exist()) {
                    if (OP_xfer((*this)[op_index]) && 
                        !OP_chk((*this)[op_index])) {
                        has_issue_br_op = TRUE;
                    } else {
                        if (has_issue_br_op && !OP_noop((*this)[op_index]) )
                            return false;
                    }
                }
                last_stop_hit = ISA_EXEC_Stop(template_index, slot);
                if (last_stop_hit){
                    break;
                }
            }
        }
    }

    // now "op_index" must contain the last issued op
    if (_lRequire &&
        (op_index==invalid_op_idx ||
         !TOP_is_l_group( OP_code((*this)[op_index])))) {
        //the last issued should be the one that is required to be issued last
        return false;
    }
    // some ops cannot be issued, so it is illegal 
    for (INT I=0; I<Sum_OP(); I++){
        if (!issued[I]) {return false;}
    }

#ifdef Is_True_On
    // Debug checking integrity: whether all the ops have been issued
    for (INT I=0; I<Sum_OP(); I++){
        Is_True(issued[I]==true, ("Some op not checked in Legality Check!"));
    }
#endif

    return true;
}

/////////////////////////////////////////////////////////////////////
// Function: Polish_Tail 
//   It is to check the last cycle in a basic block. If it ends with
//   a compressed template, change it into a non-compressed one. Here
//   we use an assumption: whenever a bunch of instructions can be
//   issued in one cycle using compressed template, they can also be
//   issued in this cycle without using compressed template.
//
//   Note that we can NOT just remove the stop bit, fill the bundle
//   with nops, which will cause extra cycle sometimes.For instance,
//   the pattern is: MII, M_MI; If we just remove the stop bit and
//   fill up nops, the pattern will be changed to MII, MMI; For
//   we got only two M ports, so there will be a stop at the fourth
//   slot, and this group will take two cycles. We need to change it
//   into: MII, MFB; or something like that to avoid extra cycle.
//
void CYCLE_STATE::Polish_Tail(void)
{
    if (Empty()) return;

    const PTN_TABLE_LINE ptns = Patterns();
    const PATTERN_TYPE s_ptn = ptns[Current_Pattern()];

    INT good_ptn_idx = invalid_ptn_idx;
    INT probe_better_ptn_num = 0;

    for (INT ptn_idx=Current_Pattern(); ptn_idx<ptns.size; ptn_idx++) {
        const PATTERN_TYPE ptn = ptns[ptn_idx];

        if (ptn.end_in_bundle) {
        // This pattern stops IN a bundle,
            // does NOT end with a bundle's boundary
            continue;
        }

        if (ptn.start_in_bundle != s_ptn.start_in_bundle) {
            continue;
        }

        if (ptn.start_in_bundle && ptn[0]!= s_ptn[0]){
            continue;
        }
        if (Legality_Chk_Needed() &&
            !Legality_Chk(ptn)){
            continue;
        }

        Current_Pattern (ptn_idx);
        return;
    }

    Is_True (FALSE, 
        ("Fail to find a replacement for a compressed template!"));


    return;
}

/////////////////////////////////////////////////////////////////////
// Function: Do_Bundling
//   Sort the ops and insert nops according to Current_Pattern();
//   Watch out the compressed template when inserting nops!
//   Do NOT touch slots that NOT belong to this cycle
//   set the flag OpBundled of every op.
//   set the flag OpEndGroup of the last op;
void CYCLE_STATE::Do_Bundling(void)
{
    BOOL issued[CYCLE_MAX_LENGTH]= {false, false};
    BOOL first_stop_hit = false;
    BOOL last_stop_hit = false;
    BOOL is_first_pos = true;
    OP_IDX  op_index;

    if (Empty())  return;
    
    const PATTERN_TYPE ptn = Current_Pattern_Type();
    ISSUE_PORT issue_port;
    const DISPERSAL_TARG  *ports_vector = dispersal_table.Query(&ptn);
    
    // Here we got an assumption: 
    // ALL the OPs in this cycle should be in the SAME BB when
    // they got bundled. And all the OPs in this cycle should
    // be together.
    BB *bb = OP_bb((*this)[0]);
    OP *position = OP_prev((*this)[0]);
   
    INT issue_op_list[ CYCLE_MAX_LENGTH ];
    if (!Best_Issue_Order(issue_op_list)) {
        Is_True(FALSE, ("Can't find a suitable issue order!"));
    }

    for (INT bundle=0; bundle < ISA_MAX_ISSUE_BUNDLES; bundle++){
        INT template_index = ptn[bundle];
        OP  *op = NULL;
        UINT slot;

        if (template_index == ISA_MAX_BUNDLES){
            break;  // It's an invalid one
        }
        Is_True( !last_stop_hit,
                 ("Still bundle after stop bit!"));
        for (slot=0; slot < ISA_MAX_SLOTS; slot++){
            if ((Fill_Before() && !first_stop_hit)
                || (last_stop_hit && Fill_After())){
                // An NOOP should be issued here for negotiate failed.
                group_assemble.EmitNop(template_index, slot);
            }

            if (last_stop_hit && Fill_After()) continue; 
            if (ptn.start_in_bundle && !first_stop_hit) {
                first_stop_hit = ISA_EXEC_Stop(template_index, slot);
                continue;
            }
            
            ISA_EXEC_UNIT_PROPERTY slot_prop =
                       ISA_EXEC_Slot_Prop(template_index, slot);
                       
            //Find out which op should be issued in this slot 
            op_index = issue_op_list[bundle * ISA_MAX_SLOTS + slot];


            if (op_index == invalid_op_idx){
                // An NOOP should be issued here for no enough op.                
                OP *op = group_assemble.EmitNop(template_index, slot);               
            }
            else{
                // We should remove this op from BB, and sort it.
                op = (*this)[op_index];
                Is_True(op,
                        ("Invalid OP found in a cycle when do bundling!"));
                if(position == op) {
                    position = OP_prev(position);
                }

                BB_Remove_Op(bb, op);
                group_assemble.EmitOp(op, template_index, slot);
                                
                issued[op_index] = true;
                if (Long_Instruction(op)){
                    // It occupies TWO slots! skip the next slot
                    slot ++;
                    Is_True( slot < ISA_MAX_SLOTS,
                             ("MAX slots exceeded in bundling!"));
                    Is_True( slot_prop == ISA_EXEC_PROPERTY_L_Unit,
                             ("Long Instruction should issued in LX slots!"));
                    issue_port = ports_vector->Port(bundle, slot);
                    Is_True(issue_port!=ip_invalid,
                            ("Invalid Pattern Found in Valid Pattern Table!"));
                    op_index = OP_In_Port(issue_port);
                    Is_True(op_index == invalid_op_idx,
                            ("X slot occupied by other ops!"));
                }
            }
        
            last_stop_hit = ISA_EXEC_Stop(template_index, slot);
            if (last_stop_hit){
                // Negotiate failed, insert nop to compressed template
                // otherwise; you can end the bundling;
                if (!Fill_After())  { break;}
            }
        } // for each slot
    } // for each bundle in this pattern

    // the last op is the end of group
    group_assemble.Group_End();
    group_assemble.InsertAfter(bb,position);

#ifdef Is_True_On
    // Debug checking integrity: whether all the ops have been issued
    for (INT I=0; I<Sum_OP(); I++){
        Is_True( issued[I]==true, ("Some op not issued in bundling!"));
    }
#endif

    return;
}

/////////////////////////////////////////////////////////////////////
// Function: Set_M_Unit 
//
// TODO: finish this comment
void
CYCLE_STATE::Set_M_Unit (void) {
    
    for (ISSUE_PORT p=ip_M0; M_PORTS.In(p); p=(ISSUE_PORT)(p+1)) {
        OP_IDX op_idx = _fu_owner[p];
        if (op_idx == invalid_op_idx) continue;

        OP* op = _ops[op_idx].op;
        if (EXEC_PROPERTY_is_I_Unit(OP_code(op)) &&
            EXEC_PROPERTY_is_M_Unit(OP_code(op))) {
            Set_OP_m_unit(op);
        }
    }
}

// This should be used when micro-scheduler finish one BB. 
// Because Scheduler schedule other op after branch op, 
// But the issue order should be kept. branch op is the 
// last op in this BB. Then, the individual bundling 
// phase will do right thing. 
// This function move branch op to be last op when 
// there are more ops scheduled after issue branch op.
// return branch op else NULL;
OP *
CYCLE_STATE::Reorder_Branch_OP (void)
{
    OP_IDX op_idx;
    OP *op=NULL;
    if (!Branch_Exist()) return NULL; 
    INT last_idx = Sum_OP() - 1;
    for (op_idx=0; op_idx< last_idx; op_idx++){
        op = (*this)[op_idx];
        if (OP_xfer(op) && !OP_chk(op)) {
           break; 
        }
    }
    if(op_idx < last_idx) {
        BB *bb = OP_bb(op);
        BB_Remove_Op(bb, op);
        mUINT16 old_map_idx = OP_map_idx(op);
        BB_Append_Op(bb, op);
        op->map_idx = old_map_idx;
        return op;
    }
    return NULL; 
}
// End definition of class CYCLE_STATE
/////////////////////////////////////////////////////////////////////

//To trace states of current and previous cycle, and a temp one.
static CYCLE_STATE prev_state, cur_state, temp_state;

///////////////////////////////////////////////////////////////
// Below is the algorithm of the interfaces:
// CGGRP_Issue_OP, CGGRP_Cycle_Advance, CGGRP_End_BB

BOOL CGGRP_Cycle_Full(void)
{
    if (!LOCS_Enable_Bundle_Formation){
        return (!cur_state.Empty());
    }
    return cur_state.Full();
}

/////////////////////////////////////////////////////////////////////
// Function: CGGRP_Issue_OP,
//   Scheduler calls this function to query whether an op can be issued
//   in the current cycle, and if commit is true, the issue will be
//   committed.
// Rough Steps:
//   If this cycle is full (6 ops already) return fail;
//   Add this op into current cycle:
//     (CYCLE_STATE class will do miscellaneous checking now,
//         as dependency, etc.)
//   Call Bundle_Helper to find out an issue port and valid group pattern
//       for this new op.

inline BOOL valid_ptn_found(BOOL commit)
{    // inline function called in CGGRP_Issue_OP
    temp_state.Valid(TRUE);
    if (commit){
        cur_state = temp_state;
        temp_state.Valid(FALSE);
    }
    return true;
}
inline BOOL invalid_ptn(void)
{    // inline function called in CGGRP_Issue_OP
    Is_True(!(temp_state.Empty()),
        ("Rejecting OP with an empty cycle!!"));
    temp_state.Valid(FALSE);
    return false;
}

// Since we are going to do op reordering with recursive algorithm,
// we must ensure that the algorithm WILL stop. The bit vector below
// is for this purpose. Whenever we remove an op from its issue port
// and put the new comer in, the port will be marked and further
// reordering related to this port will be avoided in the following
// iterations.
// Only used in Bundle_Helper and checked in CGGRP_Issue_OP
static PORT_SET port_mark = 0;

static BOOL Bundle_Helper(OP_IDX new_op);


BOOL CGGRP_Issue_OP(OP * inst, BOOL commit)
{
    std::vector<OP_IDX> deps;
    OP_IDX op_index;
    
    if (IPFEC_sched_care_machine == Sched_care_nothing)
        return TRUE;

    PORT_SET issuable_ports = TSI_Issue_Ports(OP_code(inst));
    if (issuable_ports.Body() == 0){
        if (IPFEC_sched_care_machine < Sched_care_bundle){
            return TRUE;
        }
        if (commit && cur_state.Empty()){
            // Force new bundle for dummy instructions
            CGGRP_Force_Bundle_End();
        }
        return cur_state.Empty(); // Nothing get done on dummy instructions
    }

    MSCHED_Real_OP(inst);

    if (!LOCS_Enable_Bundle_Formation){
        // Here we just do one-cycle-one-op
        // No bundling
        if (!cur_state.Empty())
            return FALSE;
        if (cur_state.Add_OP(inst)== invalid_op_idx){
            cur_state.Clear();
            return FALSE;
        }
        if (!commit)
            cur_state.Clear();
        return TRUE;
    }

    if (cur_state.Full()) return FALSE;
    if (temp_state.Valid() && temp_state[temp_state.Sum_OP()-1]==inst){
        // use the result of the successful query just now
        return valid_ptn_found(commit);
    }

    temp_state = cur_state;
    temp_state.Current_Pattern(invalid_ptn_idx);
    op_index = temp_state.Add_OP(inst);
    if (op_index == invalid_op_idx){
        return invalid_ptn(); //fail to pass misc checking
    }
    if (IPFEC_sched_care_machine == Sched_care_width){
        return valid_ptn_found(commit);
    }

    Is_True( port_mark.Body()==0,
        ("Recursive Cleanup malfunctioning in Bundle_Helper!"));

    if (Bundle_Helper(op_index) ){
        return valid_ptn_found(commit);
    }
    // Fail to find valid pattern
    return invalid_ptn();
}

/////////////////////////////////////////////////////////////////////
// Function: Bundle_Helper
//   It is the routine that finds the valid issue ports and group
//   pattern recursively. It uses the temp state as the workspace.
// General Steps:
//  For each spare port that the new op can be issued to
//    Issue the op into this port to
//           form a new function unit request.
//    For each group pattern of this request
//      If ops in current cycle issued with this pattern do not
//              violate the dependency
//        Return success;
//  For each occupied port that the new op can be issued to
//    Take out the old op that occupies this port;
//    Put the new op in
//    Recursively call Bundle_Helper with the old op as parameter.
//    If the return value is successful, return success
//    Take out new op in this port
//    Put the old one in
//  Return Fail

static BOOL Bundle_Helper(OP_IDX new_op)
{
    PORT_SET issuable_ports = TSI_Issue_Ports(OP_code(temp_state[new_op]));

    BOOL deal_with_LX = FALSE; // Deal with itanium LX slot op

    if ( Long_Instruction(temp_state[new_op]) ){
        // This is a LX op, according to Itanium document,
        // MLX issued to MFI corresponding ports!
        deal_with_LX = TRUE;
        issuable_ports = F_PORTS;
    }

    if ( (INT)(issuable_ports-port_mark) == 0){
        // No port to issue
        return FALSE;
    }

    PORT_SET available_ports = issuable_ports
                               - (temp_state.Reserve())
                               - port_mark;

    INT i;
    for (i=0; issue_port_seq[i]<ip_invalid; i++){
        if (!available_ports.In(issue_port_seq[i])){
              continue;
        }

        if (deal_with_LX){
            // Hacker on MLX long instructions processing!
            // Make sure both F and I ports are availabe!
            Is_True( F_PORTS.In(issue_port_seq[i]),
                     ("LX only issued to FI in Itanium!"));
            ISSUE_PORT extra_i = (ISSUE_PORT)(ip_I0+(issue_port_seq[i]-ip_F0));
            if (temp_state.Reserve().In(extra_i))
                continue;
        }

        Is_Violate_Issue_Rule(temp_state[new_op], issue_port_seq[i]);
    
        temp_state.OP_Issue_Port(new_op, issue_port_seq[i]);
        if (IPFEC_sched_care_machine!=Sched_care_bundle){
            return TRUE;
        }

        if (!temp_state.Has_Valid_Patterns()){
            // do NOT have valid templates
            temp_state.Clear_OP_Issue_Port(new_op);
            continue;
        }

        const PTN_TABLE_LINE ptns = temp_state.Patterns();

        for (INT ptn_idx=0; ptn_idx<ptns.size; ptn_idx++){
            if (!IPFEC_Enable_Compressed_Template){
                if (ptns[ptn_idx].start_in_bundle ||
                    ptns[ptn_idx].end_in_bundle)
                    continue;
            }

            if (prev_state.Empty() && ptns[ptn_idx].start_in_bundle){
                continue;
            }

            if ((!temp_state.Legality_Chk_Needed())
               || (temp_state.Legality_Chk(ptns[ptn_idx]))){

                if ( temp_state.Risky() && ptns[ptn_idx].start_in_bundle ){
                    // M0 depend on other instructions while only a partial
                    // pattern found. Not certain whether there is an complete
                    // pattern fits
                    continue;
                }
                if (temp_state.Current_Pattern() == invalid_ptn_idx) {
                    temp_state.Current_Pattern(ptn_idx); 
                    return TRUE;
                }
            }

        }

        temp_state.Clear_OP_Issue_Port(new_op);
    }

    if (deal_with_LX){
        // Do NOT do reording when dealing with long instructions
        return FALSE;
    }

    if (IPFEC_sched_care_machine!=Sched_care_bundle) {
       // Do Not do reording when it didn't care bundling; 
	return FALSE;
    }
    // now reordering of issue ports needed
    PORT_SET reorder_ports = issuable_ports - port_mark - available_ports;

    for (i=0; issue_port_seq[i]<ip_invalid; i++){
        if (!reorder_ports.In(issue_port_seq[i])){
            continue;
        }
        OP_IDX old_op = temp_state.OP_In_Port(issue_port_seq[i]);

        if (old_op == invalid_op_idx){
            // Perhaps a LX Xslot place holder encountered
            Is_True( I_PORTS.In(issue_port_seq[i]),
                ("Invalid op_index when reordering old ops"));
            continue; // can NOT move X slot without touching L slot
        }

        temp_state.Clear_OP_Issue_Port(old_op);
        temp_state.OP_Issue_Port(new_op, issue_port_seq[i]);
        port_mark = port_mark + issue_port_seq[i];

        if (Bundle_Helper(old_op)){
            port_mark = port_mark - issue_port_seq[i];
            return TRUE;
        }

        port_mark = port_mark - issue_port_seq[i];
        temp_state.Clear_OP_Issue_Port(new_op);
        temp_state.OP_Issue_Port(old_op, issue_port_seq[i]);
    }

    // reorder method 2
    for (i= temp_state.Sum_OP()-1 ; i>=0 ; i--) {
        if (EXEC_PROPERTY_is_I_Unit(OP_code(temp_state[new_op])) && 
            EXEC_PROPERTY_is_M_Unit(OP_code(temp_state[new_op]))) { 
            break; // reorder has been done in the above process
        }
        if (i == new_op) continue;
        OP *op = temp_state[i]; 
        if (EXEC_PROPERTY_is_I_Unit(OP_code(op)) &&
            EXEC_PROPERTY_is_M_Unit(OP_code(op))) {
            ISSUE_PORT issue_port = temp_state.OP_Issue_Port(i);
            if (port_mark.In(issue_port)) break;
            
            if (M_PORTS.In(issue_port) && 
                (I_PORTS-temp_state.Reserve()) != 0) {
                temp_state.Clear_OP_Issue_Port(i);
                temp_state.OP_Issue_Port(i,(I_PORTS-temp_state.Reserve()).First_IP());
            } else  if (I_PORTS.In(issue_port) && 
                (M_PORTS-temp_state.Reserve()) != 0) {
                temp_state.Clear_OP_Issue_Port(i);
                temp_state.OP_Issue_Port(i,(M_PORTS-temp_state.Reserve()).First_IP());
            }
            if (issue_port == temp_state.OP_Issue_Port(i)) break; 
            port_mark = port_mark + issue_port + temp_state.OP_Issue_Port(i); 
            
            if (Bundle_Helper(new_op)) {
                port_mark = port_mark - issue_port - temp_state.OP_Issue_Port(i); 
                return TRUE;
            }
            port_mark = port_mark - issue_port - temp_state.OP_Issue_Port(i); 
            temp_state.Clear_OP_Issue_Port(i);
            temp_state.OP_Issue_Port(i,issue_port);
            break;
        }
    }
    return FALSE;
}

//////////////////////////////////////////////////////////////////////
// Function CGGRP_Get_Issue_Port()
//  Scheduler calls this function to get the op's issue port

ISSUE_PORT CGGRP_Get_Issue_Port(OP *inst)
{
    return cur_state.Get_Issue_Port(inst);
}

/////////////////////////////////////////////////////////////////////
// Function CGGRP_Set_Issue_Port()
//  Scheduler calls this function to tell the micro-scheduler which ports 
//  the specified op should be issued to

void CGGRP_Set_Issue_Port( OP *inst,PORT_SET set_op_fu)
{
    cur_state.Set_Issue_Port(inst,set_op_fu);
}

//////////////////////////////////////////////////////////////////////
// Function CGGRP_Cycle_Advance()
//
// Scheduler calls this function to tell us that there is no more op
// to be issued in this cycle, go on. And we should do some cleanup,
// including:
// 1. If we care about compressed template:
//   a) Check the current and the previous cycle to see whether they can
//      share a compressed template.
//   b) Bundle the previous cycle according to the pattern selected,
//      which means order the operations and insert noops.
//   c) Copy current cycle state to previous cycle state, clear
//      current cycle state and the temp state.
// 2. And if we don't care about compressed template
//   a) Bundle the operations in current cycle.
//   b) Clear the current cycle state and the temp state.
static void Negotiate_Bundle(void);

void CGGRP_Cycle_Advance(void)
{
    if (cur_state.Empty()){
        cur_state.Clear();
        temp_state.Clear();
        return;
    }

    if (!LOCS_Enable_Bundle_Formation){
        // Do one-op-per-cycle, No bundling!
        Is_True(cur_state.Sum_OP()==1,
           ("More than one op found in one-op-per-cycle!"));
        Set_OP_end_group(cur_state[0]);

        // Now care for simulated op converstion
        // Architectural hardcode here.
        // Hope I got forgived, for we ONLY got here when we DO NOT
        // do bundling!!!!
        if (OP_simulated(cur_state[0])){
            MSCHED_Convert_Simulated(cur_state[0],
                      ISA_EXEC_PROPERTY_I_Unit);
        }
        cur_state.Clear();
        temp_state.Clear();
        return;
    }

    if (IPFEC_sched_care_machine!=Sched_care_bundle){        
        if (cur_state.Valid()&& !cur_state.Empty()){
            OP *br_op = cur_state.Reorder_Branch_OP();
            if (br_op == NULL) 
                Set_OP_end_group(cur_state[cur_state.Sum_OP()-1]);
            else
                Set_OP_end_group(br_op);
        }
        cur_state.Clear();
        temp_state.Clear();
        return;
    }

    if (IPFEC_Enable_Compressed_Template){
        if (!prev_state.Empty()){
            //Negotiation between prev and cur state, on compress template
            Negotiate_Bundle();
            prev_state.Do_Bundling();
        }
        prev_state = cur_state;
    }
    else{
        cur_state.Do_Bundling();
    }

    cur_state.Set_M_Unit ();

    cur_state.Clear();
    temp_state.Clear();
}

//////////////////////////////////////////////////////////////////////
// Function CGGRP_Force_Bundle_End()
//   Scheduler calls this function to tell us not to consider
//   compressed template sharing between the just finished cycle and
//   the newly started cycle.

void CGGRP_Force_Bundle_End(void)
{
    CGGRP_Cycle_Advance();

    if (!LOCS_Enable_Bundle_Formation){
       // Do one-op-per-cycle, No bundling!
        return;
    }

    if (!IPFEC_Enable_Compressed_Template){
        return;
    }

    prev_state.Polish_Tail();
    prev_state.Do_Bundling();
    prev_state.Clear();
}

//////////////////////////////////////////////////////////////////////
// Below are the local functions:

/////////////////////////////////////////////////////////////////////
// Function: Negotiate_Bundle
//   This function adjusts the group patterns of prev and the current
//   cycle to find a compressed template that can be shared
//   between them.
// Rough Steps:
//  For each pattern of the previous cycle's function units request (prev_ptn)
//    For each pattern of the current cycle's function units request (cur_ptn)
//        If (the last bundle of prev_ptn == the first bundle of cur_ptn)
//          and it's a compressed template, 
//          and current cycle uses the slots after the stop bit
//            Check legality of prev_ptn in the previous cycle
//            set prev_ptn as previous cycle's selected pattern
//            Check cur_ptn valid in the current cycle
//            set cur_ptn as the current cycle's selected pattern
//            return;
//        if prev_ptn end with a bundle's boundary
//          and cur_ptn start with a bundle's boundary
//            Check legality of prev_ptn in the ops in previous cycle
//            set prev_ptn as previous cycle's selected pattern
//            Check legality of cur_ptn in the current cycle
//            set cur_ptn as current cycle's selected pattern
//            return;
//  Failed if we get here.

static void Negotiate_Bundle(void)
{
    // Patterns which has been checked to be invalid in the current cycle,
    // used to avoid repeatingly checking.
    std::vector <BOOL> dep_chk_fail;
    
    for (INT i=0; i< MAX_PTN_TABLE_LINE_SIZE; i++) {
        dep_chk_fail.push_back(false);
    }

    Is_True(!prev_state.Empty() && !cur_state.Empty(),
        ("No Need to Call Negotiate Bundle!"));

    const PTN_TABLE_LINE prev_cycle_ptns = prev_state.Patterns ();
    const PTN_TABLE_LINE cur_cycle_ptns = cur_state.Patterns();
    const PATTERN_TYPE s_ptn = prev_cycle_ptns[prev_state.Current_Pattern ()];

    for (INT prev_ptn_idx = prev_state.Current_Pattern(); 
         prev_ptn_idx < prev_cycle_ptns.size ; prev_ptn_idx ++) {

        const PATTERN_TYPE prev_ptn = prev_cycle_ptns[prev_ptn_idx];
        INT tail_template_in_prev_ptn;
        INT m;

        for (m=ISA_MAX_ISSUE_BUNDLES-1; m>=0; m--){
            if (prev_ptn[m]!=ISA_MAX_BUNDLES)
                break;
        }

        Is_True((prev_ptn[m]!=ISA_MAX_BUNDLES),
            ("Empty pattern in pattern table!"));
        tail_template_in_prev_ptn = prev_ptn[m];

        if (prev_ptn.start_in_bundle && prev_ptn[0] != s_ptn[0]){
            continue;
        }
       
        // First template in group should be same
        if (s_ptn.start_in_bundle &&   
            (!prev_ptn.start_in_bundle || prev_ptn[0] != s_ptn[0])) {
            continue; 
        }
            
        if (prev_state.Legality_Chk_Needed()){
            if (!prev_state.Legality_Chk(prev_ptn)){
                continue;
            }
        }

        for (INT cur_ptn_idx = cur_state.Current_Pattern();
             cur_ptn_idx<cur_cycle_ptns.size; cur_ptn_idx++){
            const PATTERN_TYPE cur_ptn = cur_cycle_ptns[cur_ptn_idx];

            if (!prev_ptn.end_in_bundle){
                // Prev cycle do NOT end with a compressed template.
                // We need to ensure current cycle does NOT start
                // with compressed template
                if (cur_ptn.start_in_bundle){
                    continue;
                }
            }
            else{
                // Now the prev cycle end with a compressed template
                // We need to ensure the first bundle in current cycle
                // is also this one.
                if (!cur_ptn.start_in_bundle){
                    continue;
                }
                if (cur_ptn[0] != tail_template_in_prev_ptn){
                    //prev_ptn's last bundle
                    continue;
                }
            }
            if (cur_state.Legality_Chk_Needed()){
                if (dep_chk_fail[cur_ptn_idx]){
                    //this group pattern has been checked to be failed
                    continue;
                }
                if (!cur_state.Legality_Chk(cur_ptn)){
                    dep_chk_fail[cur_ptn_idx]=true;
                    continue;
                }
            }

            prev_state.Current_Pattern(prev_ptn_idx);
            cur_state.Current_Pattern(cur_ptn_idx);
            return; // now we successfully find a negotiation

        }
    }
   
    dep_chk_fail.clear();
    PATTERN_TYPE ptn_t_prev = prev_state.Current_Pattern_Type();
    PATTERN_TYPE ptn_t_cur  = cur_state.Current_Pattern_Type();

    // When negotiate fails, set flag to notice Do_Bundling()
    // to add nop in appropriate order
    if ( ptn_t_prev.end_in_bundle ) {
        prev_state.Set_Flags(CYCLE_FILL_NOP_A);
    }
    if ( ptn_t_cur.start_in_bundle ) {
        cur_state.Set_Flags(CYCLE_FILL_NOP_B);
    }
    DevWarn(("Two valid cycles with a failed group pattern negotiation!"));
    return;
}
////////////////////////////////////////////////////////////////////
// Function: Calculate_BB_Cycle
//   Caculate bb_cycle after bundling in level 0-2 of micro-scheduler;
//   set bb_cycle field according to result, which can be displayed in
//   assemble code; 
////////////////////////////////////////////////////////////////////
void Calculate_BB_Cycle(BB *bb, BOOL dag_exist = TRUE)
{
    if (!BB_length (bb)) { BB_cycle(bb) = 0; return;};
    
    DAG_BUILDER dag (bb,NULL);

    if (dag_exist == FALSE) {
        dag.Build_DAG ();
    }

    mUINT16  * op_start_cycle =
        TYPE_ALLOCA_N (mUINT16,BB_next_op_map_idx (bb));

    OP * op ;
    FOR_ALL_BB_OPs (bb, op) {
        op_start_cycle[OP_map_idx(op)] = 1 ;
    }

   
    INT32 grp_start_cycle = 1 ;
    OP  * grp_first_op = BB_first_op(bb) ;

    FOR_ALL_BB_OPs (bb, op) { 
        if (!OP_noop(op)) {

            for (ARC_LIST * arcs = OP_preds (op) ; arcs != NULL;
                arcs = ARC_LIST_rest(arcs)) {

                ARC * arc = ARC_LIST_first(arcs);
                OP * pred = ARC_pred (arc);

                INT32 expected_cycle =
                    op_start_cycle[OP_map_idx(pred)] + ARC_latency(arc);

                op_start_cycle[OP_map_idx(op)] =
                    (op_start_cycle[OP_map_idx(op)] > expected_cycle) ?
                    op_start_cycle[OP_map_idx(op)] :
                    expected_cycle ;

            }

            grp_start_cycle = (grp_start_cycle > op_start_cycle[OP_map_idx(op)]) ?
                           grp_start_cycle : op_start_cycle[OP_map_idx(op)] ;
        }

        if (OP_end_group(op)) {
            for ( ; grp_first_op != op ; grp_first_op = OP_next(grp_first_op)) {
                    op_start_cycle [OP_map_idx(grp_first_op)] = grp_start_cycle ;
                OP_scycle(grp_first_op) = grp_start_cycle - 1; 
            }

            op_start_cycle [OP_map_idx(op)] = grp_start_cycle ;
            OP_scycle(grp_first_op) = grp_start_cycle - 1; 

            grp_first_op = OP_next(op);
            grp_start_cycle ++ ;
        }
    }

    if (dag_exist == FALSE) {
        Delete_Graph(bb);
    }

    BB_cycle(bb) = grp_start_cycle - 1 ;
}
////////////////////////////////////////////
//  Function: CGGRP_Bundle_BB
//    A simple method to simulate bundling phase of
//    decoupled scheduler for each BB.
////////////////////////////////////////////
static void CGGRP_Bundle_BB(BB *bb)
{
    extern void Clean_Up(BB* bb);

    // Clean_Up(bb);
    DAG_BUILDER bb_dag(bb);
    bb_dag.Build_DAG();

    for (OP *op = BB_first_op(bb); op != NULL; ){
        if (CGGRP_Issue_OP(op, FALSE)){ // not commit
            CGGRP_Issue_OP(op, TRUE);
            if (OP_end_group(op)) {
	       op = OP_next(op);
               CGGRP_Cycle_Advance();
            } else {
               OP *prev_op = op;
               op = OP_next(op);

               // OSP
               // Force to start a new cycle for the code in new line
               //   to get better debugability at -g
               if ( Debug_Level > 0 && op != NULL &&
                    OP_srcpos(op) != OP_srcpos(prev_op) )
                   CGGRP_Cycle_Advance(); 
	    }
        }
        else{
            CGGRP_Cycle_Advance();
        }
    }
    CGGRP_End_BB();
    Delete_Graph(bb);
    Calculate_BB_Cycle(bb,FALSE);
}

void CGGRP_Bundle(void)
{
    Is_True(IPFEC_sched_care_machine!=Sched_care_bundle,
            ("Bundling Shouldn't called after integerated scheduling done!"));
    Set_Error_Phase ("Ipfec Bundling");
    Start_Timer(T_Ipfec_LOCS_CU);

    INT32 save = IPFEC_sched_care_machine;
    IPFEC_sched_care_machine = Sched_care_bundle;
    prev_state.Clear();
    cur_state.Clear();

    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
        if (BB_length(bb) == 0) continue;
        CGGRP_Bundle_BB(bb);
        Set_BB_scheduled(bb);
    }

    IPFEC_sched_care_machine = save;
    Stop_Timer(T_Ipfec_LOCS_CU);
} // end of CGGRP_Bundle 


BOOL CGGRP_Check_Split_BB(BB* split_bb, BB** end_bbp)
{

    Is_True(BB_chk_split(split_bb), ("CGGRP_Check_Split_BB: split_bb must be a chk split BB!\n"));

    if(BB_length(split_bb) == 0){
        *end_bbp = split_bb;
        return FALSE;
    }
    
    BOOL no_touch = TRUE;
    BB* end_bb;
    BB* bb = split_bb;

    while(BB_chk_split(bb)){
        if(!BB_scheduled(bb))
            no_touch = FALSE;
        BB* next_bb = BB_next(bb);
        if(!BB_chk_split(next_bb))
            break;
        OP* roof  = BB_last_op(bb);
        OP* floor = BB_first_op(next_bb);
        if(roof  && OP_end_group(roof) &&
           floor && OP_start_bundle(floor))
            break;
        bb = next_bb;
    }

    *end_bbp = end_bb = bb;   
    if(!no_touch){
        return FALSE;
    }
    
    bb = split_bb;  
    for (OP* op = BB_first_op(bb);;) {
        ISA_BUNDLE bundle;
        UINT64 slot_mask;
        UINT stop_mask;
        INT slot;
        INT ibundle;

        //Gather up the OPs for the bundle.
         
        stop_mask = 0;
        slot_mask = 0;
        for (slot = 0; op && slot < ISA_MAX_SLOTS; ) {
            INT words;
            INT w;
            
            if(!OP_dummy(op)){ 
                if (OP_simulated(op)) 
                    return FALSE;    
                if(slot == 0){
                    Set_OP_start_bundle(op);
                }else{
                    Reset_OP_start_bundle(op);
                }
                words = Instruction_Length(op);
                for (w = 0; w < words; ++w) {
                    if (slot >= ISA_MAX_SLOTS)  
                        return FALSE;    
                    slot++;
                    slot_mask = slot_mask << ISA_TAG_SHIFT;
                    if ( EXEC_PROPERTY_is_M_Unit(OP_code(op)) && 
                         EXEC_PROPERTY_is_I_Unit(OP_code(op)) ){
                         slot_mask |= OP_m_unit(op) ? ISA_EXEC_PROPERTY_M_Unit : ISA_EXEC_PROPERTY_I_Unit;
                    } else { 
                         slot_mask |= ISA_EXEC_Unit_Prop(OP_code(op)); 
                    }
                    stop_mask = stop_mask << 1;
                }
                if (TSI_Issue_Ports(OP_code(op)).In(ip_B0) &&
                    TSI_Issue_Ports(OP_code(op)).In(ip_B2)){
                    slot_mask = slot_mask | ISA_EXEC_PROPERTY_B_Unit;
                }
                stop_mask |= (OP_end_group(op) != 0);
            }
            op = OP_next(op);
            if(op == NULL){
                bb = BB_next(bb);
                while(bb != BB_next(end_bb)){
                    if(BB_length(bb) == 0){
                        bb = BB_next(bb);
                        continue;
                    }
                    op = BB_first_op(bb);
                    break;
                }
            }
        }

        if (slot == 0){
            return TRUE;
        }    
        
        if (slot != ISA_MAX_SLOTS)
            return FALSE;    
    
        for (ibundle = 0; ibundle < ISA_MAX_BUNDLES; ++ibundle) {
            UINT64 this_slot_mask = ISA_EXEC_Slot_Mask(ibundle);
            UINT32 this_stop_mask = ISA_EXEC_Stop_Mask(ibundle);
            if ((slot_mask & this_slot_mask) == this_slot_mask && (stop_mask & ~1) == this_stop_mask) 
                break;
        } 
        if (ibundle == ISA_MAX_BUNDLES)
            return FALSE;    
    }
    Is_True(FALSE,("CGGRP_Check_Split_BB: Can not get to here!"));

}

inline BOOL bundle_ops_fail(void){
    prev_state.Clear();
    temp_state.Valid(FALSE);
    return FALSE;
}
/////////////////////////////////////////////////////////////////
//
//  Find_Pattern_OPS
//    find pattern for complete group in OPS: one bundle or two.
//    stop_idx indicate where is the stop bit.
//    
//    return a valid PATTERN_TYPE else invalid pattern.
//
static PATTERN_TYPE 
Find_Pattern_OPS(OPS *ops, INT stop_idx=0)
{
    OP *cur_op;
    INT idx=0;
    UINT64 slot_mask=0;
    UINT64 stop_mask=0;
    PATTERN_TYPE origin_ptn;

    // Init origin pattern;
    for(INT bundle=0; bundle < ISA_MAX_ISSUE_BUNDLES; bundle++)
    {
        origin_ptn.bundle[bundle] = ISA_MAX_BUNDLES;
        origin_ptn.start_in_bundle = 0;
        origin_ptn.end_in_bundle = 0;
    } 

    FOR_ALL_OPS_OPs(ops, cur_op) {
        INT words = Instruction_Length(cur_op);
        for (INT w = 0; w < words; ++w) {
             idx++;
             slot_mask = slot_mask << ISA_TAG_SHIFT;
             if ( EXEC_PROPERTY_is_M_Unit(OP_code(cur_op)) && 
                  EXEC_PROPERTY_is_I_Unit(OP_code(cur_op)) ){
                 
                // A type instruction, identify its property
                if (idx >= ISA_MAX_SLOTS) {
                    slot_mask |= ((idx-1)%ISA_MAX_SLOTS==2) ? ISA_EXEC_PROPERTY_I_Unit : ISA_EXEC_PROPERTY_M_Unit;
                    slot_mask |= ((idx-1)%ISA_MAX_SLOTS==0) ? ISA_EXEC_PROPERTY_M_Unit : ISA_EXEC_PROPERTY_I_Unit;
                }
                else 
                 slot_mask |= OP_m_unit(cur_op) ?
                             ISA_EXEC_PROPERTY_M_Unit : 
                             ISA_EXEC_PROPERTY_I_Unit; 
             } else { slot_mask |= ISA_EXEC_Unit_Prop(OP_code(cur_op)); }
        }

        // IPFEC hacker on brp special case
        // brp can issue in B0 and B2 while pro64 only model in B2
        if (TSI_Issue_Ports(OP_code(cur_op)).In(ip_B0) &&
            TSI_Issue_Ports(OP_code(cur_op)).In(ip_B2)){
            slot_mask = slot_mask | ISA_EXEC_PROPERTY_B_Unit;
        }

        // determine template
        if (idx % ISA_MAX_SLOTS == 0 && idx != 0) {
            if (stop_idx == 0) stop_mask = 0;
            else if (idx>stop_idx && ((idx-stop_idx)<ISA_MAX_SLOTS)) {
                 stop_mask = 1 << (ISA_MAX_SLOTS - stop_idx); 
            }
            INT ibundle;
            for (ibundle = 0; ibundle < ISA_MAX_BUNDLES; ++ibundle) {
                 UINT64 this_slot_mask = ISA_EXEC_Slot_Mask(ibundle);
                 UINT32 this_stop_mask = ISA_EXEC_Stop_Mask(ibundle);
                 if (   (slot_mask & this_slot_mask) == this_slot_mask
                 && stop_mask == this_stop_mask) break;
            }

            FmtAssert(ibundle != ISA_MAX_BUNDLES,
                ("couldn't find bundle for slot mask=0x%llx, stop mask=0x%x in OPS\n",
                     slot_mask, stop_mask));
            origin_ptn.bundle[(idx/ISA_MAX_SLOTS)-1] = ibundle;
            if (stop_idx>0) origin_ptn.start_in_bundle = 1; 
            slot_mask = 0;
            stop_mask = 0;
        } 
    } 

    return origin_ptn;
}
///////////////////////////////////////////////////////
// Best_Issue_Port: Find each op's best issue port
// INPUT: 
//       a complete list of ops by order, 
//       pattern for these ops
// OUTPUT:
//      each op's best issue port 
//      If find return TRUE else FASLE;
//
BOOL 
Best_Issue_Port(OPS *ops, INT stop_idx, const PATTERN_TYPE ptn, ISSUE_PORT *ip_list)
{
    INT idx = 0; 
    OP *cur_op;
    const DISPERSAL_TARG *ports_vector = dispersal_table.Query(&ptn);
    std::vector<OP*> _op_orders;
    std::vector<OP*>::iterator iter;
    std::map<OP*, PORT_SET> _op_avail; 

    _op_orders.empty();

    FOR_ALL_OPS_OPs(ops, cur_op) {
        if (Long_Instruction(cur_op)){
            // Give up long instruction for complex process
            // and low performance improvement
            return false;
        }
        ip_list[idx] = ip_invalid;
        if (idx >= stop_idx) {
            if (!OP_noop(cur_op)) {
                INT bundle = (idx)/ISA_MAX_SLOTS;
                INT slot = (idx) % ISA_MAX_SLOTS;
                PORT_SET ports = ports_vector->Ports(bundle,slot);
                PORT_SET avail_ports = TSI_Issue_Ports(OP_code(cur_op));
                ports = ports & avail_ports;
                _op_avail[cur_op] = ports;

                // no available issue port, return
                if (ports == 0) return false;

                INT count = ports.Count(); 
                for (iter=_op_orders.begin(); iter!=_op_orders.end(); iter++)
                {
                   OP* order = *iter;
                   if (_op_avail[order].Count() > count) 
                      break;
                }
                _op_orders.insert(iter, cur_op);    
            }
        }
        idx++;
    } 
    PORT_SET used_ports(0);
    for (INT m=0; m<_op_orders.size(); m++)
    {
       OP *op = _op_orders[m]; 
       PORT_SET ports =  _op_avail[op];
       ports = ports - used_ports;
       ISSUE_PORT issue_port = ports.First_IP();
       Is_True(issue_port!=ip_invalid, 
               ("op %s can't find suitable issue port", 
                TOP_Name(OP_code(op))));

       used_ports = used_ports + issue_port;
       idx = 0; 
       FOR_ALL_OPS_OPs(ops, cur_op) {
           if (cur_op == op) 
               break;
           idx++;
       }
       Is_True(cur_op == op, ("Issue_OP can't find in list of ops"));
       ip_list[idx] = issue_port; 
    }

   return TRUE;
}


////////////////////////////////////////////////////////////
// CGGRP_Bundle_OPS
//   Assumption: ops is one group for a cycle, When there are
//     stop bit between bundle, the ops must hold the complete
//     op sequence, that is include some op in last cycle.
// 
//   Given a list of bundled ops, and one new ops, Firstly find 
//   the orignal pattern for the ops, Then find new pattern 
//   after insert new ops. Last Do bundling for ops+op, save to
//   ops.
//   return TRUE when bundle new op in the same cycle with ops
//   else return FALSE
//
BOOL CGGRP_Bundle_OPS(OPS *ops,OPS *new_ops,INT stop_idx, BOOL cyclic)
{

    Is_True(OPS_length(ops) <= CYCLE_MAX_LENGTH, 
            ("OPS must be put suitable bundle group."));
    Is_True(stop_idx < ISA_MAX_SLOTS, 
            ("stop bit only can occur in first bundle."));

    BOOL dump_details = FALSE;

    if (Get_Trace(TP_A_MLBR, 0x01)) {
         dump_details = TRUE;
    }

    temp_state.Clear();
    prev_state.Clear();
    if (!cyclic)
         temp_state.Set_Flags(CYCLE_STRICT_ORDER);  // used in multi_branch
    else {           // cyclic==TURE, i.e. function was called from SWP, set flag
         temp_state.Cyclic(TRUE);
         prev_state.Cyclic(TRUE);
    }

    // care_bundle value
    OP *cur_op;
    INT idx=0;
    INT keep_template;
    OP *insert_point=NULL; 
    PATTERN_TYPE origin_ptn;
    PTN_TABLE_LINE ptns;

    origin_ptn = Find_Pattern_OPS(ops, stop_idx);

    // set temp_state
    if (dump_details) {
        fprintf(TFile, "Find last cylce template group:\n");
        origin_ptn.Dump(TFile);
    }
    Is_True(origin_ptn[0]!=ISA_MAX_BUNDLES, ("ops can't bundle in one cycle!"));

    ISSUE_PORT ip_list[CYCLE_MAX_LENGTH];
    if (!Best_Issue_Port(ops, stop_idx, origin_ptn, ip_list))
       return false;

    // recover cycle state: temp_state for adding new op.
    idx = 0;
    FOR_ALL_OPS_OPs(ops, cur_op) {
        if (idx < stop_idx) {
            if (idx == stop_idx-1) {
                prev_state.Add_OP(cur_op);
                insert_point=cur_op;
            } 
        } else {
            if (!OP_noop(cur_op)) { 
                Is_True(ip_list[idx]!=ip_invalid, ("op do not find the suitable issue port!"));
                OP_IDX op_idx=temp_state.Add_OP(cur_op,ip_list[idx]);
                Is_True(op_idx!=invalid_op_idx, ("Add_op failed. ops must be bundling well op list!"));
            }
       }
       idx++;
    }

    if (temp_state.Full()) return bundle_ops_fail();

    temp_state.Valid(TRUE);
    // keep template for first bundle
    if (stop_idx > 0) { keep_template = origin_ptn[0]; } 

    Is_True(temp_state.Valid(),("cannot find bundling for ops %d", OPS_length(ops)));
    
    // add new op to current cycle
    OP *op; 
    FOR_ALL_OPS_OPs(new_ops, op) {
        if (OP_dummy(op) || OP_simulated(op) || OP_noop(op)) continue;
        if (Long_Instruction(op)) return bundle_ops_fail();

        temp_state.Current_Pattern(invalid_ptn_idx); 
        if (temp_state.Full()) return bundle_ops_fail();
        OP_IDX new_op_idx = temp_state.Add_OP(op);
        if (new_op_idx == invalid_op_idx) 
            return bundle_ops_fail();
        if (!Bundle_Helper(new_op_idx)) {
            return bundle_ops_fail();
        }
    }
    
    ptns = temp_state.Patterns();

    // If slot is not enough because use compress template,
    // assume it to be splited after. Then, don;t consider 
    // keep_template and prev_state should be empty
    temp_state.Valid(FALSE); 
    for (INT ptn_idx=temp_state.Current_Pattern(); ptn_idx<ptns.size; ptn_idx++)
    {
        const PATTERN_TYPE ptn = ptns[ptn_idx];
        if (stop_idx == 0) {
            if (ptn.start_in_bundle || ptn.end_in_bundle) continue; 
        } else {
            if (ptn[0] != keep_template || 
                ptn.start_in_bundle != 1) continue;
        }
        if (temp_state.Legality_Chk_Needed()){
            if (!temp_state.Legality_Chk(ptn)) continue;
        }

        // pass all check
        temp_state.Current_Pattern(ptn_idx); 
        temp_state.Valid(TRUE); 
        break;
    }

    // try it again under split cycle; 
    BOOL split_between_cycle = FALSE;
    if (!temp_state.Valid()) {
        if (stop_idx) { // use split last cycle
            split_between_cycle = TRUE;
            prev_state.Clear();

            for (INT ptn_idx=temp_state.Current_Pattern(); ptn_idx<ptns.size; ptn_idx++)
            {
                const PATTERN_TYPE ptn = ptns[ptn_idx];
                if (ptn.start_in_bundle || ptn.end_in_bundle) continue; 
                
                if (temp_state.Legality_Chk_Needed()){
                  if (!temp_state.Legality_Chk(ptn)) continue;
                }
                
                temp_state.Current_Pattern(ptn_idx); 
                temp_state.Valid(TRUE); 
                 break;
            }
        } 
        if (!temp_state.Valid()) return bundle_ops_fail();
    }    
    temp_state.Polish_Tail();
    
    //  temp_state.Do_Bundling() to insert nop to OPS;
    //  if split_between_cycle is true, we should fill nops 
    //  to compress template.
    ptns = temp_state.Patterns();
    PATTERN_TYPE ptn  = ptns[temp_state.Current_Pattern()];
    if (split_between_cycle) {
        if (dump_details) {
           fprintf(TFile, "split last cycle can get pattern\n");
           ptn.Dump(TFile); 
        }
    }
 
    // delete OPS nop instruction
    idx = 0;
    OP *remove_op=NULL;
    INT cur_cycle = 0;
    FOR_ALL_OPS_OPs(ops, cur_op) {
        idx++;
        if (remove_op) {OPS_Remove_Op(ops, remove_op);}
        if (idx <= stop_idx) continue;

        if (OP_noop(cur_op)) {
            remove_op = cur_op;
        } else {
            remove_op = NULL;
            if (!cyclic) cur_cycle = OP_scycle(cur_op);
        }
    } 
    if (remove_op) {OPS_Remove_Op(ops, remove_op);}
    
    for(cur_op = OPS_first(new_ops); cur_op ;) {
        op  = cur_op;
        cur_op = OP_next(cur_op);
        if (OP_dummy(op) || OP_simulated(op) || OP_noop(op)) continue;
        if (!cyclic) OP_scycle(op) = cur_cycle;
        OPS_Remove_Op(new_ops, op);
        OPS_Append_Op(ops, op);
    }
    OPS_Remove_All(new_ops);

    Is_True(OPS_length(new_ops)==0, ("new ops should be empty!"));

    INT issue_op_list[CYCLE_MAX_LENGTH];
    if (!temp_state.Best_Issue_Order(issue_op_list)) return false; 

    GROUP_ASSEMBLE group_assemble = temp_state.Assemble();
    if (split_between_cycle) // add nop to ops
    {
        for (INT slot=0; slot < ISA_MAX_SLOTS; slot++) {
            if (slot<(stop_idx-1)) continue;
            if (slot == (stop_idx-1)) Reset_OP_end_group(insert_point);
            if (slot>(stop_idx-1)) {
                OP *new_op = group_assemble.MakeNop2Slot(keep_template, slot);
                OPS_Insert_Op_After(ops, insert_point, new_op);
                insert_point = new_op;
            }
        }
        Set_OP_end_group(insert_point);
    } 
    for (INT bundle=0; bundle < ISA_MAX_ISSUE_BUNDLES; bundle++){
        INT template_index = ptn[bundle];
        BOOL meet_bundle_start=TRUE;
        if (template_index == ISA_MAX_BUNDLES){
            break;  // It's an invalid one
        }
        if (ptn.start_in_bundle && bundle==0) 
            meet_bundle_start = FALSE;

        for (INT slot=0; slot < ISA_MAX_SLOTS; slot++) {
            if (ptn.start_in_bundle && 
                ISA_EXEC_Stop(template_index,slot)) {
                    meet_bundle_start=TRUE;
                    continue;
            }
            if (!meet_bundle_start) continue;
            OP_IDX op_idx = issue_op_list[bundle * ISA_MAX_SLOTS + slot];

            // should insert noop to ops
            if (op_idx==invalid_op_idx) {
                OP *new_op = group_assemble.MakeNop2Slot(template_index, slot);
                
                if (insert_point != NULL) {
                    OPS_Insert_Op_After(ops, insert_point,new_op);
                }else {
                    OPS_Prepend_Op(ops, new_op);
                }  
                insert_point = new_op;
            } else {
                
                OPS_Remove_Op(ops, temp_state[op_idx]);
                if (insert_point != NULL) {
                    OPS_Insert_Op_After(ops, insert_point,temp_state[op_idx]);
                }else {
                    OPS_Prepend_Op(ops, temp_state[op_idx]);
                }  
                insert_point = temp_state[op_idx];
            }
            if (!cyclic) group_assemble.Bundle_OP_End(insert_point,template_index, slot);
        }
    }
    if (!cyclic) group_assemble.Group_End(insert_point);
    else temp_state.Set_M_Unit();
    
    if (dump_details) {
        fprintf(TFile, "\n-----------------------------\n");
        fprintf(TFile, "\nWe can get new bundle group: \n");
        ptn = temp_state.Current_Pattern_Type();
        ptn.Dump(TFile); 
        fprintf(TFile, "\n-----------------------------\n");
    }

    temp_state.Clear();
    prev_state.Clear();
    return TRUE;
}

void
PTN_TABLE_LINE::Dump (FILE *f) {
    for (INT i = 0; i < size; i++) {
        fprintf(f,"%d ",i);
        ptns[i].Dump(f);
    }
}

//   Note: cyclic==FALSE means this function is call from multi_branch.cxx.
//         cyclic==TRUE means it is call from cg_swp_bundle.
BOOL CGGRP_Bundle_OPS(OPS *ops,OP *op,INT stop_idx, BOOL cyclic)
{
    OPS new_ops;
    OPS_Init(&new_ops);

    OPS_Append_Op(&new_ops, op);

    if (CGGRP_Bundle_OPS(ops, &new_ops, stop_idx, cyclic)) {
        return TRUE;
    } else {
        Is_True(OPS_length(&new_ops) == 1, ("new ops length is not correct!"));
        OPS_Remove_Op(&new_ops, op);
       return FALSE; 
    }
    
}
void CYCLE_STATE::Dump (FILE *f)
{
    extern void dump_op(const OP *op);
    fprintf(f, "\nValid:%d ",_valid);
    fprintf(f, "dependence:%d ",_dependency_exist);
    fprintf(f, "Occupied:");

    for (INT port=0; port<ip_number; port++){
        if (_reserve.In((ISSUE_PORT)port)){
            fprintf(f, " %s", Issue_Port_Name((ISSUE_PORT)port));
        }
    }
    fprintf(f, "\n");

    for (OP_IDX i=0; i<_num_ops; i++){
        fprintf(f, "%s: ",Issue_Port_Name(_const_OP_State(i).op_fu));
        dump_op(_const_OP_State(i).op);
    }
    fprintf(f,"\n");
    if (_reserve != 0) {
        const PTN_TABLE_LINE& ptns = Patterns (); 
        for (INT ptn_idx = 0; ptn_idx < ptns.size; ptn_idx++) {
    
            mINT16 bundle;
            PATTERN_TYPE ptn = ptns[ptn_idx];

            /* Print init data */
            if (ptn_idx == _ptn) {
                fprintf (f, "[=>] ");
            } else {
                fprintf (f, "[%2d] ", ptn_idx);
            }

            fprintf(f, "{ %d, %d, {", ptn.start_in_bundle, ptn.end_in_bundle);
            for (bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++){
                if (bundle!=0)
                    fprintf(f, ",");
                fprintf(f, " %d", ptn[bundle]);
            }
            fprintf(f, "} }");

            /* Print Comments */
            fprintf(f, "\t/*");
            if (ptn.start_in_bundle)
            fprintf(f, "->");
            for (bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++){
                fprintf(f, "%s ",ISA_EXEC_Name(ptn[bundle]));
            }
            fprintf(f," */\n");
        }
    }   
}

#ifdef Is_True_On

void CYCLE_STATE::gdb_dump(void) {
    Dump (stderr) ; fflush (stderr);    
}

void PTN_TABLE_LINE::gdb_dump (void) {
    Dump (stderr); fflush (stderr);
}

#endif /* Is_True_On */
