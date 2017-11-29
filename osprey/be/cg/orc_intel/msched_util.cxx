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
#include "errors.h"
#include "tn.h"
#include "op.h"
#include "cgtarget.h"
#include "targ_isa_bundle.h"
#include "targ_issue_port.h"
#include "dag.h"
#include "msched_util.h"

//-*-c++-*-
//=============================================================================
//=============================================================================
//
//  Module :  msched_util.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/msched_util.cxx,v $
//
//  Description:
//  ============
//
//  basic funtion of micro-scheduler to reduce dependence on IR.
//
//=============================================================================
//=============================================================================


// convert simulated op by given UNIT property. If m_unit , set flags.
void MSCHED_Convert_Simulated(OP *op, ISA_EXEC_UNIT_PROPERTY prop)
{
    TOP adjust_top = CGTARG_Simulated_Top(op, prop);
    OP_Change_Opcode(op, adjust_top);
    if (prop == ISA_EXEC_PROPERTY_M_Unit) {
        Set_OP_m_unit(op);   
    } else {
        Reset_OP_m_unit(op);
    }
}

//============================================================================
//
//  Specail case in dealing with TOP_mov_t_ar_i TOP_mov_t_ar_r
//  And TOP_mov_f_ar, their operands decide which form opcode
//  should be convert to.
//============================================================================
void MSCHED_Real_OP(OP *op)
{
   
    REGISTER reg;
    BOOL is_m, is_i;
    switch (OP_code(op)){
    case TOP_mov_t_ar_i:
    case TOP_mov_t_ar_r:
        if (TN_is_dedicated(OP_result(op,0))){
            reg = TN_register(OP_result(op,0));
            is_m = REGISTER_SET_MemberP(
                     REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_ar_m),
                     reg);
            is_i = REGISTER_SET_MemberP(
                     REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_ar_i),
                     reg);
            if (is_m && !is_i){
                MSCHED_Convert_Simulated(op, ISA_EXEC_PROPERTY_M_Unit);
            }
            else if (is_i && !is_m){
                MSCHED_Convert_Simulated(op, ISA_EXEC_PROPERTY_I_Unit);
            }
        }
        break;
    case TOP_mov_f_ar:
        if (TN_is_dedicated(OP_opnd(op,1))){
            reg = TN_register(OP_opnd(op,1));
            is_m = REGISTER_SET_MemberP(
                     REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_ar_m),
                     reg);
            is_i = REGISTER_SET_MemberP(
                     REGISTER_SUBCLASS_members(ISA_REGISTER_SUBCLASS_ar_i),
                     reg);
            if (is_m && !is_i){
                MSCHED_Convert_Simulated(op, ISA_EXEC_PROPERTY_M_Unit);
            }
            else if (is_i && !is_m){
                MSCHED_Convert_Simulated(op, ISA_EXEC_PROPERTY_I_Unit);
            }
        }
        break;
    }
}

BOOL Is_Violate_Issue_Rule(OP *op, INT ip)
{
    if ( (OP_code(op) == TOP_break)
        && (*Issue_Port_Name(ip) == 'B')){
        // Specail case on break
        // if the immediate operand non zero, it should NOT be issued to b
        TN *immd_tn=OP_opnd(op, 1);
        FmtAssert((TN_has_value(immd_tn) && TN_value(immd_tn) == 0),
                  ("break opruction with none zero operand"
                   " should specifies the slot type!") );
    }
    return false;
}

//============================================================================
//
//  Class: GROUP_ASSEMBLE
//  
//  Class Description:
//     To give some function to support bundling, such as StartBundle..
//     EndBundle, Set OP to slot, fill_Nop
//

GROUP_ASSEMBLE::GROUP_ASSEMBLE(void)
{
    OPS_Init(&_op_list);
}
GROUP_ASSEMBLE::~GROUP_ASSEMBLE(void)
{
    // No op left in _op_list;
    Is_True(OPS_length(&_op_list) == 0, 
            ("There are %d ops were not issued!"
             , OPS_length(&_op_list)));

    // destruction
}
// Set this op was bundled.
void GROUP_ASSEMBLE::SetBundled(OP *op)
{
    Set_OP_Scheduled(op);
    Set_OP_bundled(op);
}
//  Set op as start of a bundle
void GROUP_ASSEMBLE::SetStartBundle(OP *op)
{
    SetBundled(op);
    Set_OP_start_bundle(op);
}
//  Set op as end of a bundle
void GROUP_ASSEMBLE::SetEndBundle(OP *op)
{
    SetBundled(op);
    //  Set_OP_end_bundle(op);
}
//  Set op as start of a group (in same cycle)
void GROUP_ASSEMBLE::SetStartGroup(OP *op)
{
    // nothing for ORC
}
//  Set op as end of a group
void GROUP_ASSEMBLE::SetEndGroup(OP *op)
{
  
    if (op != NULL) {
        Set_OP_end_group(op);
        SetStopBit(op);
    }
}
//  Set stop bit after this op;
void GROUP_ASSEMBLE::SetStopBit(OP *op)
{
    // nothing for ORC;
}
// reset function below:
// Reset this op was not bundled.
void GROUP_ASSEMBLE::ResetBundled(OP *op)
{
    Reset_OP_bundled(op);
}
//  Reset op as start of a bundle
void GROUP_ASSEMBLE::ResetStartBundle(OP *op)
{
   Reset_OP_start_bundle(op);
}
//  Reset op as end of a bundle
void GROUP_ASSEMBLE::ResetEndBundle(OP *op)
{    
    // nothing for ORC
}
//  Reset op as start of a group (in same cycle)
void GROUP_ASSEMBLE::ResetStartGroup(OP *op)
{
    // nothing for ORC
}
//  Reset op as end of a group
void GROUP_ASSEMBLE::ResetEndGroup(OP *op)
{
    Reset_OP_end_group(op);
    ResetStopBit(op);
}
//  Reset stop bit after this op;
void GROUP_ASSEMBLE::ResetStopBit(OP *op)
{
    // nothing for ORC;
}

ISA_EXEC_UNIT_PROPERTY
GROUP_ASSEMBLE::ReplaceMLX(INT slot, ISA_EXEC_UNIT_PROPERTY prop) {
    // For MLX template assign similar as MFI,
    // Hacker on Itanium MLX dispersal rule: the same as MFI
    // when nop or one word instruction.
    if (prop == ISA_EXEC_PROPERTY_L_Unit) // got MLX
    {   // It should be MFI
        switch (slot){
            case 1:
                prop = ISA_EXEC_PROPERTY_F_Unit;
                break;
            case 2:
                prop = ISA_EXEC_PROPERTY_I2_Unit;
                break;
            default:
                Is_True(FALSE, ("L slot occured in slot 0!"));
         }
    }
    return prop;
}
//  Set this slot in template as nop
//  return the nop op;
OP *GROUP_ASSEMBLE::MakeNop2Slot(INT itemplate, INT slot)
{
    ISA_EXEC_UNIT_PROPERTY slot_prop =
           ISA_EXEC_Slot_Prop(itemplate, slot);

    slot_prop = ReplaceMLX(slot, slot_prop); 
   
    TOP top = (TOP)CGTARG_Noop_Top(slot_prop);
    Is_True(top!=TOP_nop, ("Failed in find proper nop!"));
    OP *op = Mk_OP(top, True_TN, Gen_Literal_TN(0,4));
    return op;
}

// Insert op to _op_list in order
void GROUP_ASSEMBLE::InsertOP(OP *op)
{
    OPS_Append_Op(&_op_list, op);
}

// Remove op from _op_list;
void GROUP_ASSEMBLE::RemoveOP(OP *op)
{
    OPS_Remove_Op(&_op_list, op);
}

// covert the simulated op to real op
void GROUP_ASSEMBLE:: ConvertSimulatedOP(OP *op, INT itemplate, INT slot)
{
    if (TOP_is_simulated(OP_code(op))){
        ISA_EXEC_UNIT_PROPERTY slot_prop =
               ISA_EXEC_Slot_Prop(itemplate, slot);

        if (!Long_Instruction(op)) {
            slot_prop = ReplaceMLX(slot, slot_prop);
        }

        MSCHED_Convert_Simulated(op, slot_prop);
    }
}

// when bundle one op end, Set flags, and give extra information to other phase.
void GROUP_ASSEMBLE::Bundle_OP_End(OP *op, INT itemplate, INT slot)
{
    
    Is_True(op, ("Not anything issued in a used slot!"));

    // clean and set flags of bundle.
    ResetStartBundle(op);
    ResetEndBundle(op);
    ResetStartGroup(op);
    ResetEndGroup(op);

    if (slot == 0) {
        SetStartBundle(op);
    } else if(slot == (ISA_MAX_SLOTS-1)) {
        SetEndBundle(op);
    } else {
        SetBundled(op);
    }

    // give information to scheduler when op use m issue port
    ISA_EXEC_UNIT_PROPERTY slot_prop =
               ISA_EXEC_Slot_Prop(itemplate, slot);
 
    if (slot_prop==ISA_EXEC_PROPERTY_M_Unit){
        Set_OP_m_unit(op);
    }
    else{
        Reset_OP_m_unit(op); 
    }    
}
// when one group ends, set flags
void GROUP_ASSEMBLE::Group_End(OP *op)
{
    if (op == NULL) op = OPS_last(&_op_list);
    SetStartGroup(op);
    SetEndGroup(op);    
}
// Insert _op_list to BB;
void GROUP_ASSEMBLE::InsertAfter(BB *bb, OP *position)
{ 
    OP *op, *next;
    for (op = OPS_first(&_op_list); op && op != OP_next(OPS_last(&_op_list));
         op = next){
        next = OP_next(op);
        RemoveOP(op);
        mUINT16 old_map_idx = OP_map_idx(op);
        if (position){
            BB_Insert_Op_After(bb, position, op);
        }
        else{
            BB_Prepend_Op(bb,op);
        }
        if (!OP_noop(op)){
            op->map_idx = old_map_idx;
        }
        position = op;
    }
}
OP *GROUP_ASSEMBLE::EmitNop(INT itemplate, INT slot)
{
     OP *op = MakeNop2Slot(itemplate, slot);
     InsertOP(op);
     Bundle_OP_End(op, itemplate, slot);
     return op;
}
void GROUP_ASSEMBLE::EmitOp(OP *op, INT itemplate, INT slot)
{
    ConvertSimulatedOP(op, itemplate, slot);
    InsertOP(op);
    Bundle_OP_End(op, itemplate, slot);
}
