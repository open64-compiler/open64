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
//  Module :  msched_util.h
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/msched_util.h,v $
//
//  Description:
//  ============
//
//  basic funtion of micro-scheduler to reduce dependence on IR.
//
//=============================================================================
//=============================================================================

#ifndef MSCHED_UTIL_INCLUDED
#define MSCHED_UTIL_INCLUDED

#include "cg_dep_graph.h"

#define  Dependence_Between Is_There_OP_Dependence
#define  Delete_Graph  CG_DEP_Delete_Graph

extern BOOL Dependence_Between(OP *, OP *);

inline INT Instruction_Length(OP *op) {
    return ISA_PACK_Inst_Words(OP_code(op));
}
 
inline BOOL Long_Instruction(OP *op) {
    return ( Instruction_Length(op) > 1); 
}

// a kind of arc
inline BOOL PostBranchArc(ARC *arc) {
    return (ARC_kind (arc) == CG_DEP_POSTBR);
}

//Use unit property to convert opcode of op
extern void MSCHED_Convert_Simulated(OP *op, ISA_EXEC_UNIT_PROPERTY prop);

// change simulated op to the real op
extern void MSCHED_Real_OP(OP *op);

// Whether violate issue rule, some special case ,
//  such as break with nozero immediate.
extern BOOL Is_Violate_Issue_Rule(OP *op, INT ip);

//============================================================================
//
//  Class: GROUP_ASSEMBLE
//  
//  Class Description:
//     To give some function to support bundling, such as StartBundle..
//     EndBundle, Set OP to slot, fill_Nop
//
class GROUP_ASSEMBLE
{
private:
    OPS _op_list;
    void SetStartBundle ( OP *op );
    void SetEndBundle ( OP *op );
    void SetBundled ( OP *op );
    void SetStartGroup (OP *op);
    void SetEndGroup (OP *op);

    void ResetBundled ( OP *op );
    void ResetStartBundle ( OP *op );
    void ResetEndBundle ( OP *op );
    void ResetStartGroup ( OP *op );
    void ResetEndGroup ( OP *op );
    
    void SetStopBit ( OP *op );
    void ResetStopBit ( OP *op );

    ISA_EXEC_UNIT_PROPERTY 
       ReplaceMLX(INT slot, ISA_EXEC_UNIT_PROPERTY prop);
public:
    GROUP_ASSEMBLE(void); 
    ~GROUP_ASSEMBLE(void);

    // bundle related func

    void Bundle_OP_End(OP *op, INT itemplate, INT slot);

    OP   *MakeNop2Slot(INT itemplate, INT slot);
    void InsertOP(OP *op);
    void RemoveOP(OP *op);
    void ConvertSimulatedOP(OP *op, INT itemplate, INT slot);
    void InsertAfter(BB *bb, OP *position);
    
    void EmitOp(OP *op, INT itemplate, INT slot);
    OP   *EmitNop(INT itemplate, INT slot);
    void Group_End(OP *op=NULL);
    
};
#endif
