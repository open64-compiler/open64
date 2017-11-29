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

//-*-C++-*-

//=============================================================================
//=============================================================================
//
//  Module: speculation.h
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/speculation.h,v $
//
//  Description:
//  ============
//
//  Interface to Aurora Control and Date speculation.
//
//=============================================================================
//=============================================================================

//=============================================================================
// Change_ld_Form 
//   -  Convert a pure ld to ld.s,ld.a,ld.sa .
//
// TN_is_GFP 
//   - Decide whether the tn is GR, FR, or PR.
//=============================================================================


#ifndef speculation_INCLUDED
#define speculation_INCLUDED

#include <vector>
#include <utility>
#include "op_list.h"
#include "cg_dep_graph.h"
#include "region.h"

struct compare_node
{
    bool operator()(const REGIONAL_CFG_NODE* n1, const REGIONAL_CFG_NODE* n2) const
    {
        INT32 id1 = n1->Id();
        INT32 id2 = n2->Id();
    
        return ( id1 < id2 );
    }
};

struct compare_tn
{
    bool operator()(const TN* tn1, const TN* tn2) const
    {
       mTN_NUM num1 = TN_number(tn1); 
       mTN_NUM num2 = TN_number(tn2); 
    
       return (num1 < num2);
    }
};

struct compare_op
{
    bool operator()(const OP* op1, const OP* op2 ) const
    {
       mBB_NUM id1 = BB_id(OP_bb(op1)); 
	mBB_NUM id2 = BB_id(OP_bb(op2)); 
       mTN_NUM num1 = OP_map_idx(op1); 
       mTN_NUM num2 = OP_map_idx(op2); 

	if(id1 == id2){
           return (num1 < num2);
       }
       return id1<id2;
    }
};

extern std::vector< std::pair<OP*,OP*> >  load_chk_pairs;
extern OP* Local_Insert_CHK(OP *spec_ld, OP *point, TN *pr_tn = True_TN);
extern OP* Insert_CHK(OP* primary_ld, std::vector<OP *>& copys, BB* home_bb, OP* pos, TN* pr_tn);
extern BOOL OP_baneful(OP* op);
extern OP *Change_ld_Form(OP *load_op, ISA_ENUM_CLASS_VALUE target_form);
extern BOOL Is_Control_Speculation_Gratuitous(OP*, BB*, OP*);
extern void Delete_Recovery_Info_For_BB(BB *bb);
extern void Set_Speculative_Chain_Begin_Point(OP*, OP*);
extern BOOL BB_Hold_Disjoint_Speculative_Code(BB*);

//  ===== (<%pr%p6>) <r7>=<ldtype>,<ldhint>,[<%bs%r7>] ===== 
//  Instruction_Group("O_108", TOP_ld1, TOP_ld2, TOP_ld4, TOP_ld8, TOP_UNDEFINED);
//  Operand(0, pr, predicate);
//  Result(0, int64);
//  Operand(1, ldtype);
//  Operand(2, ldhint);
//  Operand(3, int64, base);
#define  enum_ldtype_pos  1


inline BOOL
TN_is_GFP(TN *tn)
{

    if(!TN_is_register(tn))
        return  FALSE;
      
    ISA_REGISTER_CLASS rc = TN_register_class(tn);
    if( rc == ISA_REGISTER_CLASS_integer ||
        rc == ISA_REGISTER_CLASS_float ||
        rc == ISA_REGISTER_CLASS_predicate ) return TRUE;

    return FALSE;
}

#endif
