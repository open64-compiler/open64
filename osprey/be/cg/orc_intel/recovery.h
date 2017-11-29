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
//  Module :  recovery.h
//  $Date  :  2001/02/20 21:12:34 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/recovery.h,v $
//
//  Description:
//  ============
//
//  Interface to Ipfec Recovery Code Generation.
//
//=============================================================================
//=============================================================================


#ifndef recovery_INCLUDED
#define recovery_INCLUDED


#include "speculation.h"
#include "tn.h"
#include "region.h"
#include "op.h"


extern int  Generate_Recovery_Code();
extern void Adjust_Recovery_Block();
extern BB* Handle_Chk_Split_Bunch(BB*);
extern void Force_Chk_Fail();


#define  rec_prob  0.01F
#define  btm_prob  0.99F


#define  chks_reg_pos  1
#define  chks_tgt_pos  2  

#define  chka_reg_pos  2  
#define  chka_tgt_pos  3


//=============================================================================
// Set_chk_tgt
//   - Set chk_op's branch target opnd.
//   
// Get_chk_tgt
//   - Get chk_op's branch target opnd.
// 
// Set_chk_reg
//   - Set the under checking opnd of the chk_op.
// 
// Get_chk_reg
//   - Get the under checking opnd of the chk_op.
// 
// BB_chk_op 
//   -  If the BB's last op is a chk op, return it, or return NULL.
//=============================================================================
inline OP* 
Set_chk_tgt(OP *chk_op, TN *target_tn)
{
    Is_True(OP_chk(chk_op),("not a chk op!"));

    INT target_pos;
    target_pos = OP_chk_s(chk_op) ? chks_tgt_pos : chka_tgt_pos ;
    Set_OP_opnd(chk_op, target_pos, target_tn);

    return chk_op;
}

inline TN*
Get_chk_tgt(OP* chk_op)
{
    Is_True(OP_chk(chk_op),("not a chk op!"));
    
    return ( OP_chk_s(chk_op) ? OP_opnd(chk_op,chks_tgt_pos) : 
                                OP_opnd(chk_op,chka_tgt_pos) );
}

inline OP* 
Set_chk_reg(OP *chk_op, TN *register_tn)
{
    Is_True(OP_chk(chk_op),("not a chk op!"));

    INT register_pos;
    register_pos = OP_chk_s(chk_op) ? chks_reg_pos : chka_reg_pos ;
    Set_OP_opnd(chk_op, register_pos, register_tn);

    return chk_op;
}


inline TN*
Get_chk_reg(OP *chk_op)
{
    Is_True(OP_chk(chk_op),("not a chk op!"));

    return ( OP_chk_s(chk_op) ? OP_opnd(chk_op,chks_reg_pos) :  
                                OP_opnd(chk_op,chka_reg_pos) );
}

inline OP* BB_chk_op( BB *bb )
{
  OP *last_op = BB_last_op(bb);

  if (!last_op || !OP_chk(last_op)){
      return NULL;
  } else {
      return last_op;
  }
}

#endif






