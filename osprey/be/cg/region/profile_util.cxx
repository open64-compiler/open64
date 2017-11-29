/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
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

//*********************************************************************
//
// Module: profile_bb_util.cxx
// $Date: 2005/12/30 01:47:13 $
// $Author: weitang $
// $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/region/profile_util.cxx,v $
//
// Description:
//
// Routines for manipulating the BB's frequency and edge's probability.
//
//*********************************************************************
#include "profile_util.h"
#include "freq.h"

#define NONE_EDGE  -1.00
#define ZERO_FREQ   0.0
#define ZERO_PROB   0.0
#define FREQ_CHECK_MSG_MAX    10

BBLIST* Find_Edge(BB* src_bb,BB* tgt_bb)
{
  BBLIST* succ_edge;
  for(succ_edge = BB_succs(src_bb); succ_edge != NULL; 
          succ_edge = BBLIST_next(succ_edge))
  {
		if(BBLIST_item(succ_edge) == tgt_bb)
		  return succ_edge;
  }
  return succ_edge;
}

// Get the edge (from src_bb to tgt_bb) frequency
float Freq(BB* src_bb,BB* tgt_bb)
{
  BBLIST* succ_edge;
  for(succ_edge = BB_succs(src_bb); succ_edge != NULL; 
          succ_edge = BBLIST_next(succ_edge))
  {
		if(BBLIST_item(succ_edge) == tgt_bb)
		  return BBLIST_freq(succ_edge);
  }
  if(succ_edge == NULL)
    return NONE_EDGE;
}

// Get the local edge probability 
float Prob_Local(BB* src_bb, BB* tgt_bb)
{
  BBLIST* edge = Find_Edge(src_bb,tgt_bb);
  if ( edge != NULL )
    return BBLIST_prob(edge);
  else
    return 0;
}

BOOL Set_Freq( BB* src_bb, BB* tgt_bb, float nfreq)
{
  BBLIST* succ_edge;
  for(succ_edge = BB_succs(src_bb); succ_edge != NULL; 
        succ_edge=BBLIST_next(succ_edge))
  {
    if(BBLIST_item(succ_edge) == tgt_bb)
    {
      BBLIST_freq(succ_edge) = nfreq;
      return TRUE;         
    }
  }
  if(succ_edge==NULL)
          return FALSE;
}

BOOL Set_Prob( BB* src_bb, BB* tgt_bb, float nprob)
{
  BBLIST* succ_edge;
  for(succ_edge=BB_succs(src_bb); succ_edge!=NULL; 
        succ_edge=BBLIST_next(succ_edge))
  {
    if(BBLIST_item(succ_edge) == tgt_bb)
    {
      Is_True(REGION_First_BB != NULL, ("Region first BB can not be NULL"));
      Is_True(nprob <= 1.0,("prob can not excess 1.0 "));
      Is_True(nprob >= ZERO_PROB, ("prob can not be less than 0.0")); 
      BBLIST_prob(succ_edge) = nprob;
      return TRUE;
    }
  }
  if(succ_edge==NULL)
          return FALSE;
}

BB*  Find_Last_BB(BB* src_bb)
{
  BB* lbb;
  for(lbb = src_bb; BB_next(lbb) != NULL; lbb = BB_next(lbb))
    ;
  return lbb;
}

BOOL Prob_OK(BB* bb)
{
  if (!BB_succs(bb)) return TRUE;
  float prob = 0.0F;
  for(BBLIST* edge = BB_succs(bb); edge != NULL; 
      edge = BBLIST_next(edge))
  {
    prob += BBLIST_prob(edge);    
  }
  return FREQ_Match(prob, 1.0F);
}

BOOL Freq_OK(BB* bb)
{
  if (!BB_succs(bb)) return TRUE; 
  float prob = 0.0F;
  float edge_freq, in_total_freq, out_total_freq;
  edge_freq = in_total_freq = out_total_freq = 0.0F;
  
  if(BB_succs(bb))
  {
  // make sure that outcoming edges' frequency is equal to incoming edges'
  // prob * bb's freq
    for(BBLIST* edge = BB_succs(bb); edge != NULL; 
          edge = BBLIST_next(edge))
      {
        prob = BBLIST_prob(edge);    
        edge_freq = prob * BB_freq(bb);
        if(!FREQ_Match(edge_freq, BBLIST_freq(edge)))
        {
          return FALSE;
        }
        out_total_freq += edge_freq;  
      }
  }
  
  // compute the total frequency of incoming edges
  for(BBLIST* edge = BB_preds(bb); edge != NULL; 
      edge = BBLIST_next(edge))
  {
    prob = Prob_Local(BBLIST_item(edge),bb);    
    edge_freq = prob * BB_freq(BBLIST_item(edge));
    in_total_freq += edge_freq;  
  }
  
  if(BB_succs(bb) && BB_preds(bb))
  {
    if(!FREQ_Match(in_total_freq,out_total_freq))
    {
       return FALSE;
    }  
  }

  // make sure that outcoming edges' frequency is equal to BB's frequency
  if(BB_succs(bb))
  {
    if(!FREQ_Match(BB_freq(bb),out_total_freq))
      return FALSE;
  }  
  return TRUE;  
}

BOOL Check_CFG_Consistency(const char* caller)
{
  INT msg1_cnt = 0;
  INT msg2_cnt = 0;
  BOOL  is_ok = TRUE;
  BB   *bb;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (!Prob_OK(bb)) {
      is_ok = FALSE;
      ++msg1_cnt;
      if (msg1_cnt <= FREQ_CHECK_MSG_MAX) {
	DevWarn("%s: Succ probs of BB:%d do not sum to 1.0", caller,
		BB_id(bb)); 
      } else if (msg1_cnt == FREQ_CHECK_MSG_MAX+1) {
	DevWarn("%s: more than %d Succ probs sum msgs - disabling msg\n",
		caller, FREQ_CHECK_MSG_MAX);
      }
    }

    if (!Freq_OK(bb)) {
      is_ok = FALSE;
      ++msg2_cnt;
      if (msg2_cnt <= FREQ_CHECK_MSG_MAX) {
	DevWarn("%s: BB:%d freq (%g) != the sum of its incoming edge freq",
		caller, BB_id(bb), BB_freq(bb));
      } else if (msg2_cnt == FREQ_CHECK_MSG_MAX+1) {
	DevWarn("%s: more than %d sum of pred msgs - disabling msg\n",
		caller, FREQ_CHECK_MSG_MAX);
      }
    }
  }
  return is_ok;
}

BOOL Check_BB_Consistency(const char* caller,BB* bb,INT msg1_cnt,INT msg2_cnt)
{
  bool is_ok = TRUE;
  if (!Prob_OK(bb)) 
  {
    is_ok = FALSE;
    ++msg1_cnt;
    if (msg1_cnt <= FREQ_CHECK_MSG_MAX) 
    {
	    DevWarn("%s: Succ probs of BB:%d do not sum to 1.0", caller,
		     BB_id(bb)); 
    } else if (msg1_cnt == FREQ_CHECK_MSG_MAX+1) 
    {
	    DevWarn("%s: more than %d Succ probs sum msgs - disabling msg\n",
		    caller, FREQ_CHECK_MSG_MAX);
    }
   }

   if (!Freq_OK(bb)) 
   {
     is_ok = FALSE;
     ++msg2_cnt;
     if (msg2_cnt <= FREQ_CHECK_MSG_MAX) {
	     DevWarn("%s: BB:%d freq (%g) != the sum of its pred edge freq",
		      caller, BB_id(bb), BB_freq(bb));
     } else if (msg2_cnt == FREQ_CHECK_MSG_MAX+1) 
     {
	     DevWarn("%s: more than %d sum of pred msgs - disabling msg\n",
		    caller, FREQ_CHECK_MSG_MAX);
     }
   }  
}

float BB_Total_In_Freq(BB *bb)
{
  float total_in = 0.0F;
  for(BBLIST* in_edge = BB_preds(bb); in_edge != NULL; 
                                in_edge = BBLIST_next(in_edge))
    total_in += Freq(BBLIST_item(in_edge),bb);
  return total_in;
}

float BB_Total_Out_Freq(BB *bb)
{
  float total_out = 0.0F;
  for(BBLIST* out_edge = BB_succs(bb); out_edge != NULL; 
                                out_edge = BBLIST_next(out_edge))
    total_out += Freq(bb,BBLIST_item(out_edge));
  return total_out;
}

LABEL_IDX Get_Op_Label(op* branch_op)
{
  INT tn_num = OP_opnds( branch_op );
  for( INT i = 0; i<tn_num; i++)
  {
    TN* tgt_tn = OP_opnd( branch_op, i );
    if( TN_is_label(tgt_tn) )
    return TN_label(tgt_tn);
  }  
}

BOOL TN_In_BB(BB* bb, TN* tn)
{
  OP* opcode = bb->ops.first;
  for( ; opcode != NULL; opcode = OP_next(opcode))
  {
    INT i;
    for (i = 0; i < OP_results(opcode); i++) 
    {
      if (tn == OP_result(opcode,i)) 
        return TRUE; 
    }
 
    for (i = 0; i < OP_opnds(opcode); i++) 
    {
      if (tn == OP_opnd(opcode,i))
        return TRUE; 
    }
  }   
  return FALSE;
}
