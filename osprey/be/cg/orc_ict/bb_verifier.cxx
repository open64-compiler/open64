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
// Module: bb_verifier.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/bb_verifier.cxx,v $
//
// Description:
//
// Utilities to verify various attributes of basic blocks.
//
//*********************************************************************

#include "bb_verifier.h"
#include "targ_isa_registers.h"
#include "bb.h"

void BB_Verify_Flags(void)
{
    BB* bb;
    OP* op;
    for(bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
    {
        if(BB_entry(bb)) {
          Is_True(!BB_preds_len(bb),("An entry bb hasn't been set"));
        }

        if(BB_exit(bb)) {
          Is_True(!BB_succs_len(bb), ("An exit bb hasn't been set!"));
        }

        ANNOTATION * annotations = BB_annotations(bb);
        while(annotations){
            ANNOTATION_KIND kind = annotations->kind;
            switch(kind){
            case ANNOT_LABEL:
            	Is_True(BB_has_label(bb), ("a bb with label hasn't been set"));
            	break;
            case ANNOT_PRAGMA:
            	Is_True(BB_has_pragma(bb), ("a bb with pragma hasn't been set"));
            	break;
            case ANNOT_ENTRYINFO:
            	Is_True(BB_entry(bb),("An entry bb hasn't been set"));
                break;
            case ANNOT_EXITINFO:
            	Is_True(BB_exit(bb), ("An exit bb hasn't been set!"));
        	    break;
            case ANNOT_CALLINFO:
        	    Is_True(BB_call(bb), ("A call bb hasn't been set!"));
        	    break;
            case ANNOT_NOTE:
        	    Is_True(BB_has_note(bb), ("a bb with note hasn't been set!"));
        	    break;
            case ANNOT_LOOPINFO:
            	Is_True(BB_loophead(bb), ("loophead bb hasn't been set!"));
            	break;
            case ANNOT_ROTATING_KERNEL:
            	Is_True(BB_rotating_kernel(bb), ("a rotating kernel bb hasn't been set!"));
            	break;
            case ANNOT_ASMINFO:
            	Is_True(BB_asm(bb), ("a bb with asm hasn't been set!"));
            	break;
            case ANNOT_SWITCH:
            	break;
            default:
                Is_True(FALSE, ("unexpected annotation kind: %d", kind));
            }
            annotations = annotations->next;
        }
        
        op = BB_last_op(bb);
        if(BB_call(bb)) {
          Is_True(OP_call(op), ("A call bb isn't right"));
        }

        if(BB_scheduled(bb)) {
        	Is_True(BB_Verify_Sched(bb), ("a sched bb isn't right!"));
        }

        if(BB_reg_alloc(bb)) {
        	Is_True(BB_Verify_Reg_Alloc(bb), ("a reg alloc bb isn't right!"));
        }

        if(BB_recovery(bb)) {
        	Is_True(BB_Verify_Recovery(bb), ("a recovery bb isn't right!"));
        }

        if(BB_chk_split(bb)) {
        	Is_True(BB_Verify_Chk_Split(bb), ("a chk split bb isn't right!"));
        }
    }
}

BOOL BB_Verify_Sched(BB* bb)
{
    OP* op;
    FOR_ALL_BB_OPs(bb, op){
    	if(!OP_simulated(op) && !OP_dummy(op) && !OP_bundled(op)) {
        	return FALSE;
	}
    }
    return TRUE;
}

BOOL BB_Verify_Reg_Alloc(BB* bb)
{
	OP* op;
	TN* tn;
  mUINT8 i;
	FOR_ALL_BB_OPs(bb, op){
		for(i = 0; i<OP_results(op); i++){
			tn = OP_result(op, i);
			if(TN_is_register(tn) &&
				TN_register(tn) == REGISTER_UNDEFINED)
				return FALSE;
		}

		for(i = 0; i<OP_opnds(op); i++){
			tn = OP_opnd(op, i);
			if(TN_is_register(tn) &&
				TN_register(tn) == REGISTER_UNDEFINED)
				return FALSE;
		}
	}
	return TRUE;
}

BOOL BB_Verify_Recovery(BB* bb)
{
    if(BB_preds_len(bb) != 1) return FALSE;
    OP* op = BB_last_op(BBLIST_item(BB_preds(bb)));
    if(OP_chk(op)) {
      return TRUE;
    }
    else if ((op = OP_prev(op)) && OP_chk(op)) {
      return TRUE;
    }
    else if ((op = OP_prev(op)) && OP_chk(op)) {
      return TRUE;
    }

    return FALSE;
}

BOOL BB_Verify_Chk_Split(BB* bb)
{
    if(BB_chk_split_head(bb)) {
      return TRUE;
    }

    OP* op;
    BOOL has_chk_op = FALSE;
    FOR_ALL_BB_OPs(bb, op) {
    	if(OP_chk(op)) {
          has_chk_op = TRUE;
          break;
        }
    }

    if (!has_chk_op) {
    	BB* succ_bb = BBLIST_item(BB_succs(bb));
    	if(BB_chk_split(succ_bb)) {
	Is_True(BB_chk_split_head(succ_bb), ("The chk BB should be chk_split_head!"));
        } else {
    	  if(BB_succs(succ_bb) && BB_chk_split(BBLIST_item(BB_succs(succ_bb)))) {
    		Is_True(BB_chk_split_head(BBLIST_item(BB_succs(succ_bb))),
			      	("The chk BB should be chk_split_head!"));
        }
      }

    	succ_bb = BBLIST_item(BB_preds(bb));
    	if(BB_chk_split(succ_bb)) {
        return TRUE;
        } else if(BB_preds(succ_bb)) {
    	succ_bb = BBLIST_item(BB_preds(succ_bb));
    	  if(BB_chk_split(succ_bb)) {
            return TRUE;
          }
        }
    }
    else {
      return TRUE;
    }

    return FALSE;
}

