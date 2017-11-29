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

#ifndef _Optimized_LRA_For_EBO
#define _Optimized_LRA_For_EBO
#include "lra.h"
#include "cxx_template.h"
#include "cg_vector.h"
#include <list>
using std::list;

class SCHED_PU_MEM_POOL {
protected :
      MEM_POOL _mem_pool;
public:
      SCHED_PU_MEM_POOL () {
         MEM_POOL_Initialize( &_mem_pool,"SCHED_PU_MEM",TRUE);
         MEM_POOL_Push ( &_mem_pool);
      }
      ~SCHED_PU_MEM_POOL () {
         MEM_POOL_Pop (&_mem_pool);
         MEM_POOL_Delete(&_mem_pool);
      }
};

class PU_REGION_CFLOW_MGR : public SCHED_PU_MEM_POOL {
private :
      int _bb_num;
      typedef BS REACH_INFO_VECT;
      typedef struct tagREACH_PROB_VECT {
         INT32 elem_num;
         INT32 vect_size;
         tagREACH_PROB_VECT(void) {
            elem_num=0;
            vect_size=0;
         }

      } REACH_PROB_VECT ;
      typedef struct tagNODE_CFLOW_INFO {
         union {
            BB *bb;
         } node ;
         REACH_PROB_VECT reach_prob;
         INT32 min_level;
         INT32 max_level;
         REACH_INFO_VECT *reach_bb;
         tagNODE_CFLOW_INFO () {
            node.bb  = NULL;
            reach_bb = NULL;
            min_level=max_level =1;
         }

      } _NODE_CFLOW_INFO;
      typedef mempool_allocator< _NODE_CFLOW_INFO > _NODE_CFLOW_INFO_ALLOC;
      typedef vector<_NODE_CFLOW_INFO, _NODE_CFLOW_INFO_ALLOC> _NODE_CFLOW_VECT;
      typedef _NODE_CFLOW_VECT::iterator _NODE_CFLOW_VECT_ITER;
      _NODE_CFLOW_VECT _bb_node_cflow_info;
      void Compute_PU_BB (void);
      BS *_reach_info_vect (BB *bb);
      BS *_create_empty_reach_bb_vect ();
      BS *_add_reachable_bb (BB *from, BB *to);
      BS *_add_reachable_bbs (BB *from, BS *reach_bbs);
      BS *_set_bb_is_reachable (BS *reach_vect, BB *bb);
      BOOL _is_bb_reachable (BS *reach_vect, BB *bb);
      void _init_data_member (void);
public :
      PU_REGION_CFLOW_MGR (void): 
         _bb_node_cflow_info (_NODE_CFLOW_INFO_ALLOC (&_mem_pool)) {
         _init_data_member ();
      }
      ~PU_REGION_CFLOW_MGR(void) {};
      void Set_Up_Reachable_Info ();
      BOOL BB1_Reachable_To_BB2 (BB *from, BB* to);
};
extern PU_REGION_CFLOW_MGR pu_region_cflow_mgr;
extern void Optimize_For_Needless_OP ();
extern REGISTER_SET Suitable_Reg_Set_For_EBO;
extern REGISTER_SET Destroied_Reg_Set;
extern REGISTER_SET Search_Proper_Reg_For_Ld_Through_Dom (BB *check_bb, ST *mem_loc, ISA_REGISTER_CLASS regclass);
extern REGISTER_SET All_St_Ld_Regs_In_Dom_Set (BB *check_bb,ISA_REGISTER_CLASS regclass);
extern REGISTER_SET Collect_Result_Regs (OP *start_op, OP *end_op,ISA_REGISTER_CLASS regclass);
extern REGISTER_SET Used_Regs_Of_St_Ld_In_BB (BB *bb,ISA_REGISTER_CLASS regclass);
extern REGISTER_SET Used_Regs_Of_St_Ld_In_BB (BB *bb, ST *mem_loc);
extern MEM_POOL opt_after_lra_for_mem_op;
typedef STACK<BB *> STACK_OF_BBS_BETWEEN_DOM_SET;
typedef std::list<BB *> LIST_SEARCH;
typedef struct  OP_REGS_INFO_P
{
   ST *mem_loc;
   OP *op;
   INT opnum;
   REGISTER reg;
} OP_REGS_INFO_STRUCT;

typedef struct BB_REGS_INFO_P
{
   REGISTER_SET *st_used_regs;
   REGISTER_SET *ld_used_regs;
   VECTOR *st_op_reg_info_vector;  
   VECTOR *ld_op_reg_info_vector;  
} BB_REGS_INFO_STRUCT;

extern void **Init_Vector_Info_For_Optimized_LRA ();
extern void Initialize_Optimized_LRA_And_EBO ();
extern void Finalize_Optimized_LRA_And_EBO ();
extern void Init_Mem_Pool_For_Optimized_LRA ();
extern BOOL Spill_Of_GRA (OP *op, BB *bb, TN *tn);
extern BOOL Spill_Of_LRA (OP *op, BB *bb, TN *tn);
extern BS *Loop_BBs_Set;
extern BS *Pick_Out_Loop_BB (PU_REGION_CFLOW_MGR *rgn_mgr);


extern OP_REGS_INFO_STRUCT * New_One (ST *mem_loc,OP *op, INT opnum, REGISTER reg);
extern REGISTER_SET Used_Regs_Of_St_In_BB (BB *bb, ST *mem_loc);
extern REGISTER_SET Search_Try_Use_Regs (BB *bb, ST *mem_loc);
extern REGISTER_SET All_St_Regs_In_Dom_Set (BB *check_bb);
extern REGISTER Search_Used_Regs_Of_Ld (ST *mem_loc, BB *bb, ISA_REGISTER_CLASS regclass);
extern void Reset_OP_Reg_Vector_BB (BB *bb);
extern BB_REGS_INFO_STRUCT **BB_REGS_VECTOR;
extern REGISTER_SET Search_St_Reg_For_Ld_Through_Dom (BB *check_bb, ST *mem_loc);
extern REGISTER_SET Process_For_LCM_Delay_Slot (OP *op, TN *alloc_tn, BB *check_bb);
extern REGISTER_SET Used_Regs_Of_All_Ld_In_BB (BB *bb, OP *op, ISA_REGISTER_CLASS regclass);
extern void Optimize_Ld_In_Dom ();
extern void Check_St_Ld_Reg_For_Ld_Through_Dom (BB *check_bb, ST *mem_loc, REGISTER reg, OP *check_op, ISA_REGISTER_CLASS regclass);
extern BOOL Local_Check_St_Ld_Reg_For_Ld_Through_Dom (BB *check_bb, ST *mem_loc, OP *check_op);

// All reg_set types we need to consider
enum REGISTER_SET_IN_ALLOC{
  ALL_ST_LD_IN_DOM_BB,			// all regs of st/ld in dom set bb
  REG_USED_BY_SAME_SPILL_IN_DOM_SET,	// regs which is used by st/ld 
                                    	// of the same spill in dom set bb
  ALL_RESULT_REGS_IN_BB_AFTER_OP,	// all def regs in the bb after the current op
  PREFER_REGS_OF_OTHER_LR_IN_BB,	// all prefer regs of the tn before the op
  ONLY_USED_REGS_BY_LD_IN_BB,		// all regs used by ld in current bb after op
  REGS_MAY_DESTROY_LCM_DELAY_SLOT,	// regs which may corrupt the 
                                  	// delay slot schedule in LCM
  REG_OF_SAME_SPILL_LD_IN_BB,		// use the reg of ld in current bb
  TRY_NOT_USE
};
extern void Optimize_St_In_Entry_BB ();
extern BOOL Worth_Perform_Sorted_GRA ();
extern BOOL St_Is_Redundant (BB *check_bb, ST *mem_loc, OP *check_op);
#define REGISTER_SET_IN_ALLOC_NUMBER 8
#endif
