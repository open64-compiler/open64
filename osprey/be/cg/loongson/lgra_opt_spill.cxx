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

#include "cg.h"
#include "lra.h"
#include "lgra_opt_spill.h"
#include "cgexp.h"
#include "ipfec_options.h"
#include "dominate.h"
#include "reg_live.h"
typedef BS REACH_INFO_VECT;
/******************************************************
 * Check through the region tree from bottom to up. Delete all
 * store which is needless
 ******************************************************/
struct OP_KIND
{
    int op_idx; // describe the relation in the same bb
    OP *op;
    BB *bb;
    OP_KIND *next;
} ;
struct MEMORY_LD_STORE_STRUCT
{
    TN *mem_offset_tn;
    struct OP_KIND *Load_List , *Store_List , *point_head_ld, *point_head_st;
};
BS *joint_set;
static VECTOR Load_Store_Vector ;
static VECTOR Will_Be_Deleted_Vector ;

#define MEMORY_VECTOR_element(v,i)  ((MEMORY_LD_STORE_STRUCT *)VECTOR_element(v,i))
#define Delete_VECTOR_element(v,i)  ((OP_KIND *)VECTOR_element(v,i))
#define Struct_TN(v) v->mem_offset_tn
#define Struct_Load(v) v->Load_List
#define Struct_Store(v) v->Store_List
#define Struct_Head_Ld(v) v->point_head_ld // last element in Load_List
#define Struct_Head_St(v) v->point_head_st // last element in Store_List
#define Struct_OP(v) v->op
#define Struct_BB(v) v->bb
#define Struct_IDX(v) v->op_idx
#define Struct_OP_Next(v)  v->next

/* Just check the tn 's spill_name */

static inline
BOOL GRA_LRA_Spilled(TN *tn)
{
    char* c = ST_name(TN_spill(tn));
    return
        (strstr(ST_name(TN_spill(tn)), "gra_spill_temp") ||
         strstr(ST_name(TN_spill(tn)), "lgra_spill_temp") ||
         strstr(ST_name(TN_spill(tn)), "lra_spill_temp")
        );
}

static void
Init_Delete_Vector(MEM_POOL *pool)
{
    Will_Be_Deleted_Vector = VECTOR_Init(50000, pool);
    VECTOR_Add_Element(Will_Be_Deleted_Vector , NULL);
}

static void
Init_Load_Store_Struct_Vector(MEM_POOL *pool)
{
    Load_Store_Vector = VECTOR_Init(50000, pool);
    VECTOR_Add_Element(Load_Store_Vector, NULL);
}

OP_KIND *
New_OP_Kind(OP* op, BB* bb , int op_idx)
{
    OP_KIND *new_load = TYPE_PU_ALLOC(OP_KIND);
    Struct_OP(new_load) = op;
    Struct_BB(new_load) = bb;
    Struct_IDX(new_load) = op_idx;
    Struct_OP_Next(new_load) = NULL;
    return new_load ;
}

MEMORY_LD_STORE_STRUCT *
New_Memory_Struct(BOOL is_load, TN* tn, OP* op, BB* bb, int op_idx)
{
    MEMORY_LD_STORE_STRUCT *Memory_op = TYPE_PU_ALLOC(MEMORY_LD_STORE_STRUCT);
    OP_KIND * op_kind =  New_OP_Kind(op, bb, op_idx);
    Struct_TN(Memory_op) = tn;
    if (is_load)
    {
        Struct_Load(Memory_op) = op_kind;
        Struct_Head_Ld(Memory_op) = op_kind;
    }
    else
    {
        Struct_Store(Memory_op) = op_kind;
        Struct_Head_St(Memory_op) = op_kind;
    }
    return Memory_op;
}

/****************************************************************
 * Check the memory of load, add the item for it.
 ****************************************************************/
BOOL
Is_Spill_Load(OP *op)
{

    if (OP_load(op))
    {
        BOOL is_spill_load = FALSE;
        for (int i = 0; i < OP_opnds(op); i++)
        {
            TN *spill_tn =  OP_opnd(op, i);
            if (TN_is_register(spill_tn)) continue;
            if (TN_spill(spill_tn) && GRA_LRA_Spilled(spill_tn))
            {
                return TRUE;
            }
        }
    }
    return FALSE;
}

void
Check_Load_OP(OP *op, BB *bb, int op_idx)
{
    TN *opnd_tn = OP_opnd(op, 1); // symbol tn
    int tnnum = 1;
    BOOL found = FALSE;

    if (!Is_Spill_Load(op)) return ;

    while (tnnum < VECTOR_count(Load_Store_Vector))
    {
        MEMORY_LD_STORE_STRUCT *m_struct =
            MEMORY_VECTOR_element(Load_Store_Vector , tnnum);
        if (Struct_TN(m_struct) == opnd_tn)
        {
            found = TRUE;
            // find the memory_tn , so insert the op in the list of the struct.
            OP_KIND *new_load = New_OP_Kind(op, bb , op_idx) ;
            if (Struct_Head_Ld(m_struct) != NULL)
            {
                Struct_OP_Next(Struct_Head_Ld(m_struct)) = new_load;
            }
            else
            {
                Struct_Load(m_struct) = new_load ;
            }
            Struct_Head_Ld(m_struct) = new_load;
            break;
        }
        else
        {
            tnnum ++;
        }
    }
    if (! found)
    {
        MEMORY_LD_STORE_STRUCT *m_struct = New_Memory_Struct(1, opnd_tn , op, bb , op_idx);
        VECTOR_Add_Element(Load_Store_Vector , m_struct);
    }
}

BOOL
Is_Spill_Store(OP *op)
{
    if (OP_store(op))
    {
        // has relation with arch. 2 is the offset of sp, sorry to use 2 here
        TN *opnd_tn = OP_opnd(op, 2);
        BOOL is_spill_store = FALSE;
        for (int i = 0; i < OP_opnds(op); i++)
        {
            TN *spill_tn = OP_opnd(op, i);
            if (TN_is_register(spill_tn)) continue;
            if (TN_spill(spill_tn) && GRA_LRA_Spilled(spill_tn))
            {
                return TRUE;
            }
        }
    }
    return FALSE;
}

void
Check_Store_OP(OP *op, BB *bb , int op_idx)
{
    // has relation with arch. 2 is the offset of sp
    TN *opnd_tn = OP_opnd(op, 2);
    int tnnum = 1;
    BOOL found = FALSE;

    if (!Is_Spill_Store(op)) return;

    while (tnnum < VECTOR_count(Load_Store_Vector))
    {
        MEMORY_LD_STORE_STRUCT *m_struct =
            MEMORY_VECTOR_element(Load_Store_Vector , tnnum);
        if (Struct_TN(m_struct) == opnd_tn)
        {
            found = TRUE;
            // find the memory_tn , so insert the op in the list of the struct.
            OP_KIND *new_store = New_OP_Kind(op, bb, op_idx) ;
            if (Struct_Head_St(m_struct) != NULL)
            {
                Struct_OP_Next(Struct_Head_St(m_struct)) = new_store;
            }
            else
            {
                Struct_Store(m_struct) = new_store;
            }
            Struct_Head_St(m_struct) = new_store;
            break;
        }
        else
        {
            tnnum ++;
        }
    }
    if (! found)
    {
        MEMORY_LD_STORE_STRUCT *m_struct = New_Memory_Struct(0, opnd_tn , op, bb , op_idx);
        VECTOR_Add_Element(Load_Store_Vector , m_struct);
    }
}

/***********************************************************************
 * Check load/store op one by one , add the need to vector
 ***********************************************************************/
void
Collect_Memory_OP(void)
{
    for (BB* bb = REGION_First_BB; bb; bb = BB_next(bb))
    {
        int op_idx = 0;
        for (OP* op = BB_first_op(bb) ; op ; op = OP_next(op))
        {

            if (OP_load(op))
                Check_Load_OP(op, bb, op_idx);
            else if (OP_store(op))
                Check_Store_OP(op, bb, op_idx);
            op_idx ++;
        }
    }
}

/***********************************************************************
 * in out-of-order arch, when we delete a st/ld , it may distroy the profits
 * of out-of-order. many instructions which can be exeute during out-of-order
 * will be stall
 * Here, we give a simple model for the prediction of cache miss. if there
 * is a mem op before the op being considered , we can consider to delete
 * the op .
 ***********************************************************************/
BOOL
Simple_Model_For_Performance(OP *op_c, BB *bb)
{
    TN *use_tn;
    ST *st , *base_st;
    INT64 base_ofst;

    Is_True(OP_store(op_c), ("The op under check must be a store op"));

    // for a st op, use the tn of symbol which is the offset of its mem loc
    TN *use_tn_c = OP_opnd(op_c, 2);
    ST *st_c = TN_spill(use_tn_c);
    ST *base_st_c;
    INT64  base_ofst_c;
    Base_Symbol_And_Offset(st_c, &base_st_c, &base_ofst_c);
    // if there are some memory_op before this op, we can delete it
    INT mem_op_before, mem_op_after;
    BOOL will_not_miss_op_c = TRUE;
    BOOL will_not_miss_op = FALSE;
    BOOL possible_miss = TRUE;
    mem_op_before = 0;
    mem_op_after = 0;
    OP *op;
    op = OP_prev(op_c);
    while (op)
    {
        if (Is_Spill_Store(op))
        {
            use_tn = OP_opnd(op, 2);
        }
        else if (Is_Spill_Load(op))
        {
            use_tn = OP_opnd(op, 1);
        }
        else
        {
            op = OP_prev(op);
            continue;
        }
        mem_op_before ++;
        st = TN_spill(use_tn);
        Base_Symbol_And_Offset(st, &base_st, &base_ofst);
        INT64 cache_line = base_ofst_c - base_ofst;
        if ((cache_line >= 31) || (cache_line <= -31))
        {
            op = OP_prev(op);
            continue;
        }
        // find the nearest op which is possible in the same cache line
        will_not_miss_op_c = TRUE;
        possible_miss = FALSE;
        break;
    }
    op = OP_next(op_c);
    BOOL succ_op_possible_miss = FALSE;
    while (op)
    {
        if (Is_Spill_Store(op))
        {
            use_tn = OP_opnd(op, 2);
        }
        else if (Is_Spill_Load(op))
        {
            use_tn = OP_opnd(op, 1);
        }
        else
        {
            op = OP_next(op);
            continue;
        }
        st = TN_spill(use_tn);
        Base_Symbol_And_Offset(st, &base_st, &base_ofst);
        INT64 cache_line = base_ofst - base_ofst_c;
        if ((cache_line <= 31) && (cache_line >= -31))
        {
            // find the nearest op which is possible in the same cache line
            will_not_miss_op = TRUE;
            succ_op_possible_miss = TRUE;
            break;
        }
        op = OP_next(op);
    }
    // BOOL can_not_be_delete =(! will_not_miss_op_c)&&(will_not_miss_op);
    BOOL can_not_be_delete = succ_op_possible_miss && possible_miss;
    // if it is the only mem_op, we can delete it
    // without feedback, a bb with freq bigger than 1000 is consider to be
    // hot, so , we delete the st under some condition in order not to
    // hurt the performance . for other code bb , just delete st directly
    if (!CG_PU_Has_Feedback)
    {
        if (!can_not_be_delete)
        {
            return TRUE;
        }
        else
        {
            return FALSE;
        }
    }
    else
    {
        if (!can_not_be_delete)
        {
            return TRUE;
        }
        else
        {
            return FALSE;
        }
    }
}

/**********************************************************
 * Check each item in vector, check st and ld relation
 **********************************************************/
void
Check_Through_Vector(PU_REGION_CFLOW_MGR *rgn_mgr)
{
    OP_KIND *p, *q;
    BB *store_bb, *load_bb;
    INT Min_BB_Freq = 10000;
    for (int num = 1; num < VECTOR_count(Load_Store_Vector); num ++)
    {
        MEMORY_LD_STORE_STRUCT *m_struct =
            MEMORY_VECTOR_element(Load_Store_Vector , num);
        OP_KIND *load_list = Struct_Load(m_struct);
        OP_KIND *store_list = Struct_Store(m_struct);
        q = store_list;
        while (q != NULL)
        {
            store_bb = Struct_BB(q);  // get the bb of tna
            if (BB_freq(store_bb) < Min_BB_Freq)
            {
                q = Struct_OP_Next(q);
                continue;
            }
            BOOL Can_Be_Deleted = TRUE;
            p = load_list;
            if (!BS_MemberP(Loop_BBs_Set, BB_id(store_bb)))
            {
                if (!IPFEC_Enable_Opt_Mem_OP)
                {
                    Can_Be_Deleted = FALSE;
                }
                else
                {
                    while (p != NULL)
                    {
                        load_bb = Struct_BB(p);
                        if ((load_bb != store_bb) &&
                                (rgn_mgr->BB1_Reachable_To_BB2(store_bb, load_bb)))
                        {
                            Can_Be_Deleted = FALSE;
                            break;
                        }
                        if (load_bb == store_bb)
                        {
                            if (Struct_IDX(p) > (Struct_IDX(q)))
                            {
                                Can_Be_Deleted = FALSE;
                                break;
                            }
                        }
                        p = Struct_OP_Next(p);
                    } // end while p
                }
            }
            else
            {
                if (!IPFEC_Enable_Opt_St_In_Loop)
                {
                    Can_Be_Deleted = FALSE;
                }
                else
                {
                    OP *check_op = Struct_OP(q);
                    TN *symbol_tn = Struct_TN(m_struct);
                    ST *mem_loc = TN_spill(symbol_tn);
                    Is_True(mem_loc , ("mem_loc should not be NULL"));
                    Can_Be_Deleted =
                        St_Is_Redundant(store_bb, mem_loc, check_op);
                }
            }
            if (Can_Be_Deleted)
            {
                OP* op = Struct_OP(q);
                BB* bb = Struct_BB(q);
                if (Simple_Model_For_Performance(op, bb))
                {
                    // maybe can be deleted here, I put it first.
                    VECTOR_Add_Element(Will_Be_Deleted_Vector , q);
                }
            }
            q = Struct_OP_Next(q);
        } // end while q
    }
}

void
CFLOW_Remove_Needless_Store(void)
{
    for (int num = 1; num < VECTOR_count(Will_Be_Deleted_Vector); num ++)
    {
        OP_KIND *op_kind = Delete_VECTOR_element(Will_Be_Deleted_Vector,
                           num);
        OP* op = Struct_OP(op_kind);
        BB* bb = Struct_BB(op_kind);
        Print_OP_No_SrcLine(op);
        BB_Remove_Op(bb, op);
    }
}

/************************************************************
 * Check though all the BBs of PU, delete those st who has no use
 * or def after it
 ************************************************************/
void
Optimize_For_Needless_OP()
{
    Init_Load_Store_Struct_Vector(&opt_after_lra_for_mem_op);
    Init_Delete_Vector(&opt_after_lra_for_mem_op);
    Collect_Memory_OP();
    Check_Through_Vector(&pu_region_cflow_mgr);
    CFLOW_Remove_Needless_Store();
}

void
PU_REGION_CFLOW_MGR::_init_data_member(void)
{
    _bb_num = 0;
}

void
PU_REGION_CFLOW_MGR::Compute_PU_BB(void)
{
    for (BB *bb = REGION_First_BB ; bb; bb = BB_next(bb))
    {
        if (_bb_num < BB_id(bb))
        {
            _bb_num = BB_id(bb);
        }
    }
}

BS *
PU_REGION_CFLOW_MGR::_create_empty_reach_bb_vect(void)
{
    return BS_Create_Empty(_bb_num + 2 , &_mem_pool);
}

void
PU_REGION_CFLOW_MGR::Set_Up_Reachable_Info()
{
    Compute_PU_BB();
    _bb_node_cflow_info.resize(_bb_num + 2);
    for (BB *bb = REGION_First_BB ; bb; bb = BB_next(bb))
    {
        _bb_node_cflow_info[BB_id(bb)].reach_bb =
            _create_empty_reach_bb_vect();
    }
    BS *union_set = BS_Create_Empty(_bb_num + 2  , &_mem_pool);
    BOOL change = TRUE;
    while (change)
    {
        change = FALSE;
        for (BB *bb = REGION_First_BB ; bb; bb = BB_next(bb))
        {
            BS_ClearD(union_set);
            REACH_INFO_VECT *reach_bb =  _bb_node_cflow_info[BB_id(bb)].reach_bb;
            for (BBLIST* succ = BB_succs(bb); succ; succ = BBLIST_next(succ))
            {
                BB* bb_succ = BBLIST_item(succ) ;
                REACH_INFO_VECT *succ_reach_bb = _bb_node_cflow_info[BB_id(bb_succ)].reach_bb;
                BS_UnionD(union_set , succ_reach_bb , NULL);
                if (! BS_MemberP(union_set , BB_id(bb_succ)))
                {
                    BS_Union1D(union_set , BB_id(bb_succ), NULL) ;
                }
            }
            if (! BS_EqualP(union_set , reach_bb))
            {
                BS_CopyD(reach_bb, union_set, NULL) ;
                change = TRUE;
            }
        }
    }
}

BOOL
PU_REGION_CFLOW_MGR::BB1_Reachable_To_BB2(BB *from, BB* to)
{
    BS *reach_info = _bb_node_cflow_info[BB_id(from)].reach_bb;
    Is_True(reach_info , ("bb's reachable info is not available"));
    if (BS_MemberP(reach_info , BB_id(to)))
    {
        return 1;
    }
    else
        return 0;
}

/* The following codes are for enhanced_lra and ld optimization before EBO */

MEM_POOL opt_after_lra_for_mem_op;
BB_REGS_INFO_STRUCT **BB_REGS_VECTOR;		// records for each BB
PU_REGION_CFLOW_MGR pu_region_cflow_mgr;
BS *Loop_BBs_Set;				// record all bb which belongs to a loop

#define OP_REG_VECTOR_element(v,i)  ((OP_REGS_INFO_STRUCT *)VECTOR_element(v,i))

OP_REGS_INFO_STRUCT * 
New_One(ST *mem_loc, OP *op, INT opnum, REGISTER reg)
{

    OP_REGS_INFO_STRUCT *new_one = TYPE_PU_ALLOC(OP_REGS_INFO_STRUCT);
    new_one->mem_loc = mem_loc;
    new_one->op = op;
    new_one->reg = reg;
    return new_one ;
}

/**********************************************************************
 * This is the Initialization Part for Optimized LRA ,
 * here it will initialize some vectors for LRA , and
 * some necessary info for LRA and EBO
 ***********************************************************************/
void
Initialize_Optimized_LRA_And_EBO()
{
    Init_Mem_Pool_For_Optimized_LRA();

    // Create necessary vectors for LRA
    // BB_REGS_VECTOR =Init_Vector_Info_For_Optimized_LRA();
    Init_Vector_Info_For_Optimized_LRA();
    pu_region_cflow_mgr.Set_Up_Reachable_Info();
    Loop_BBs_Set = Pick_Out_Loop_BB(&pu_region_cflow_mgr);

    // dominate set for bb
    Calculate_Dominators();
}

/**********************************************************************
 * Search all regs of st/ld in Dominate Set , if encouned with call in
 * any prev bb during the search,return
 **********************************************************************/
REGISTER_SET
Search_Proper_Reg_For_Ld_Through_Dom(
    BB *check_bb,
    ST *mem_loc,
    ISA_REGISTER_CLASS regclass)
{
    REGISTER_SET used_by_st_or_ld = REGISTER_SET_EMPTY_SET;
    BOOL try_search = TRUE;
    LIST_SEARCH list_search;
    BS *tried_bb = BS_Create_Empty(PU_BB_Count + 2, &opt_after_lra_for_mem_op);
    list_search.push_back(check_bb);
    BS_Union1D(tried_bb, BB_id(check_bb), &opt_after_lra_for_mem_op);
    BS *dom_set = BB_dom_set(check_bb);
    BOOL encount_with_call = FALSE;

    while (!list_search.empty())
    {
        BB *bb = list_search.front();
        list_search.pop_front();

        BBLIST *preds ;
        FOR_ALL_BB_PREDS(bb, preds)
        {
            BB * prev = BBLIST_item(preds);
            if (BB_call(prev))
            {
                encount_with_call = TRUE;
            }
            if (BS_MemberP(tried_bb, BB_id(prev)))
                continue;
            list_search.push_back(prev);
            BS_Union1D(tried_bb, BB_id(prev), &opt_after_lra_for_mem_op);
            if (BS_MemberP(dom_set, BB_id(prev)))
            {
                used_by_st_or_ld = Used_Regs_Of_St_Ld_In_BB(prev, mem_loc);
                if (encount_with_call)
                {
                    used_by_st_or_ld = REGISTER_SET_Difference(used_by_st_or_ld,
                                       REGISTER_CLASS_caller_saves(regclass));
                }
                if (used_by_st_or_ld != REGISTER_SET_EMPTY_SET)
                {
                    return used_by_st_or_ld;
                }
            }
        }
    }
    return used_by_st_or_ld;
}

/**********************************************************************
 * only find the nearest reg of st which can reach the current ld in dom
 **********************************************************************/
REGISTER_SET
Search_St_Reg_For_Ld_Through_Dom(BB *check_bb, ST *mem_loc)
{
    REGISTER_SET used_by_st = REGISTER_SET_EMPTY_SET;
    BOOL try_search = TRUE;
    LIST_SEARCH list_search;
    BS *tried_bb = BS_Create_Empty(PU_BB_Count + 2, &opt_after_lra_for_mem_op);
    list_search.push_back(check_bb);
    BS_Union1D(tried_bb, BB_id(check_bb), &opt_after_lra_for_mem_op);
    BS *dom_set = BB_dom_set(check_bb);
    BOOL encount_with_call = FALSE;

    if (BB_call(check_bb))
        return used_by_st;
    while (!list_search.empty() && (!encount_with_call))
    {
        BB *bb = list_search.front();
        list_search.pop_front();

        BBLIST *preds ;
        FOR_ALL_BB_PREDS(bb, preds)
        {
            BB * prev = BBLIST_item(preds);
            if (BB_call(prev))
            {
                encount_with_call = TRUE;
                break;
            }
            if (BS_MemberP(tried_bb, BB_id(prev)))
                continue;
            list_search.push_back(prev);
            BS_Union1D(tried_bb, BB_id(prev), &opt_after_lra_for_mem_op);
            if (BS_MemberP(dom_set, BB_id(prev)))
            {
                used_by_st = Used_Regs_Of_St_In_BB(prev, mem_loc);
                if (used_by_st != REGISTER_SET_EMPTY_SET)
                {
                    return used_by_st;
                }
            }
        }
    }
    return used_by_st;
}

/**********************************************************************
 * Initialize all vectors
 ***********************************************************************/
void **Init_Vector_Info_For_Optimized_LRA()
{

    INT REG_SET_NUMBER = 10;
    INT VECTOR_LENGTH = 1000;
    BB_REGS_VECTOR = TYPE_MEM_POOL_ALLOC_N(
                         BB_REGS_INFO_STRUCT *,
                         &opt_after_lra_for_mem_op,
                         PU_BB_Count + 2);
    for (int i = 0; i < PU_BB_Count + 2; i++)
    {
        BB_REGS_VECTOR[i] = TYPE_MEM_POOL_ALLOC_N(
                                BB_REGS_INFO_STRUCT , &opt_after_lra_for_mem_op, 1);
        BB_REGS_VECTOR[i]->ld_used_regs = TYPE_MEM_POOL_ALLOC_N(
                                              REGISTER_SET ,
                                              &opt_after_lra_for_mem_op,
                                              REG_SET_NUMBER);
        BB_REGS_VECTOR[i]->st_used_regs = TYPE_MEM_POOL_ALLOC_N(
                                              REGISTER_SET ,
                                              &opt_after_lra_for_mem_op,
                                              REG_SET_NUMBER);
        BB_REGS_VECTOR[i]->ld_op_reg_info_vector = TYPE_MEM_POOL_ALLOC_N(
                    VECTOR ,
                    &opt_after_lra_for_mem_op,
                    REG_SET_NUMBER);
        BB_REGS_VECTOR[i]->st_op_reg_info_vector = TYPE_MEM_POOL_ALLOC_N(
                    VECTOR ,
                    &opt_after_lra_for_mem_op,
                    REG_SET_NUMBER);
        ISA_REGISTER_CLASS cl;
        FOR_ALL_ISA_REGISTER_CLASS(cl)
        {

            BB_REGS_VECTOR[i]->ld_used_regs[cl] = REGISTER_SET_EMPTY_SET;
            BB_REGS_VECTOR[i]->st_used_regs[cl] = REGISTER_SET_EMPTY_SET;
            BB_REGS_VECTOR[i]->ld_op_reg_info_vector[cl] =
                VECTOR_Init(VECTOR_LENGTH,
                            &opt_after_lra_for_mem_op);
            VECTOR_Add_Element(BB_REGS_VECTOR[i]->ld_op_reg_info_vector[cl] ,
                               NULL);
            BB_REGS_VECTOR[i]->st_op_reg_info_vector[cl] =
                VECTOR_Init(VECTOR_LENGTH,
                            &opt_after_lra_for_mem_op);
            VECTOR_Add_Element(BB_REGS_VECTOR[i]->st_op_reg_info_vector[cl] ,
                               NULL);
        }
    }
};

void
Reset_OP_Reg_Vector_BB(BB *bb)
{
    INT bb_id = BB_id(bb);
    BB_REGS_INFO_STRUCT *bb_vec = BB_REGS_VECTOR[bb_id];
    ISA_REGISTER_CLASS cl;
    FOR_ALL_ISA_REGISTER_CLASS(cl)
    {
        VECTOR_Reset(BB_REGS_VECTOR[bb_id]->ld_op_reg_info_vector[cl]);
        VECTOR_Add_Element(BB_REGS_VECTOR[bb_id]->ld_op_reg_info_vector[cl] ,
                           NULL);
        VECTOR_Reset(BB_REGS_VECTOR[bb_id]->st_op_reg_info_vector[cl]);
        VECTOR_Add_Element(BB_REGS_VECTOR[bb_id]->st_op_reg_info_vector[cl] ,
                           NULL);
    }

}

/*********************************************************
 * All regs used by ld in the current bb
 *********************************************************/
REGISTER_SET
Used_Regs_Of_All_Ld_In_BB(BB *bb, OP *check_op, ISA_REGISTER_CLASS regclass)
{
    OP *op = check_op;
    REGISTER_SET ld_regs = REGISTER_SET_EMPTY_SET;
    while (op != BB_last_op(bb))
    {
        if (OP_load(op))
        {
            TN *tn = OP_result(op, 0);
            REGISTER reg = TN_register(tn);
            if ((reg != REGISTER_UNDEFINED) && (reg < REGISTER_MAX) &&
                    (TN_register_class(tn) == regclass))
            {

                ld_regs = REGISTER_SET_Union1(ld_regs, reg);
            }

        }
        op = OP_next(op);
    }
    return ld_regs;
}

/****************************************************************
 * All registers used by ld and st in Dominate set
 ****************************************************************/
REGISTER_SET
Used_Regs_Of_St_Ld_In_BB(BB *bb, ISA_REGISTER_CLASS regclass)
{
    INT id = BB_id(bb);
    BB_REGS_INFO_STRUCT *bb_vec = BB_REGS_VECTOR[id];
    REGISTER_SET regs_ld = bb_vec->ld_used_regs[regclass]  ;
    REGISTER_SET regs_st = bb_vec->st_used_regs[regclass]  ;
    REGISTER_SET regs = REGISTER_SET_Union(regs_ld, regs_st);
    if (regs != REGISTER_SET_EMPTY_SET)
    {
        return regs;
    }
}

/************************************************************
 * Call regs which are used by spilled st/ld in the current bb
 ************************************************************/
REGISTER_SET
Used_Regs_Of_St_Ld_In_BB(BB *bb, ST *mem_loc)
{
    REGISTER_SET used_by_st_or_ld = REGISTER_SET_EMPTY_SET;
    for (OP *op = BB_last_op(bb); op; op = OP_prev(op))
    {
        TN *tn = NULL;
        if (Is_OP_Spill_Store(op, mem_loc))
        {
            tn = OP_opnd(op, 1);
        }
        else if (Is_OP_Spill_Load(op, mem_loc))
        {
            tn = OP_result(op, 0);
        }
        if (tn != NULL)
        {
            REGISTER reg = TN_register(tn);
            if ((reg != REGISTER_UNDEFINED) && (reg < REGISTER_MAX))
            {
                return REGISTER_SET_Union1(used_by_st_or_ld, reg);
            }
        }
    }
    return used_by_st_or_ld;
}

REGISTER_SET
Used_Regs_Of_St_Ld_In_BB(BB *bb, ST *mem_loc, OP **stop_op, OP **st_op)
{
    REGISTER_SET used_by_st = REGISTER_SET_EMPTY_SET;
    OP *op = BB_last_op(bb);
    while (op)
    {
        TN *tn = NULL;
        *st_op = op;
        if (Is_OP_Spill_Store(op, mem_loc))
        {
            tn = OP_opnd(op, 1);
        }
        else if (Is_OP_Spill_Load(op, mem_loc))
        {
            tn = OP_result(op, 0);
        }
        if (tn != NULL)
        {
            REGISTER reg = TN_register(tn);
            if ((reg != REGISTER_UNDEFINED) && (reg < REGISTER_MAX))
            {
                return REGISTER_SET_Union1(used_by_st, reg);
            }
        }
        *stop_op = op;
        op = OP_prev(op);

    }
    return used_by_st;
}

/****************************************************************
 * All regs of spilled st  in Current BB
 *****************************************************************/
REGISTER_SET
Used_Regs_Of_St_In_BB(BB *bb, ST *mem_loc)
{
    REGISTER_SET used_by_st = REGISTER_SET_EMPTY_SET;

    for (OP *op = BB_last_op(bb); op; op = OP_prev(op))
    {
        if (Is_OP_Spill_Store(op, mem_loc))
        {
            TN *st_tn = OP_opnd(op, 1); // st value of op
            REGISTER reg = TN_register(st_tn);
            if ((reg != REGISTER_UNDEFINED) && (reg < REGISTER_MAX))
            {
                return REGISTER_SET_Union1(used_by_st, reg);
            }
        }
    }
    return used_by_st;
}

REGISTER_SET
All_St_Ld_Regs_In_Dom_Set(BB *check_bb, ISA_REGISTER_CLASS regclass)
{
    REGISTER_SET used_by_st_or_ld = REGISTER_SET_EMPTY_SET;
    BOOL try_search = TRUE;
    LIST_SEARCH list_search;
    BS *tried_bb = BS_Create_Empty(PU_BB_Count + 2, &opt_after_lra_for_mem_op);
    list_search.push_back(check_bb);
    BS_Union1D(tried_bb, BB_id(check_bb), &opt_after_lra_for_mem_op);
    BS *dom_set = BB_dom_set(check_bb);
    BOOL encount_with_call = FALSE;

    while (!list_search.empty())
    {
        BB *bb = list_search.front();
        list_search.pop_front();

        BBLIST *preds ;
        FOR_ALL_BB_PREDS(bb, preds)
        {
            BB * prev = BBLIST_item(preds);
            if (BB_call(prev))
            {
                encount_with_call = TRUE;
            }
            if (BS_MemberP(tried_bb, BB_id(prev)))
            {
                continue;
            }
            list_search.push_back(prev);
            BS_Union1D(tried_bb, BB_id(prev), &opt_after_lra_for_mem_op);
            if (BS_MemberP(dom_set, BB_id(prev)))
            {
                REGISTER_SET diff = REGISTER_SET_EMPTY_SET;
                if (encount_with_call)
                {
                    diff = REGISTER_SET_Difference(
                               Used_Regs_Of_St_Ld_In_BB(prev, regclass),
                               REGISTER_CLASS_caller_saves(regclass));

                }
                used_by_st_or_ld =
                    REGISTER_SET_Union(used_by_st_or_ld,
                                       diff);
            }
        }
    }
    return used_by_st_or_ld;
}

/*************************************************************
 * search all prev bb ,try not use what the st uses
 *************************************************************/
REGISTER
Search_Used_Regs_Of_Ld(ST *mem_loc, BB *bb, ISA_REGISTER_CLASS regclass)
{
    INT id = BB_id(bb);
    BB_REGS_INFO_STRUCT *bb_vec = BB_REGS_VECTOR[id];
    if (bb_vec == NULL) return REGISTER_UNDEFINED;

    REGISTER_SET ld_used_regs = bb_vec->ld_used_regs[regclass];

    VECTOR vec = bb_vec->ld_op_reg_info_vector[regclass]  ;
    INT length = VECTOR_count(vec);
    if (length > 1)
    {
        for (int i = length - 1; i > 0; i--)
        {
            OP_REGS_INFO_STRUCT *mem = OP_REG_VECTOR_element(vec, i);
            if (mem->mem_loc == mem_loc)
                return mem->reg;
        }
    }
    return REGISTER_UNDEFINED;
}

BOOL
Spill_Of_GRA(OP *op, BB *bb, TN *tn)
{
    Is_True(OP_store(op) || OP_load(op), ("Should check St/ld here"));
    ST *spill_loc = TN_spill(tn);

    if (!spill_loc)
    {
        return FALSE;
    }
    if (!(Is_OP_Spill_Store(op, spill_loc) || Is_OP_Spill_Load(op, spill_loc)))
    {
        return FALSE;
    }
    if (strstr(ST_name(spill_loc), "gra_spill_temp"))
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

BOOL
Spill_Of_LRA(OP *op, BB *bb, TN *tn)
{
    Is_True(OP_store(op) || OP_load(op), ("Should check St/ld here"));
    ST *spill_loc = TN_spill(tn);

    if (!spill_loc)
    {
        return FALSE;
    }
    if (!(Is_OP_Spill_Store(op, spill_loc) || Is_OP_Spill_Load(op, spill_loc)))
    {
        return FALSE;
    }
    if (strstr(ST_name(spill_loc), "lra_spill_temp") &&
            strstr(ST_name(spill_loc), "lgra_spill_temp"))
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

void
Init_Mem_Pool_For_Optimized_LRA()
{
    MEM_POOL_Initialize(&opt_after_lra_for_mem_op,
                        "optmized_lra_mem_pool",
                        FALSE);
    MEM_POOL_Push(&opt_after_lra_for_mem_op);
}

void
Finalize_Optimized_LRA_And_EBO()
{
    Free_Dominators_Memory();
    MEM_POOL_Pop(&opt_after_lra_for_mem_op);
    MEM_POOL_Delete(&opt_after_lra_for_mem_op);
}

/*************************************************************
 * because after annotaion, some entry bb of loops may be
 * otpimized Here , we have to add this ugly part to make
 * them out
 **************************************************************/
BS *
Pick_Out_Loop_BB(PU_REGION_CFLOW_MGR *rgn_mgr)
{
    BS *loop_bbs_set = BS_Create_Empty(PU_BB_Count + 2,
                                       &opt_after_lra_for_mem_op);

    for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
    {
        if (rgn_mgr->BB1_Reachable_To_BB2(bb, bb))
        {
            BS_Union1D(loop_bbs_set, BB_id(bb), &opt_after_lra_for_mem_op);
        }
    }
    return loop_bbs_set;
}

/*************************************************************
 * Search for the redefination reg in a BB which can be allocated
 * to other tn
 **************************************************************/
REGISTER_SET
Collect_Redefination_Regs_In_BB(BB *check_bb, ISA_REGISTER_CLASS regclass)
{
    REGISTER_SET exposed_use = REGISTER_SET_EMPTY_SET;
    REGISTER_SET redefination = REGISTER_SET_EMPTY_SET ;

    for (OP *op = BB_first_op(check_bb); op; op = OP_next(op))
    {
        for (int i = 0; i < OP_opnds(op); i++)
        {
            TN *tn = OP_opnd(op, i);
            if (tn == NULL || TN_is_constant(tn) || TN_register_class(tn) != regclass)
                continue;
            REGISTER reg = TN_register(tn);
            if ((reg != REGISTER_UNDEFINED) && (reg < REGISTER_MAX))
            {
                if (!REGISTER_SET_Intersection1(redefination, reg))
                {
                    exposed_use = REGISTER_SET_Union1(exposed_use, reg);
                }
            }
        }
        for (int i = 0; i < OP_results(op); i++)
        {
            TN *tn = OP_result(op, i);
            if (tn == NULL || TN_is_constant(tn) || TN_register_class(tn) != regclass)
                continue;
            REGISTER reg = TN_register(tn);
            if ((reg != REGISTER_UNDEFINED) && (reg < REGISTER_MAX))
            {
                if (!REGISTER_SET_Intersection1(exposed_use, reg))
                {
                    redefination = REGISTER_SET_Union1(redefination, reg);
                }
            }
        }

    }
    return redefination;
}

REGISTER_SET
Exposed_Used_Regs(BB *check_bb, TN *alloc_tn)
{
    REGISTER_SET exposed_used_regs = REGISTER_SET_EMPTY_SET;
    REGISTER_SET regset = REGISTER_SET_Union(exposed_used_regs, 0xffffffff);
    ISA_REGISTER_CLASS regclass = TN_register_class(alloc_tn);
    REGISTER reg;
    for (reg = REGISTER_SET_Choose(regset);
            reg != REGISTER_UNDEFINED;
            reg = REGISTER_SET_Choose_Next(regset, reg))
    {
        if (REG_LIVE_Into_BB(regclass, reg, check_bb))
        {
            exposed_used_regs = REGISTER_SET_Union1(exposed_used_regs,
                                                    reg);
        }
    }
    exposed_used_regs = REGISTER_SET_Difference(exposed_used_regs,
                                                Collect_Redefination_Regs_In_BB(check_bb, regclass));
    return exposed_used_regs;
}

/**************************************************************
 * In LCM , We must make sure that our allocation will not destroy
 * the delay slot
 * Here, we check a bb's pred , if pred is a branch bb, then the first
 * op the check_bb may be advanced to delay slot . so , we check the
 * des_tn of check_op, which is the first op of check_bb , try not
 * use the regs which live_out of branch_bb, or it will not be insert
 * to delay slot
 **************************************************************/
REGISTER_SET
Process_For_LCM_Delay_Slot(OP *op, TN *alloc_tn, BB *check_bb)
{

    REGISTER_SET try_not_use = REGISTER_SET_EMPTY_SET;
    if (OP_xfer(op) || OP_load(op) || OP_store(op))
    {
        return try_not_use;
    }

    BB *bb;
    if (BB_preds_len(check_bb) == 0) return try_not_use;
    BBLIST* bl;
    FOR_ALL_BB_PREDS(check_bb, bl)
    {
        bb = BBLIST_item(bl);
        if (BB_length(bb) == 0)
        {
            continue;
        }
        if (OP_xfer(BB_last_op(bb)))
        {
            BB *succ_b;
            BBLIST* succ_bl = BB_succs(bb);
            // find the hottest path
            FOR_ALL_BB_SUCCS(bb, succ_bl)
            {
                succ_b = BBLIST_item(succ_bl);
                if (succ_b != check_bb)
                {
                    try_not_use = REGISTER_SET_Union(try_not_use,
                                                     Exposed_Used_Regs(succ_b, alloc_tn));

                }
            }
        }
    }
    return try_not_use;
}

void
Optimize_Ld_In_Dom()
{
    BB_LIST *elist;
    BS *tried_bb = BS_Create_Empty(PU_BB_Count + 2, &opt_after_lra_for_mem_op);
    INT Min_BB_Freq = 100000;
    for (elist = Exit_BB_Head; elist; elist = BB_LIST_rest(elist))
    {
        for (BB *bb = BB_LIST_first(elist); bb; bb = BB_prev(bb))
        {
            if (!BS_MemberP(tried_bb, BB_id(bb)))
            {
                BS_Union1D(tried_bb, BB_id(bb), &opt_after_lra_for_mem_op);
                if (BB_freq(bb) < Min_BB_Freq)
                    continue;
                for (OP *op = BB_last_op(bb); op; op = OP_prev(op))
                {
                    if (OP_load(op))
                    {
                        TN *ld_tn = OP_result(op, 0);
                        ISA_REGISTER_CLASS regclass = TN_register_class(ld_tn);
                        TN *symbol_tn = OP_opnd(op, 1);
                        ST *spill_loc = TN_spill(symbol_tn);
                        REGISTER reg = TN_register(ld_tn);
                        if (symbol_tn && Spill_Of_GRA(op, bb, symbol_tn))
                        {
                            if (!Local_Check_St_Ld_Reg_For_Ld_Through_Dom(bb, spill_loc, op))
                                Check_St_Ld_Reg_For_Ld_Through_Dom(bb, spill_loc,
                                                                   reg, op, regclass);
                        }
                    }
                }
            }
        }
    }
}

REGISTER_SET
Search_For_Sts_To_Same_Loc(OP *start_op, OP *end_op, ST *mem_loc, BOOL *found)
{
    REGISTER_SET def_regs = REGISTER_SET_EMPTY_SET;

    OP *op = start_op;
    while (op && op != end_op)
    {
        if (OP_results(op) != 0)
        {
            TN *res = OP_result(op, 0);
            if ((TN_register(res) != REGISTER_UNDEFINED) &&
                    (TN_register(res) < REGISTER_MAX))
            {
                def_regs = REGISTER_SET_Union1(def_regs, TN_register(res));
            }

        }
        else if (Is_OP_Spill_Store(op, mem_loc))
        {
            *found = TRUE;
            def_regs = REGISTER_SET_EMPTY_SET;
            return def_regs ;
        }
        op = OP_prev(op);
    }
    if (op == end_op)
    {
        if (OP_results(op) != 0)
        {
            TN *res = OP_result(op, 0);
            if ((TN_register(res) != REGISTER_UNDEFINED) &&
                    (TN_register(res) < REGISTER_MAX))
            {
                def_regs = REGISTER_SET_Union1(def_regs, TN_register(res));
            }

        }
    }

    return def_regs;
}

/*******************************************************
 * Search the def regs from bottom to top
 *******************************************************/
REGISTER_SET
Collect_Result_Regs(OP *start_op, OP *end_op, ISA_REGISTER_CLASS regclass)
{
    REGISTER_SET def_regs = REGISTER_SET_EMPTY_SET;

    OP *op = start_op;
    while (op && op != end_op)
    {
        if (OP_results(op) != 0)
        {
            TN *res = OP_result(op, 0);
            if ((TN_register(res) != REGISTER_UNDEFINED) &&
                    (TN_register(res) < REGISTER_MAX) &&
                    (TN_register_class(res) == regclass))
            {
                def_regs = REGISTER_SET_Union1(def_regs, TN_register(res));
            }

        }
        op = OP_prev(op);
    }
    if (op == end_op)
    {
        if (OP_results(op) != 0)
        {
            TN *res = OP_result(op, 0);
            if ((TN_register(res) != REGISTER_UNDEFINED) &&
                    (TN_register(res) < REGISTER_MAX) &&
                    (TN_register_class(res) == regclass))
            {
                def_regs = REGISTER_SET_Union1(def_regs, TN_register(res));
            }

        }
    }

    return def_regs;
}

INT 
Search_St_Ld_In_Dom_BB(OP *start_op, OP *end_op, ST *mem_loc)
{

    OP *op = start_op;
    while (op && op != end_op)
    {
        if (Is_OP_Spill_Load(op, mem_loc))
        {
            return 1;
        }
        else if (Is_OP_Spill_Store(op, mem_loc))
        {
            Print_OP_No_SrcLine(op);
            return 2;
        }
        op = OP_next(op);
    }
    if (op == end_op)
    {
        if (Is_OP_Spill_Load(op, mem_loc))
        {
            return 1;
        }
        else if (Is_OP_Spill_Store(op, mem_loc))
        {
            return 2;
        }
    }

    return 0;
}

BOOL
Search_For_Ld_From_Same_Loc(OP *start_op, OP *end_op, ST *mem_loc)
{
    REGISTER_SET def_regs = REGISTER_SET_EMPTY_SET;

    OP *op = start_op;
    while (op && op != end_op)
    {
        if (Is_OP_Spill_Load(op, mem_loc))
        {
            return TRUE;
        }
        op = OP_prev(op);
    }
    if (op == end_op)
    {
        if (Is_OP_Spill_Load(op, mem_loc))
        {
            return TRUE;
        }
    }

    return FALSE;
}

BOOL
St_Is_Redundant(BB *check_bb, ST *mem_loc, OP *check_op)
{
    BOOL try_search = TRUE;
    BBLIST *succs = BB_succs(check_bb);
    LIST_SEARCH list_search;
    BS *tried_bb = BS_Create_Empty(PU_BB_Count + 2, &opt_after_lra_for_mem_op);
    list_search.push_back(check_bb);
    BS_Union1D(tried_bb, BB_id(check_bb), &opt_after_lra_for_mem_op);
    BS *dom_set = BB_pdom_set(check_bb);

    INT found = 0;
    while (!list_search.empty())
    {
        BOOL conflict_reg = FALSE;
        OP *op;
        BB *bb = list_search.front();
        list_search.pop_front();
        // check all successors for the ld from the same loc which
        // will use the value of st
        if (BB_length(bb) != 0)
        {
            if (check_bb == bb)
            {
                if (!BS_MemberP(Loop_BBs_Set, BB_id(check_bb)) &&
                        (check_op != BB_first_op(bb)))
                {
                    op = OP_next(check_op);
                    conflict_reg =
                        Search_For_Ld_From_Same_Loc(op,
                                                    BB_first_op(bb), mem_loc);
                }
                else
                {
                    if ((check_op != BB_first_op(bb)))
                    {
                        op = OP_prev(check_op);
                        conflict_reg = Search_For_Ld_From_Same_Loc(op, BB_first_op(bb), mem_loc);
                    }
                    if (check_op != BB_last_op(check_bb))
                    {
                        op = OP_next(check_op);
                        conflict_reg |= Search_For_Ld_From_Same_Loc(BB_last_op(bb), op, mem_loc);
                    }
                }
                if ((strstr(ST_name(mem_loc), "lgra_spill_temp")) ||
                        (strstr(ST_name(mem_loc), "lra_spill_temp")))
                {
                    return (!conflict_reg);
                }

            }
            else
            {
                conflict_reg = Search_For_Ld_From_Same_Loc(BB_last_op(bb), BB_first_op(bb), mem_loc);
            }
            if (conflict_reg) return FALSE;
        }
        FOR_ALL_BB_SUCCS(bb, succs)
        {
            BB * succ = BBLIST_item(succs);

            if (!BS_MemberP(tried_bb, BB_id(succ)))
            {
                BS_Union1D(tried_bb, BB_id(succ), &opt_after_lra_for_mem_op);
                if (BS_MemberP(dom_set, BB_id(succ)) && (BB_length(succ) != 0))
                {
                    found = Search_St_Ld_In_Dom_BB(BB_first_op(succ), BB_last_op(succ), mem_loc);
                    if (found == 1)
                    {
                        // there is ld in post-dom set , so can not delete
                        return FALSE;
                    }
                    else if (found == 0)
                    {
                        list_search.push_back(succ);
                    }
                }
                else
                {
                    list_search.push_back(succ);
                }
            }
        }
    }
    return TRUE;
}

/* return value  indicate whether we succeed in transforming the ld */
BOOL
Local_Check_St_Ld_Reg_For_Ld_Through_Dom(BB *check_bb, ST *mem_loc, OP *check_op)
{
    OP* end_op = BB_first_op(check_bb);
    OP* op = check_op;
    while (op && op != end_op)
    {
        op = OP_prev(op);
        if (Is_OP_Spill_Store(op, mem_loc) || Is_OP_Spill_Load(op, mem_loc))
        {

            OPS ops = OPS_EMPTY;
            TN *tgt_tn = OP_result(check_op, 0);// must be a ld_op
            TN *src_tn;
            if (OP_store(op))
            {
                // if it is a st op. fecth the store tn
                src_tn = OP_opnd(op, 1);
            }
            else
            {
                // if it is a ld op. fetch the ld tn
                src_tn = OP_result(op, 0);
            }
            REGISTER tgt_reg = TN_register(tgt_tn);
            REGISTER src_reg = TN_register(src_tn);
            if (tgt_reg == src_reg)
            {
                Print_OP_No_SrcLine(check_op);
                BB_Remove_Op(check_bb, check_op);
            }
            else
            {
                Print_OP_No_SrcLine(check_op);
                Exp_COPY(tgt_tn, src_tn, &ops);
                BB_Insert_Ops(check_bb, check_op, &ops, FALSE);
                BB_Remove_Op(check_bb, check_op);
            }

            return  TRUE;
        }
    }
    return FALSE;
}

/***********************************************************
 * Check whether there is a reg of st/ld who has the same loc
 * with ld under checking , if yes, then substitute the ld with
 * copy .if the checked_ld is in a loop, we should check whether
 * the founded st/ld is the first and the only one value who can
 * reach the ld . Here, we use check_which_half and for it .
 ************************************************************/
void
Check_St_Ld_Reg_For_Ld_Through_Dom(BB *check_bb, ST *mem_loc, REGISTER reg, OP *check_op, ISA_REGISTER_CLASS regclass)
{
    REGISTER_SET used_by_st_with_same_loc = REGISTER_SET_EMPTY_SET;
    REGISTER_SET def_regs_in_each_bb  = REGISTER_SET_EMPTY_SET;
    REGISTER_SET def_regs_in_all_bb  = REGISTER_SET_EMPTY_SET;
    BBLIST *preds ;
    LIST_SEARCH list_search;
    BB *end_bb = NULL;
    OP *end_op = NULL;
    OP *st_op_with_same_loc = NULL;
    BOOL encount_with_call = FALSE;
    // if check_which_half is 1, check half before op, if 2, check
    // half after op
    BOOL check_which_half = FALSE;


    BS *dom_set = BB_dom_set(check_bb);
    BS *tried_bb = BS_Create_Empty(PU_BB_Count + 2,
                                   &opt_after_lra_for_mem_op);
    list_search.push_back(check_bb);
    BS_Union1D(tried_bb, BB_id(check_bb), &opt_after_lra_for_mem_op);
    BOOL *found = TYPE_MEM_POOL_ALLOC_N(
                      BOOL , &opt_after_lra_for_mem_op, 1);
    *found = FALSE;
    while (!list_search.empty())
    {
        OP *op;
        BB *bb = list_search.front();
        list_search.pop_front();

        if (BB_length(bb) != 0)
        {
            if (check_bb == bb)
            {
                if (!BS_MemberP(Loop_BBs_Set, BB_id(check_bb)))
                {
                    if (check_op != BB_first_op(bb))
                    {
                        op = OP_prev(check_op);
                        def_regs_in_each_bb  =
                            Search_For_Sts_To_Same_Loc(op,
                                                       BB_first_op(bb), mem_loc, found);
                    }
                }
                else
                {
                    // a loop bb whose predessesor is itself
                    REGISTER_SET defs_before = REGISTER_SET_EMPTY_SET;
                    REGISTER_SET defs_after = REGISTER_SET_EMPTY_SET;
                    BOOL *found_before = TYPE_MEM_POOL_ALLOC_N(
                                             BOOL , &opt_after_lra_for_mem_op, 1);
                    BOOL *found_after = TYPE_MEM_POOL_ALLOC_N(
                                            BOOL , &opt_after_lra_for_mem_op, 1);
                    *found_before = *found_after = FALSE;
                    if ((check_op != BB_first_op(bb)) && (!check_which_half))
                    {
                        op = OP_prev(check_op);
                        defs_before = Search_For_Sts_To_Same_Loc(op, BB_first_op(bb), mem_loc, found_before);
                    }
                    else if (check_op != BB_last_op(check_bb) && (check_which_half))
                    {
                        op = OP_next(check_op);
                        defs_after = Search_For_Sts_To_Same_Loc(BB_last_op(bb), op, mem_loc, found_after);
                    }
                    def_regs_in_each_bb = REGISTER_SET_Union(defs_before, defs_after);
                    *found = *found_before || *found_after;
                }

            }
            else
            {
                def_regs_in_each_bb = Search_For_Sts_To_Same_Loc(BB_last_op(bb), BB_first_op(bb), mem_loc, found);
            }
            if (*found) return ;
            def_regs_in_all_bb = REGISTER_SET_Union(def_regs_in_all_bb, def_regs_in_each_bb);
        }
        FOR_ALL_BB_PREDS(bb, preds)
        {
            BB * prev = BBLIST_item(preds);
            if (prev == check_bb)
            {
                if (!check_which_half)
                {
                    check_which_half = TRUE;
                    list_search.push_back(prev);
                }
                continue;
            }
            if (!BS_MemberP(tried_bb, BB_id(prev)))
            {
                // When encount with a call, we only take callee-saved reg
                // into consideration
                if (BB_call(prev))
                {
                    encount_with_call = TRUE;
                }
                BS_Union1D(tried_bb, BB_id(prev), &opt_after_lra_for_mem_op);
                if (BS_MemberP(dom_set, BB_id(prev)))
                {
                    OP **stop_op = TYPE_MEM_POOL_ALLOC_N(
                                       OP* , &opt_after_lra_for_mem_op, 1);
                    OP **st_op = TYPE_MEM_POOL_ALLOC_N(
                                     OP* , &opt_after_lra_for_mem_op, 1);
                    used_by_st_with_same_loc = Used_Regs_Of_St_Ld_In_BB(prev, mem_loc, stop_op, st_op);
                    if ((used_by_st_with_same_loc != REGISTER_SET_EMPTY_SET))
                    {
                        if (!encount_with_call || (encount_with_call &&
                                                   REGISTER_SET_Intersection(used_by_st_with_same_loc,
                                                                             REGISTER_CLASS_callee_saves(regclass))))
                        {
                            end_bb = prev;
                            end_op = *stop_op;
                            st_op_with_same_loc = *st_op;
                        }
                        else
                        {
                            list_search.push_back(prev);
                        }
                    }
                    else
                    {
                        list_search.push_back(prev);
                    }
                }
                else
                {
                    list_search.push_back(prev);
                }
            }
        }
    }
    if (end_bb != NULL)
    {
        def_regs_in_each_bb =  Search_For_Sts_To_Same_Loc(BB_last_op(end_bb), end_op, mem_loc, found);
        // if there is some st to the same location ,or some redefination
        // to the same reg, return
        if (*found) return;
        // if there is a call among the check_bb and
        // the dom bb which has st/ld of same loc,
        // check whether the reg of st/ld is a caller reg,
        // if so , return
        if ((encount_with_call) &&
                REGISTER_SET_Intersection(used_by_st_with_same_loc,
                                          REGISTER_CLASS_caller_saves(regclass)))
            return;
        def_regs_in_all_bb = REGISTER_SET_Union(def_regs_in_all_bb, def_regs_in_each_bb);
        if ((used_by_st_with_same_loc != REGISTER_SET_EMPTY_SET) &&
                (!REGISTER_SET_Intersection(def_regs_in_all_bb,
                                            used_by_st_with_same_loc)))
        {
            OPS ops = OPS_EMPTY;
            TN *tgt_tn = OP_result(check_op, 0); // must be a ld_op
            TN *src_tn;
            if (OP_store(st_op_with_same_loc))
            {
                // if it is a st op. fecth the store tn
                src_tn = OP_opnd(st_op_with_same_loc, 1);
            }
            else
            {
                // if it is a ld op. fetch the ld tn
                src_tn = OP_result(st_op_with_same_loc, 0);
            }
            REGISTER tgt_reg = TN_register(tgt_tn);
            REGISTER src_reg = TN_register(src_tn);
            if (tgt_reg == src_reg)
            {
                Print_OP_No_SrcLine(check_op);
                BB_Remove_Op(check_bb, check_op);
            }
            else
            {
                Print_OP_No_SrcLine(check_op);
                Exp_COPY(tgt_tn, src_tn, &ops);
                BB_Insert_Ops(check_bb, check_op, &ops, FALSE);
                BB_Remove_Op(check_bb, check_op);
            }

        }
    }
}

OP *
Search_Copy_TN_For_St(OP *check_op, TN *check_tn, ST *mem_loc)
{
    OP *op = OP_prev(check_op);
    REGISTER_SET redef = REGISTER_SET_EMPTY_SET;
    while (op)
    {
        if ((CGTARG_Copy_Operand(op) > 0) &&
                (OP_result(op, 0) == check_tn) &&
                TN_register((OP_opnd(op, 1))) == 32)
        {
            REGISTER def_reg = TN_register(OP_result(op, 0));
            if (!REGISTER_SET_Intersection1(redef, def_reg))
            {
                Print_OP_No_SrcLine(op);
                return op;
            }
        }
        else
        {
            if (OP_results(op) != 0)
            {
                redef = REGISTER_SET_Union1(redef,
                                            TN_register(OP_result(op, 0)));

            }
            else
            {
                if (OP_store(op))
                {
                    TN *spill_tn = OP_opnd(op, 2);
                    ST *spill_loc = TN_spill(spill_tn);
                    if (spill_loc && spill_loc == mem_loc)
                    {
                        return NULL;
                    }
                }

            }
        }
        op = OP_prev(op);
    }
    return NULL;
}

/***********************************************************
 * An stupid opt for the redundant or and st of $31 in Entry BB
 * which will ignore by EBO, so perform this after EBO
 * Here, We assume that no other TN will def $31 except jr op
 ***********************************************************/
void
Optimize_St_In_Entry_BB()
{
    BB_LIST *elist;

    for (elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist))
    {
        BB *entry = BB_LIST_first(elist);
        if (BB_freq(entry) <= IPFEC_Enable_RA_OPT)
        {
            continue;
        }
        for (OP *op = BB_last_op(entry); op; op = OP_prev(op))
        {
            if (OP_store(op))
            {
                INT opndnum = 1;
                TN *st_tn = OP_opnd(op, 1);
                REGISTER reg = TN_register(st_tn);
                TN *mem_loc_tn = OP_opnd(op, 2);
                if (Spill_Of_GRA(op, entry, mem_loc_tn))
                {
                    OP *copy_op = Search_Copy_TN_For_St(op,
                                                        st_tn,
                                                        TN_spill(mem_loc_tn));

                    if (copy_op)
                    {
                        TN *copy_tn = OP_opnd(copy_op, 1);
                        Set_OP_opnd(op, opndnum, copy_tn);
                        BB_Remove_Op(entry, copy_op);
                    }
                }
            }
        }
    }
}

BOOL
Worth_Perform_Sorted_GRA()
{
    if (!IPFEC_Enable_Sorted_GRA)
        return FALSE;
    BB_LIST *elist;
    for (elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist))
    {
        BB *entry = BB_LIST_first(elist);
    }
    return TRUE;
}
