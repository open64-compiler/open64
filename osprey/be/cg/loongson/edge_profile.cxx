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

#include "bb.h"
#include "cg.h"
#include "defs.h"
#include "cg_region.h"
#include "label_util.h"
#include "edge_profile.h"
#include "symtab.h"
#include "data_layout.h"
#include "symtab_access.h"
#include "cg_flags.h"
#include "variants.h"
#include "tracing.h"
#include "instr_reader.h"
#include "dump_feedback.h"
#include "freq.h"
#include "ipfec_defs.h"
#include "gra_live.h"
#include "ipfec_options.h"
#include "cgexp_internals.h"
#include "cgexp.h"
#include "whirl2ops.h"

static WN *
Gen_Call_Shell(char *name, TYPE_ID rtype, INT32 argc)
{
    TY_IDX  ty = Make_Function_Type(MTYPE_To_TY(rtype));
    ST     *st = Gen_Intrinsic_Function(ty, name);

    Clear_PU_no_side_effects(Pu_Table[ST_pu(st)]);
    Clear_PU_is_pure(Pu_Table[ST_pu(st)]);
    Set_PU_no_delete(Pu_Table[ST_pu(st)]);

    WN *wn_call = WN_Call(rtype, MTYPE_V, argc, st);

    WN_Set_Call_Default_Flags(wn_call);
    return wn_call;
}

static WN *
Gen_Call(char *name, WN *arg1, WN *arg2, TYPE_ID rtype = MTYPE_V)
{
    WN *call = Gen_Call_Shell(name, rtype, 2);
    WN_actual(call, 0) = Gen_Param(arg1, WN_PARM_BY_VALUE);
    WN_actual(call, 1) = Gen_Param(arg2, WN_PARM_BY_VALUE);
    return call;
}

static WN *
Gen_Call(char *name, WN *arg1,  TYPE_ID rtype = MTYPE_V)
{
    WN *call = Gen_Call_Shell(name, rtype, 1);
    WN_actual(call, 0) = Gen_Param(arg1, WN_PARM_BY_VALUE);
    return call;
}

void CG_Edge_Profile_Instrument(CGRIN* cgrin,PROFILE_PHASE phase)
{
    // When the users don't specify the instru file, the instru file's name
    // will be defaulted to the source file's name. and the instru file name's
    // postfix is ".instr".
    extern char* Instrumentation_File_Name;
    if (Instrumentation_File_Name != NULL)
        Instru_File_Name = Instrumentation_File_Name;
    if (Instru_File_Name == NULL)
    {
        DevWarn("not specify the feedback file!use default feedback file\n");
        Instru_File_Name = Src_File_Name;
    }
    PU_PROFILE_HANDLES fb_handles; // empty for instrumentation
    EDGE_PROFILE edge_prof(cgrin,TRUE, phase, fb_handles);
    edge_prof.CG_Instrument();
}

void CG_Edge_Profile_Annotation(CGRIN* cgrin,PROFILE_PHASE phase)
{
    // srcfile_pu_name consists of  the source file name ,"/", and the current
    // PU name.
    char* srcfile_pu_name = CXX_NEW_ARRAY(char, strlen(Src_File_Name) +
                                          strlen("/") + strlen(Cur_PU_Name) + 1, &MEM_local_nz_pool);
    strcpy(srcfile_pu_name,Src_File_Name);
    strcat(srcfile_pu_name,"/");
    strcat(srcfile_pu_name,Cur_PU_Name);

    FREQ_Compute_BB_Frequencies();
    PU_PROFILE_HANDLES fb_handles
    = Get_CG_PU_Profile(srcfile_pu_name, Feedback_File_Info[phase]);
    if (fb_handles.empty())
    {
        // if there is no feedback info, use static profile
        DevWarn("Cannot find expected feedback data - function not called?");
        CG_PU_Has_Feedback = FALSE;
        FREQ_Compute_BB_Frequencies();
        Compute_Edge_Freq_Base_Prob();
        return;
    }

    FREQ_freqs_computed = TRUE;
    CG_PU_Has_Feedback = TRUE;

    edge_done= TRUE;

    if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
    {
        Dump_Fb_Data(fb_handles);
        DevWarn("find expected feedback data!");
    }
    Init_CFG();
    EDGE_PROFILE edge_prof(cgrin,FALSE, phase, fb_handles);
    edge_prof.CG_Annotate();
}

void EDGE_PROFILE::CG_Instrument()
{
    // if PU has only BB and BB's xfer op just is ret, do not
    // instrument
    int BB_Count;
    if ((Curr_PU_BB_Count() <= 1)
            && ((BB_xfer_op(_pu_first_bb) == NULL)
                || (OP_code(BB_xfer_op(_pu_first_bb)) == TOP_jr)))
        return;
    if (Get_Trace(TP_A_PROF,TT_PROF_CFG))
        ;
    // this is the main function for doing instrumentation.
    // It instruments in a PU.
    _instrument_count = 0;
    // instrument at each bb branch
    for (BB* bb =_pu_last_bb; bb != NULL;
            bb = BB_prev(bb))
    {
        if (BB_Is_For_Instru(bb))
            CG_Instrument_Node(bb);
    }

    Instrument_Entry();
    if (Get_Trace(TP_A_PROF,TT_PROF_CFG))
    {
        ;
    }
}

// ------------------------------------------------------------------
// Instrument before the PU's entry. Instrument BBs that call two
// functions "__profile_init" and "__profile_pu_init"
// ------------------------------------------------------------------
void EDGE_PROFILE::Instrument_Entry()
{
    char* outputfile_name = Instru_File_Name;
    PU_Has_Calls = TRUE;
    BB* bb = _pu_first_bb;
    BB_LIST *elist;
    BB *instru_pu_init_call_bb, *instru_init_call_bb;

    for (elist = Entry_BB_Head; elist != NULL; elist = BB_LIST_rest(elist))
    {
        bb = BB_LIST_first(elist);
        if (bb != NULL && !BB_call(bb))
        {
            op* op = BB_xfer_op(bb);
            if (op != NULL)
            {
                BBLIST* edge = BB_succs(bb);
                INT32 bblist_len =BBlist_Len(edge);
                while (edge != NULL && bblist_len--)
                {
                    BBLIST* nedge = edge;
                    edge = BBLIST_next(edge);

                    // generate two bb that call instrumentation lib function
                    BB* target_bb = BBLIST_item(nedge);
                    instru_init_call_bb = Gen_Call_BB(INSTR_INIT_NAME,
                                                      outputfile_name,_phase, RESTORE_NONE);
                    Set_BB_profile_added(instru_init_call_bb);
                    instru_pu_init_call_bb = Gen_Call_BB(PU_INIT_NAME,
                                                         _srcfile_pu_name,_instrument_count, RESTORE_NONE);
                    Set_BB_profile_added(instru_pu_init_call_bb);
                    LABEL_IDX instru_bb_label = Gen_Label_For_BB(instru_init_call_bb);

                    // tgt_label is the branch target bb's label
                    LABEL_IDX tgt_label;
                    if (Get_Label_BB(Get_Br_Op_Tgt_Label(op)) == target_bb)
                        tgt_label = Get_Br_Op_Tgt_Label(op);
                    else
                        tgt_label = Gen_Label_For_BB(target_bb);

                    if (BB_next(bb) == target_bb)
                    {
                        Insert_BB(instru_init_call_bb,bb);
                        Insert_BB(instru_pu_init_call_bb,instru_init_call_bb);
                        {
                            float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                            float original_prob = Prob_Local(bb,target_bb);
                            Unlink_Pred_Succ(bb,target_bb);
                            Link_Pred_Succ(bb,instru_init_call_bb);
                            Link_Pred_Succ(instru_init_call_bb,instru_pu_init_call_bb);
                            Link_Pred_Succ(instru_pu_init_call_bb,target_bb);

                            BB_freq(instru_init_call_bb) = original_freq;
                            BB_freq(instru_pu_init_call_bb) = original_freq;
                            Set_Prob(bb,instru_init_call_bb,original_prob);
                            Set_Prob(instru_init_call_bb,instru_pu_init_call_bb, 1.0);
                            Set_Prob(instru_pu_init_call_bb,target_bb,1.0);
                        }
                    } else
                    {
                        BB* goto_bb = Gen_BB();
                        Set_BB_profile_added(goto_bb);
                        BB* old_last_bb = Get_Pu_Last_BB();
                        Set_Pu_Last_BB(goto_bb);

                        Insert_BB(instru_init_call_bb,old_last_bb);
                        Insert_BB(instru_pu_init_call_bb,instru_init_call_bb);
                        Insert_BB(goto_bb,instru_pu_init_call_bb);
                        Add_Goto(goto_bb, target_bb);
                        {
                            float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                            float original_prob = Prob_Local(bb,target_bb);
                            Unlink_Pred_Succ(bb,target_bb);
                            Link_Pred_Succ(bb,instru_init_call_bb);
                            Link_Pred_Succ(instru_init_call_bb,instru_pu_init_call_bb);
                            Link_Pred_Succ(instru_pu_init_call_bb,goto_bb);
                            Link_Pred_Succ(goto_bb,target_bb);

                            BB_freq(instru_init_call_bb) = original_freq;
                            BB_freq(instru_pu_init_call_bb) = original_freq;
                            BB_freq(goto_bb) = original_freq;
                            Set_Prob(bb,instru_init_call_bb,original_prob);
                            Set_Prob(instru_init_call_bb,instru_pu_init_call_bb, 1.0);
                            Set_Prob(instru_pu_init_call_bb,goto_bb, 1.0);
                            Set_Prob(goto_bb,target_bb,1.0);
                        }
                        // TODO:check and use BB_branch_op or BB_xfer_op
                        Change_Tgt_Label(op,tgt_label,instru_bb_label);
                    }
                }
            } else
            {
                BB* instru_init_call_bb = Gen_Call_BB(INSTR_INIT_NAME,
                                                      outputfile_name,_phase,0);
                Set_BB_profile_added(instru_init_call_bb);
                BB* instru_pu_init_call_bb = Gen_Call_BB(PU_INIT_NAME,
                                             _srcfile_pu_name, _instrument_count,0);
                Set_BB_profile_added(instru_pu_init_call_bb);
                BB* target_bb = BBLIST_item(BB_succs(bb));
                Insert_BB(instru_init_call_bb,bb);
                Insert_BB(instru_pu_init_call_bb,instru_init_call_bb);

                {
                    float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                    float original_prob = Prob_Local(bb,target_bb);
                    Unlink_Pred_Succ(bb,target_bb);
                    Link_Pred_Succ(bb,instru_init_call_bb);
                    Link_Pred_Succ(instru_init_call_bb,instru_pu_init_call_bb);
                    Link_Pred_Succ(instru_pu_init_call_bb,target_bb);
                    BB_freq(instru_init_call_bb) = original_freq;
                    BB_freq(instru_pu_init_call_bb) = original_freq;
                    Set_Prob(bb,instru_init_call_bb,original_prob);
                    Set_Prob(instru_init_call_bb,instru_pu_init_call_bb, 1.0);
                    Set_Prob(instru_pu_init_call_bb,target_bb,1.0);
                }
            }
        } else if (bb != NULL && BB_call(bb))
        {
            BB* target_bb = BB_next(bb);
            if (target_bb == NULL) return;

            // Check whether the call function has return value. If it has, save
            // and restore it
            BB* restore_bb = NULL;
            BB* instru_init_call_bb;
            int ret_reg_num = Get_Return_Reg_Sum(target_bb);
            int f_ret_reg_num = Get_Float_Return_Reg_Sum(target_bb);
            Is_True(ret_reg_num <= 0 || f_ret_reg_num <= 0 ,
                    (" return reg sum %d, float return reg sum %d" ,
                     ret_reg_num, f_ret_reg_num));
            if (ret_reg_num > 0)
            {
                restore_bb = Gen_BB();
                Set_BB_profile_added(restore_bb);
                instru_init_call_bb = Gen_Call_BB(INSTR_INIT_NAME,
                                                  outputfile_name,_phase,1, ret_reg_num);
                Set_BB_profile_added(instru_init_call_bb);
                for (int i = 0; i < ret_reg_num; i++)
                {
                    BB_Move_Op_To_Start(restore_bb,instru_init_call_bb,
                                        instru_init_call_bb->ops.last);
                }
            } else if (f_ret_reg_num > 0)
            {
                restore_bb = Gen_BB();
                Set_BB_profile_added(restore_bb);
                instru_init_call_bb = Gen_Call_BB(INSTR_INIT_NAME,
                                                  outputfile_name,_phase,2, f_ret_reg_num);
                Set_BB_profile_added(instru_init_call_bb);
                for (int i = 0; i < f_ret_reg_num; i++)
                {
                    BB_Move_Op_To_Start(restore_bb,instru_init_call_bb,
                                        instru_init_call_bb->ops.last);
                }
            } else
            {
                instru_init_call_bb = Gen_Call_BB(INSTR_INIT_NAME,
                                                  outputfile_name,_phase,0);
                Set_BB_profile_added(instru_init_call_bb);
            }

            BB* instru_pu_init_call_bb = Gen_Call_BB(PU_INIT_NAME,
                                         _srcfile_pu_name, _instrument_count,FALSE);
            Set_BB_profile_added(instru_pu_init_call_bb);
            Insert_BB(instru_init_call_bb,bb);
            Insert_BB(instru_pu_init_call_bb,instru_init_call_bb);
            if (restore_bb != NULL)
                Insert_BB(restore_bb,instru_pu_init_call_bb);

            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_init_call_bb);
                Link_Pred_Succ(instru_init_call_bb,instru_pu_init_call_bb);

                BB_freq(instru_init_call_bb) = original_freq;
                BB_freq(instru_pu_init_call_bb) = original_freq;
                Set_Prob(bb,instru_init_call_bb,original_prob);
                Set_Prob(instru_init_call_bb,instru_pu_init_call_bb,1.0);

                if (restore_bb != NULL)
                {
                    Link_Pred_Succ(instru_pu_init_call_bb,restore_bb);
                    Link_Pred_Succ(restore_bb,target_bb);
                    BB_freq(restore_bb) = original_freq;
                    Set_Prob(instru_pu_init_call_bb,restore_bb,1.0);
                    Set_Prob(restore_bb,target_bb,1.0);
                } else
                {
                    Link_Pred_Succ(instru_pu_init_call_bb,target_bb);
                    Set_Prob(instru_pu_init_call_bb,target_bb ,1.0);
                }
            }
        }
    }
}
// ------------------------------------------------------------------
// perform the annotatation
// ------------------------------------------------------------------
void EDGE_PROFILE::CG_Annotate()
{
    // This is the main function by which annotation is completed
    // it annotations in a PU.
    if (Get_Trace(TP_A_PROF,TT_PROF_CFG))
        ;
    INT32 _edge_instrument_count = 0;

    for (BB* bb = _pu_last_bb; bb != NULL;
            bb = BB_prev(bb))
    {
        if (BB_Is_For_Instru(bb))
        {
            Set_BB_freq_fb_based(bb);
            CG_Annotate_Node(bb);
        }
    }

    if (Get_Trace(TP_A_PROF,TT_PROF_CFG))
        ;

    Propagate();
    if (Get_Trace(TP_A_PROF,TT_PROF_CFG))
        ;

    Compute_Edge_Prob_Base_Freq();

    Check_CFG_Consistency("edge profile annotation");
    if (Get_Trace(TP_A_PROF,TT_PROF_CFG))
        ;
}

// ------------------------------------------------------------------
// perform the instrumentation in a BB node
// ------------------------------------------------------------------
void EDGE_PROFILE::CG_Instrument_Node(BB *bb)
{
    // get the branch op of bb
    op* op = BB_xfer_op(bb);
    if (op != NULL)
    {
        switch (OP_code(op))
        {
        case TOP_bne:
        case TOP_beq:
        case TOP_blez:
        case TOP_bgtz:
        case TOP_bltz:
        case TOP_bgez:
        case TOP_j:
        case TOP_bc1f:
        case TOP_bc1fl:
        case TOP_bc1t:
        case TOP_bc1tl:
            Instrument_Mul_Target_Branch(bb);
            break;
        case TOP_jal:
        case TOP_jalr:
        case TOP_jr:
            Instrument_Call_Branch(bb);
            break;
        default:
            Is_True(FALSE, (" %s is not xfer ops exist!\n", TOP_Name(OP_code(op))));
            break;
        }
    } else
        Instrument_None_and_Other_Xfer(bb);
}

// ------------------------------------------------------------------
// perform the annotatation in a BB node
// ------------------------------------------------------------------
void EDGE_PROFILE::CG_Annotate_Node(BB *bb)
{
    op* op = BB_xfer_op(bb);
    if (op != NULL)
    {
        switch (OP_code(op))
        {
        case TOP_bne:
        case TOP_beq:
        case TOP_blez:
        case TOP_bgtz:
        case TOP_bltz:
        case TOP_bgez:
        case TOP_j:
        case TOP_bc1f:
        case TOP_bc1fl:
        case TOP_bc1t:
        case TOP_bc1tl:
            Annotate_Mul_Target_Branch(bb);
            break;
        case TOP_jal:
        case TOP_jalr:
        case TOP_jr:
            Annotate_Call_Branch(bb);
            break;

        default:
            Is_True(FALSE, (" %s is not xfer ops exist!\n", TOP_Name(OP_code(op))));
            break;
        }
    } else
        Annotate_None_and_Other_Xfer(bb);
}

void EDGE_PROFILE::Instrument_Indirect_Cond_Branch(BB* bb)
{
    BBLIST* edge = BB_succs(bb);
    INT32 bblist_len =BBlist_Len(edge);
    OP* op = BB_xfer_op(bb);
    while (edge != NULL && bblist_len--)
    {
        BBLIST* nedge = edge;
        edge = BBLIST_next(edge);

        BB* target_bb = BBLIST_item(nedge);
        if (BB_profile_added(target_bb))
            continue;

        INT32 id = _instrument_count++;
        ++_count_indirect_cond_branch;

        BB* instru_bb = Gen_Instru_Call_BB(EDGE_INST_NAME,_srcfile_pu_name,id);
        Set_BB_profile_added(instru_bb);

        // Generate a label for new BB
        LABEL_IDX instru_bb_label = Gen_Label_For_BB(instru_bb);
        LABEL_IDX tgt_label;
        if (Get_Br_Op_Tgt_Label(op) != LABEL_IDX_NONE &&
                Get_Label_BB(Get_Br_Op_Tgt_Label(op)) == target_bb)
            tgt_label = Get_Br_Op_Tgt_Label(op);
        else
            tgt_label = Gen_Label_For_BB(target_bb);

        Change_BB_Label(instru_bb,tgt_label);
        Change_BB_Label(target_bb,instru_bb_label);

        if (BB_next(bb) == target_bb)
        {
            Insert_BB(instru_bb,bb);
            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,target_bb,1.0);
            }
        } else
        {
            BB* goto_bb = Gen_BB();
            Set_BB_profile_added(goto_bb);
            BB* old_last_bb = _pu_last_bb;
            _pu_last_bb = goto_bb;
            Insert_BB(instru_bb,old_last_bb);
            Insert_BB(goto_bb,instru_bb);
            Add_Goto(goto_bb, target_bb);

            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,goto_bb);
                Link_Pred_Succ(goto_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                BB_freq(goto_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,goto_bb,1.0);
                Set_Prob(goto_bb,target_bb,1.0);
            }
            // change jump target label
            OP *branch_op = BB_branch_op(bb);
            Change_Tgt_Label(branch_op,tgt_label,instru_bb_label);
        }
    }
}

void EDGE_PROFILE::Instrument_Ip_Rel_Branch(BB *bb)
{
    BBLIST* edge = BB_succs(bb);
    INT32 bblist_len =BBlist_Len(edge);
    OP* op = BB_xfer_op(bb);
    while (edge != NULL && bblist_len--)
    {
        BBLIST* nedge = edge;
        edge = BBLIST_next(edge);
        BB* target_bb = BBLIST_item(nedge);
        if (BB_profile_added(target_bb))
            continue;
        LABEL_IDX tgt_label;
        if (Get_Label_BB(Get_Br_Op_Tgt_Label(op)) == target_bb)
            tgt_label = Get_Br_Op_Tgt_Label(op);
        else
            tgt_label = Gen_Label_For_BB(target_bb);

        if (BB_next(bb) == target_bb)
        {
            INT32 id = _instrument_count++;
            BB* instru_bb = Gen_Instru_Call_BB(EDGE_INST_NAME,
                                               _srcfile_pu_name,id);
            Set_BB_profile_added(instru_bb);
            // Generate a label for new BB
            LABEL_IDX instru_bb_label = Gen_Label_For_BB(instru_bb);
            Insert_BB(instru_bb,bb);

            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,target_bb,1.0);
            }

            OP *branch_op = BB_branch_op(bb);
            if (tgt_label == Get_Op_Label(branch_op))
                Change_Tgt_Label(branch_op,tgt_label,instru_bb_label);
        } else
        {
            INT32 id = _instrument_count++;
            BB* instru_bb = Gen_Instru_Call_BB(EDGE_INST_NAME,
                                               _srcfile_pu_name,id);
            Set_BB_profile_added(instru_bb);
            // Generate a label for new BB
            LABEL_IDX instru_bb_label = Gen_Label_For_BB(instru_bb);

            BB* goto_bb = Gen_BB();
            Set_BB_profile_added(goto_bb);
            BB* old_last_bb = _pu_last_bb;
            _pu_last_bb = goto_bb;
            Insert_BB(instru_bb,old_last_bb);
            Insert_BB(goto_bb,instru_bb);
            Add_Goto(goto_bb, target_bb);

            // Update CFG
            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,goto_bb);
                Link_Pred_Succ(goto_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                BB_freq(goto_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,goto_bb,1.0);
                Set_Prob(goto_bb,target_bb,1.0);
            }

            OP *branch_op = BB_branch_op(bb);
            if (tgt_label == Get_Op_Label(branch_op))
                Change_Tgt_Label(branch_op,tgt_label,instru_bb_label);
        }
    }
}

void EDGE_PROFILE::Instrument_Cloop_Branch(BB *bb)
{
    BBLIST* edge = BB_succs(bb);
    INT32 bblist_len =BBlist_Len(edge);
    OP* op = BB_xfer_op(bb);
    while (edge != NULL && bblist_len--)
    {
        BBLIST* nedge = edge;
        edge = BBLIST_next(edge);

        BB* target_bb = BBLIST_item(nedge);

        if (BB_profile_added(target_bb))
            continue;

        INT32 id = _instrument_count++;
        ++_count_cloop;

        BB* instru_bb = Gen_Instru_Call_BB(EDGE_INST_NAME,_srcfile_pu_name,id);
        Set_BB_profile_added(instru_bb);
        // Generate a label for new BB
        LABEL_IDX instru_bb_label = Gen_Label_For_BB(instru_bb);

        LABEL_IDX tgt_label;

        if (Get_Label_BB(Get_Br_Op_Tgt_Label(op)) == target_bb)
            tgt_label = Get_Br_Op_Tgt_Label(op);
        else
            tgt_label = Gen_Label_For_BB(target_bb);

        // TODO:juadge the target_BB is not out of the PU
        if (BB_next(bb) == target_bb)
        {
            Insert_BB(instru_bb,bb);
            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,target_bb,1.0);
            }
        } else
        {
            BB* goto_bb = Gen_BB();
            Set_BB_profile_added(goto_bb);
            BB* old_last_bb = _pu_last_bb;
            _pu_last_bb = goto_bb;
            Insert_BB(instru_bb,old_last_bb);
            Insert_BB(goto_bb,instru_bb);
            Add_Goto(goto_bb, target_bb);

            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,goto_bb);
                Link_Pred_Succ(goto_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                BB_freq(goto_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,goto_bb,1.0);
                Set_Prob(goto_bb,target_bb,1.0);
            }
        }

        // Change jump target label
        OP *branch_op = BB_branch_op(bb);
        Change_Tgt_Label(branch_op,tgt_label,instru_bb_label);
    }
}

void EDGE_PROFILE::Instrument_Top_Branch(BB *bb)
{
    BBLIST* edge = BB_succs(bb);
    INT32 bblist_len =BBlist_Len(edge);
    OP* op = BB_xfer_op(bb);
    while (edge != NULL && bblist_len--)
    {
        BBLIST* nedge = edge;
        edge = BBLIST_next(edge);


        BB* target_bb = BBLIST_item(nedge);

        if (BB_profile_added(target_bb))
            continue;

        INT32 id = _instrument_count++;
        ++_count_top;

        BB* instru_bb = Gen_Instru_Call_BB(EDGE_INST_NAME,_srcfile_pu_name,id);
        Set_BB_profile_added(instru_bb);
        // Generate a label for new BB
        LABEL_IDX instru_bb_label = Gen_Label_For_BB(instru_bb);
        LABEL_IDX tgt_label;
        if (Get_Label_BB(Get_Br_Op_Tgt_Label(op)) == target_bb)
            tgt_label = Get_Br_Op_Tgt_Label(op);
        else
            tgt_label = Gen_Label_For_BB(target_bb);

        // TODO:juadge the target_BB is not out of the PU
        if (BB_next(bb) == target_bb)
        {
            Insert_BB(instru_bb,bb);
            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,target_bb,1.0);
            }
        } else
        {
            BB* goto_bb = Gen_BB();
            Set_BB_profile_added(goto_bb);
            BB* old_last_bb = _pu_last_bb;
            _pu_last_bb = goto_bb;
            Insert_BB(instru_bb,old_last_bb);
            Insert_BB(goto_bb,instru_bb);
            Add_Goto(goto_bb, target_bb);

            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,goto_bb);
                Link_Pred_Succ(goto_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                BB_freq(goto_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,goto_bb,1.0);
                Set_Prob(goto_bb,target_bb,1.0);
            }
        }

        // TODO:check use BB_branch_op or BB_xfer_op
        OP *branch_op = BB_branch_op(bb);
        Change_Tgt_Label(branch_op,tgt_label,instru_bb_label);
    }
}

void EDGE_PROFILE::Instrument_None_and_Other_Xfer(BB *bb)
{

    Is_True(bb!=NULL,("can not instrument after a NULL BB"));
    if (BB_succs_len(bb) > 1)
    {
        DevWarn("BB %d does not have only succeed BB!",BB_id(bb));
        return;
    }
    if (BB_succs_len(bb) == 0)
        return;
    if (BBLIST_item(BB_succs(bb)) != BB_next(bb))
    {
        DevWarn("the next BB of BB %d is not succeed BB!",BB_id(bb));
        return;
    }

    if (BB_profile_added(BB_next(bb)))
        return;
    OP* branch_op = BB_xfer_op(bb);
    INT32 id = _instrument_count++;

    if (BB_call(bb))
    {
        BB* bp;
        BB* target_bb = BB_next(bb);
        // Check whether the call function has return value. if has, save
        // and restore it
        BB* restore_bb = NULL;
        int ret_reg_num = Get_Return_Reg_Sum(target_bb);
        int f_ret_reg_num = Get_Float_Return_Reg_Sum(target_bb);
        Is_True(ret_reg_num <= 0 || f_ret_reg_num <= 0 ,
                (" return reg sum %d, float return reg sum %d" ,
                 ret_reg_num, f_ret_reg_num));
        if (ret_reg_num > 0)
        {
            restore_bb = Gen_BB();
            Set_BB_profile_added(restore_bb);

            bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id,
                             RESTORE_INT_RETURN_TN, ret_reg_num);
            Set_BB_profile_added(bp);
            for (int i = 0; i < ret_reg_num; i++)
            {
                BB_Move_Op_To_Start(restore_bb,bp,
                                    bp->ops.last);
            }
        } else if (f_ret_reg_num > 0)
        {
            restore_bb = Gen_BB();
            Set_BB_profile_added(restore_bb);
            bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id,
                             RESTORE_FLOAT_RETURN_TN, f_ret_reg_num);
            Set_BB_profile_added(bp);
            for (int i = 0; i < f_ret_reg_num; i++)
            {
                BB_Move_Op_To_Start(restore_bb,bp,
                                    bp->ops.last);
            }
        } else
        {
            bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id, RESTORE_NONE);
            Set_BB_profile_added(bp);
        }

        // Update CFG
        Insert_BB(bp,bb);
        if (restore_bb != NULL)
            Insert_BB(restore_bb,bp);

        {
            float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
            float original_prob = Prob_Local(bb,target_bb);
            Unlink_Pred_Succ(bb,target_bb);
            Link_Pred_Succ(bb,bp);
            BB_freq(bp) = original_freq;
            Set_Prob(bb,bp,original_prob);
            if (restore_bb != NULL)
            {
                Link_Pred_Succ(bp,restore_bb);
                Link_Pred_Succ(restore_bb,target_bb);
                BB_freq(restore_bb) = original_freq;
                Set_Prob(bp,restore_bb,1.0);
                Set_Prob(restore_bb,target_bb,1.0);
            } else
            {
                Link_Pred_Succ(bp,target_bb);
                Set_Prob(bp,target_bb,1.0);
            }
        }
    } else
    {

        BB* bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id,0);
        Set_BB_profile_added(bp);
        Insert_BB(bp,bb);
        BB* succ = BBLIST_item(BB_succs(bb));

        {
            float original_freq = Prob_Local(bb,succ) * BB_freq(bb);
            float original_prob = Prob_Local(bb,succ);
            Unlink_Pred_Succ(bb,succ);
            Link_Pred_Succ(bp,succ);
            Link_Pred_Succ(bb,bp);
            BB_freq(bp) = original_freq;
            Set_Prob(bp,succ,1.0);
            Set_Prob(bb,bp,original_prob);
        }
    }
}


void EDGE_PROFILE::Instrument_Mul_Target_Branch(BB* bb)
{
    BBLIST* edge = BB_succs(bb);
    INT32 bblist_len =BBlist_Len(edge);
    OP* op = BB_xfer_op(bb);
    while (edge != NULL && bblist_len--)
    {
        BBLIST* nedge = edge;
        edge = BBLIST_next(edge);

        INT32 id = _instrument_count++;
        ++_count_cloop;

        BB* target_bb = BBLIST_item(nedge);

        BB* instru_bb = Gen_Instru_Call_BB(EDGE_INST_NAME,_srcfile_pu_name,id);
        Set_BB_profile_added(instru_bb);
        // Generate a label for new BB
        LABEL_IDX instru_bb_label = Gen_Label_For_BB(instru_bb);

        LABEL_IDX tgt_label;

        if (Get_Label_BB(Get_Br_Op_Tgt_Label(op)) == target_bb)
            tgt_label = Get_Br_Op_Tgt_Label(op);
        else
            tgt_label = Gen_Label_For_BB(target_bb);

        // TODO:juadge the target_BB is not out of the PU
        if (BB_next(bb) == target_bb)
        {
            Insert_BB(instru_bb,bb);
            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,target_bb,1.0);
            }
        } else
        {
            BB* goto_bb = Gen_BB();
            Set_BB_profile_added(goto_bb);
            BB* old_last_bb = _pu_last_bb;
            _pu_last_bb = goto_bb;
            Insert_BB(instru_bb,old_last_bb);
            Insert_BB(goto_bb,instru_bb);
            Add_Goto(goto_bb, target_bb);

            {
                float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
                float original_prob = Prob_Local(bb,target_bb);
                Unlink_Pred_Succ(bb,target_bb);
                Link_Pred_Succ(bb,instru_bb);
                Link_Pred_Succ(instru_bb,goto_bb);
                Link_Pred_Succ(goto_bb,target_bb);
                BB_freq(instru_bb) = original_freq;
                BB_freq(goto_bb) = original_freq;
                Set_Prob(bb,instru_bb,original_prob);
                Set_Prob(instru_bb,goto_bb,1.0);
                Set_Prob(goto_bb,target_bb,1.0);
            }
        }

        // Change jump target label
        OP *branch_op = BB_branch_op(bb);
        Change_Tgt_Label(branch_op,tgt_label,instru_bb_label);
    }
}


void EDGE_PROFILE::Instrument_Call_Branch(BB *bb)
{

    Is_True(bb!=NULL,("can not instrument after a NULL BB"));
    if (BB_succs_len(bb) > 1)
    {
        DevWarn("BB %d does not have only succeed BB!",BB_id(bb));
        return;
    }
    if (BB_succs_len(bb) == 0)
        return;
    if (BBLIST_item(BB_succs(bb)) != BB_next(bb))
    {
        DevWarn("the next BB of BB %d is not succeed BB!",BB_id(bb));
        return;
    }
    OP* branch_op = BB_xfer_op(bb);
    INT32 id = _instrument_count++;

    if (BB_call(bb))
    {
        BB* bp;
        BB* target_bb = BB_next(bb);
        // Check whether the call function has return value. if has, save
        // and restore it
        BB* restore_bb = NULL;

        int ret_reg_num = Get_Return_Reg_Sum(target_bb);
        int f_ret_reg_num = Get_Float_Return_Reg_Sum(target_bb);
        Is_True(ret_reg_num <= 0 || f_ret_reg_num <= 0 ,
                (" return reg sum %d, float return reg sum %d" ,
                 ret_reg_num, f_ret_reg_num));
        if (ret_reg_num > 0)
        {
            restore_bb = Gen_BB();
            Set_BB_profile_added(restore_bb);
            bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id,
                             RESTORE_INT_RETURN_TN, ret_reg_num);
            Set_BB_profile_added(bp);
            for (int i = 0; i < ret_reg_num; i++)
            {
                BB_Move_Op_To_Start(restore_bb,bp,
                                    bp->ops.last);
            }
        } else if (f_ret_reg_num > 0)
        {
            restore_bb = Gen_BB();
            Set_BB_profile_added(restore_bb);
            bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id,
                             RESTORE_FLOAT_RETURN_TN, f_ret_reg_num);
            Set_BB_profile_added(bp);
            for (int i = 0; i < f_ret_reg_num; i++)
            {
                BB_Move_Op_To_Start(restore_bb,bp,
                                    bp->ops.last);
            }
        } else
        {
            bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id, RESTORE_NONE);
            Set_BB_profile_added(bp);
        }

        // Update CFG
        Insert_BB(bp,bb);
        if (restore_bb != NULL)
            Insert_BB(restore_bb,bp);

        {
            float original_freq = Prob_Local(bb,target_bb) * BB_freq(bb);
            float original_prob = Prob_Local(bb,target_bb);
            Unlink_Pred_Succ(bb,target_bb);
            Link_Pred_Succ(bb,bp);
            BB_freq(bp) = original_freq;
            Set_Prob(bb,bp,original_prob);
            if (restore_bb != NULL)
            {
                Link_Pred_Succ(bp,restore_bb);
                Link_Pred_Succ(restore_bb,target_bb);
                BB_freq(restore_bb) = original_freq;
                Set_Prob(bp,restore_bb,1.0);
                Set_Prob(restore_bb,target_bb,1.0);
            } else
            {
                Link_Pred_Succ(bp,target_bb);
                Set_Prob(bp,target_bb,1.0);
            }
        }
    } else
    {

        BB* bp = Gen_Call_BB(EDGE_INST_NAME,_srcfile_pu_name, id,0);
        Set_BB_profile_added(bp);
        Insert_BB(bp,bb);
        BB* succ = BBLIST_item(BB_succs(bb));

        {
            float original_freq = Prob_Local(bb,succ) * BB_freq(bb);
            float original_prob = Prob_Local(bb,succ);
            Unlink_Pred_Succ(bb,succ);
            Link_Pred_Succ(bp,succ);
            Link_Pred_Succ(bb,bp);
            BB_freq(bp) = original_freq;
            Set_Prob(bp,succ,1.0);
            Set_Prob(bb,bp,original_prob);
        }
    }
}


// ------------------------------------------------------------------
// Change the branch op's label from old_label to new_lable.
// If branch_op has not old_label, don't change it.
// ------------------------------------------------------------------
void EDGE_PROFILE::Change_Tgt_Label(op* branch_op,
                                    LABEL_IDX old_label,LABEL_IDX new_label)
{

    INT tn_num = OP_opnds(branch_op);
    for (int i = 0; i<tn_num; i++)
    {
        TN* tgt_tn = OP_opnd(branch_op, i);
        if (TN_is_label(tgt_tn) && (TN_label(tgt_tn) == old_label))
        {
            Set_TN_label(tgt_tn, new_label);
            break;
        }
    }
}

void EDGE_PROFILE::Annotate_Indirect_Cond_Branch(BB *bb)
{
    for (BBLIST* edge = BB_succs(bb); edge != NULL;
            edge = BBLIST_next(edge))
    {
        INT32 id =  _instrument_count++;

        PU_PROFILE_HANDLES& handles = _fb_handles;
        FB_Info_Edge info_branch(FB_FREQ_ZERO);
        for (PU_PROFILE_ITERATOR i(handles.begin()); i != handles.end();
                ++i)
        {
            FB_Info_Edge& info = Get_Edge_Profile(*i, id);
            if (&info != NULL)
                info_branch.freq_edge += info.freq_edge;
        }
        // attach info to edge
        BBLIST_freq(edge) = info_branch.freq_edge.Value();

        if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
            Dump_Edge_Info(bb,edge);
    }
}

void EDGE_PROFILE::Annotate_Ip_Rel_Branch(BB *bb)
{
    for (BBLIST* edge = BB_succs(bb); edge != NULL;
            edge = BBLIST_next(edge))
    {
        BB* target_bb = BBLIST_item(edge);
        {
            INT32 id = _instrument_count++;

            PU_PROFILE_HANDLES& handles = FB_Handle();
            FB_Info_Edge info_Ip_Rel(FB_FREQ_ZERO);
            for (PU_PROFILE_ITERATOR i(handles.begin()); i != handles.end();
                    ++i)
            {
                FB_Info_Edge& info = Get_Edge_Profile(*i, id);
                if (&info != NULL)
                    info_Ip_Rel.freq_edge += info.freq_edge;
            }
            // attach info to edge
            BBLIST_freq(edge) = info_Ip_Rel.freq_edge.Value();
            if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
                Dump_Edge_Info(bb,edge);
        }
    }
}

void EDGE_PROFILE::Annotate_Cloop_Branch(BB *bb)
{
    for (BBLIST* edge = BB_succs(bb); edge != NULL;
            edge = BBLIST_next(edge))
    {
        INT32 id = _instrument_count++;

        PU_PROFILE_HANDLES& handles = FB_Handle();
        FB_Info_Edge info_loop(FB_FREQ_ZERO);
        for (PU_PROFILE_ITERATOR i(handles.begin()); i != handles.end();
                ++i)
        {
            FB_Info_Edge& info = Get_Edge_Profile(*i, id);
            if (&info != NULL)
                info_loop.freq_edge += info.freq_edge;
        }
        // attach info to edge
        BBLIST_freq(edge) = info_loop.freq_edge.Value();
        if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
            Dump_Edge_Info(bb,edge);
    }
}

void EDGE_PROFILE::Annotate_Top_Branch(BB *bb)
{
    for (BBLIST* edge = BB_succs(bb); edge != NULL;
            edge = BBLIST_next(edge))
    {
        INT32 id = _instrument_count++;
        ++_count_top;

        PU_PROFILE_HANDLES& handles = FB_Handle();
        FB_Info_Edge info_top(FB_FREQ_ZERO);
        for (PU_PROFILE_ITERATOR i(handles.begin()); i != handles.end();
                ++i)
        {
            FB_Info_Edge& info = Get_Edge_Profile(*i, id);
            if (&info != NULL)
                info_top.freq_edge += info.freq_edge;
        }
        // attach info to edge
        BBLIST_freq(edge) = info_top.freq_edge.Value();
        if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
            Dump_Edge_Info(bb,edge);
    }
}

void EDGE_PROFILE::Annotate_Call_Branch(BB *bb)
{
    Is_True(bb!=NULL,("can not annotate after a NULL BB"));
    if (BB_succs_len(bb) > 1)
    {
        DevWarn("BB %d does not have only succeed BB!",BB_id(bb));
        return;
    }
    if (BB_succs(bb) == NULL)
        return;

    OP* branch_op = BB_xfer_op(bb);
    INT32 id = _instrument_count++;

    PU_PROFILE_HANDLES& handles = FB_Handle();
    FB_Info_Edge info_top(FB_FREQ_ZERO);
    for (PU_PROFILE_ITERATOR i(handles.begin()); i != handles.end();
            ++i)
    {
        FB_Info_Edge& info = Get_Edge_Profile(*i, id);
        if (&info != NULL)
            info_top.freq_edge += info.freq_edge;
    }
    // attach info to edge
    BBLIST* edge = BB_succs(bb);
    if (edge !=NULL)
        BBLIST_freq(edge) = info_top.freq_edge.Value();

    if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
        Dump_Edge_Info(bb,edge);

}
void EDGE_PROFILE::Annotate_None_and_Other_Xfer(BB *bb)
{
    Is_True(bb!=NULL,("can not annotate after a NULL BB"));
    if (BB_succs_len(bb) > 1)
    {
        DevWarn("BB %d does not have only succeed BB!",BB_id(bb));
        return;
    }
    if (BB_succs(bb) == NULL)
        return;

    OP* branch_op = BB_xfer_op(bb);
    INT32 id = _instrument_count++;

    PU_PROFILE_HANDLES& handles = FB_Handle();
    FB_Info_Edge info_top(FB_FREQ_ZERO);
    for (PU_PROFILE_ITERATOR i(handles.begin()); i != handles.end();
            ++i)
    {
        FB_Info_Edge& info = Get_Edge_Profile(*i, id);
        if (&info != NULL)
            info_top.freq_edge += info.freq_edge;
    }
    // attach info to edge
    BBLIST* edge = BB_succs(bb);
    if (edge !=NULL)
        BBLIST_freq(edge) = info_top.freq_edge.Value();

    if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
        Dump_Edge_Info(bb,edge);

}


void EDGE_PROFILE::Annotate_Mul_Target_Branch(BB* bb)
{
    for (BBLIST* edge = BB_succs(bb); edge != NULL;
            edge = BBLIST_next(edge))
    {
        INT32 id =  _instrument_count++;

        PU_PROFILE_HANDLES& handles = _fb_handles;
        FB_Info_Edge info_branch(FB_FREQ_ZERO);
        for (PU_PROFILE_ITERATOR i(handles.begin()); i != handles.end();
                ++i)
        {
            FB_Info_Edge& info = Get_Edge_Profile(*i, id);
            if (&info != NULL)
                info_branch.freq_edge += info.freq_edge;
        }
        // attach info to edge
        BBLIST_freq(edge) = info_branch.freq_edge.Value();

        if (Get_Trace(TP_A_PROF,TT_PROF_FEEDBACK_DUMP))
            Dump_Edge_Info(bb,edge);
    }
}

void EDGE_PROFILE::Prepare_Call_Init_Return_BB(BB * bb)
{
    FmtAssert(FALSE,("Unimplemented"));
}

BB* EDGE_PROFILE::Gen_Call_BB(char* function_name, char* str_arg,
                              INT64 int_arg,int restore_type,int restore_sum)
{
    BB *bb=Gen_BB();
    SRCPOS srcpos = REGION_First_BB->ops.last->srcpos;

    OP* restore_f_reg;
    OP* restore_reg;
    OPS* restore_reg_ops = OPS_Create();
    OPS* restore_f_reg_ops = OPS_Create();
    OPS* call_ops = OPS_Create();
    if (restore_type == RESTORE_INT_RETURN_TN)
    {
        for (int i = 0; i < restore_sum; i++)
        {
            TN* save_reg_tn1 = Gen_Register_TN(ISA_REGISTER_CLASS_integer,8);
            TN* return_result_reg =
                Build_Dedicated_TN(ISA_REGISTER_CLASS_integer,RETURN_REG + i ,8);
            Set_TN_is_global_reg(save_reg_tn1);
            // Save the return_result registers in the beginning of call_bb
            Expand_Copy(save_reg_tn1, return_result_reg, MTYPE_U8, call_ops);

            // Restore the return_result registers in the next bb
            Expand_Copy(return_result_reg, save_reg_tn1, MTYPE_U8, restore_reg_ops);
        }
    }

    if (restore_type == RESTORE_FLOAT_RETURN_TN)
    {
        for (int i = 0 ; i < restore_sum ; i++)
        {
            TN* save_reg_tn2 = Gen_Register_TN(ISA_REGISTER_CLASS_float,8);
            // floating pointer return registers are $f0 and $f2
            TN* f_return_result_reg = Build_Dedicated_TN(
                                          ISA_REGISTER_CLASS_float,FLOAT_RETURN_REG + 2*i, 0);

            Set_TN_is_global_reg(save_reg_tn2);

            // Save the return_result registers in the beginning of call_bb
            Expand_Copy(save_reg_tn2, f_return_result_reg, MTYPE_F8, call_ops);

            // Restore the return_result registers in the next bb
            Expand_Copy(f_return_result_reg, save_reg_tn2, MTYPE_F8, restore_f_reg_ops);

        }
    }


    // Please look into cg_instru_lib.cxx/h

    // Step I:
    // Set the first parameter which is a string for PU name
    TN* arg_tn1 = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, ARG_REG1, 8);
    // create the string symtal constant
    TCON tcon = Host_To_Targ_String(MTYPE_STRING,str_arg,
                                    strlen(str_arg)+1);
    TY_IDX ty = MTYPE_To_TY(MTYPE_STRING);
    ST* st = Gen_String_Sym(&tcon, ty, FALSE);
    Allocate_Object(st);
    INT64 offset = 0;

    Exp_Lda(MTYPE_U4, arg_tn1, st, 0, OPR_STID, call_ops);

    // Step II:
    // Set the second parameter which is a int value for id
    TN* arg_tn2 = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, ARG_REG2, 8);
    Expand_Immediate(arg_tn2, Gen_Literal_TN(int_arg, 8), FALSE,  call_ops);


    // Step III:
    // Generate the call
    ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
    ST *call_st = Gen_Intrinsic_Function(ty, function_name);
    Clear_PU_no_side_effects(Pu_Table[ST_pu(call_st)]);
    Clear_PU_is_pure(Pu_Table[ST_pu(call_st)]);
    Set_PU_no_delete(Pu_Table[ST_pu(call_st)]);

    Exp_Lda(MTYPE_U4, Ep_TN, call_st, 0, OPR_PICCALL, call_ops);
    Exp_Call(OPR_PICCALL, RA_TN, Ep_TN, call_ops);

    // Append it to the bb
    BB_Append_Ops(bb, call_ops);

    if (restore_type == RESTORE_INT_RETURN_TN)
    {
        BB_Append_Ops(bb,restore_reg_ops);
    } else if (restore_type == RESTORE_FLOAT_RETURN_TN)
    {
        BB_Append_Ops(bb,restore_f_reg_ops);
    }

    Set_BB_call(bb);
    // Set_BB_profile_added(bb);
    // add annotations about callinfo
    WN *count = WN_Intconst(MTYPE_I4, int_arg);
    WN *str_arg_tn = WN_LdaString(str_arg, 0,
                                  strlen(str_arg) + 1);
    WN* call = Gen_Call(function_name, str_arg_tn ,count);
    call_st = WN_st(call);
    CALLINFO* call_info = TYPE_PU_ALLOC(CALLINFO);
    CALLINFO_call_st(call_info) = call_st;
    CALLINFO_call_wn(call_info) = call;
    BB_Add_Annotation(bb, ANNOT_CALLINFO, call_info);
    return bb;
}

BB* EDGE_PROFILE::Gen_Instru_Call_BB(char* function_name,
                                     char* file_pu_name,INT64 id)
{
    return Gen_Call_BB(function_name,file_pu_name,id,0);
}

BB* EDGE_PROFILE::Gen_PU_Init_Call_BB(char* function_name,char* file_pu_name,
                                      INT64 _instrument_count)
{
    return Gen_Call_BB(function_name,file_pu_name,_instrument_count,0);
}

// Find the TN which opcode is opc,return the No index TN
TN* EDGE_PROFILE::Find_TN(TOP opc,int bb_num,int TN_num, TN* t1, int index)
{
    Is_True(bb_num>0,("the BB num is not less than 0"));
    Is_True(TN_num>=0,(" the TN num is not less than 0"));
    Is_True(index>=0,(" the TN num is not less than 0"));
    Is_True(index<=9,(" the TN num is not less than 9"));
    BB* bb=_pu_first_bb;
    for (int i=1; i<bb_num; i++)
        if (bb != NULL)
            bb = BB_next(bb);

    if (bb == NULL)
        return NULL;

    OP* opcode = bb->ops.first;
    for (; opcode != NULL; opcode = OP_next(opcode))
    {
        if (OP_code(opcode) != opc)
            continue;

        if (OP_opnd(opcode,TN_num) != t1)
            continue;
        else
            return OP_opnd(opcode,index);
    }

    if (opcode == NULL)
        return NULL;
}

BOOL EDGE_PROFILE::BB_Is_In_BBLIST(BBLIST* bblst,BB* bb,BB* before_bb)
{
    for (BBLIST* tbblist = bblst;
            tbblist != NULL && BBLIST_item(tbblist) !=  before_bb;
            tbblist = BBLIST_next(tbblist))
    {
        if (BBLIST_item(tbblist) == bb)
            return TRUE;
    }
    return  FALSE;
}

BOOL EDGE_PROFILE::BB_Is_For_Instru(BB* bb)
{
    BB* bb1;
    if (BB_profile_added(bb))
        return FALSE;

    if (BB_profile_splitted(bb))
    {
        mBB_NUM id_before_profile = BB_id_before_profile(bb);
        for (bb1=bb; bb1 !=NULL; bb1=BB_next(bb1))
        {
            if (id_before_profile == BB_id_before_profile(bb1))
                break;
        }
        if (id_before_profile == BB_id_before_profile(bb1))
            return FALSE;
    }
    return TRUE;
}
TN* EDGE_PROFILE::Find_TN_in_BB(TOP opc, BB* in, int i1, TN* t1, int index)
{
    Is_True(in != NULL,("the BB should not be NULL"));
    Is_True(i1>=0,(" the TN num is not less than 0"));
    Is_True(index>=0,(" the TN num is not less than 0"));
    Is_True(index<=9,(" the TN num is not less than 9"));

    OP* opcode = in->ops.first;
    for (; opcode != NULL; opcode = OP_next(opcode))
    {
        if (OP_code(opcode) != opc)
            continue;

        if (OP_opnd(opcode,i1) != t1)
            continue;
        else
            return OP_opnd(opcode,index);
    }

    if (opcode == NULL)
        return NULL;
}

// ------------------------------------------------------------------
// Get the current PU's Basic Block Num ( the value of global variable
// "PU_BB_Count"  is not the right number of BBs in current PU )
// ------------------------------------------------------------------
INT32 Curr_PU_BB_Count()
{
    INT32 count = 0;
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
        ++count;
    return count;
}

void Init_CFG()
{
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
        for (BBLIST* out_edge = BB_succs(bb); out_edge != NULL;
                out_edge = BBLIST_next(out_edge))
            BBLIST_freq(out_edge) = FB_FREQ_UNINIT;
}

void EDGE_PROFILE::Propagate()
{
    for (BB* bb = _pu_first_bb; bb != NULL; bb = BB_next(bb))
    {
        Propagate_Node_In(bb);
        Propagate_Node_Out(bb);
    }
}

// ------------------------------------------------------------------
// propagate the incoming  edge's freq of bb
// ------------------------------------------------------------------
void EDGE_PROFILE::Propagate_Node_In(BB* bb)
{
    FB_FREQ total_in,total_out;
    int uninit_in, uninit_out;
    total_in = total_out = FB_FREQ_ZERO;
    uninit_in = uninit_out = 0;
    BOOL has_in_edge = FALSE;

    // Check how many edges that is not inited
    for (BBLIST* in_edge = BB_preds(bb); in_edge != NULL;
            in_edge = BBLIST_next(in_edge))
    {
        has_in_edge = TRUE;
        float freq = Freq(BBLIST_item(in_edge),bb);
        if (UNINIT_FREQ(freq) || ERROR_FREQ(freq))
        {
            ++uninit_in;
            break;
        }
        else
            total_in += FB_FREQ(freq,TRUE);
    }

    // If all the incoming edges have values, the BB's freq is equal to
    // total_in
    if (uninit_in  == 0 && has_in_edge)
    {
        BB_freq(bb) = total_in.Value();
        // Find out how many outcoming edges have no value. If only one,
        // outcoming edge has no value. Compute the outcoming edge's freq
        for (BBLIST* out_edge = BB_succs(bb); out_edge != NULL;
                out_edge = BBLIST_next(out_edge))
        {
            float freq = Freq(bb,BBLIST_item(out_edge));
            if (UNINIT_FREQ(freq) || ERROR_FREQ(freq))
                ++uninit_out;
            else
                total_out += FB_FREQ(freq,TRUE);
        }

        if (uninit_out == 1)
        {
            for (BBLIST* out_edge = BB_succs(bb); out_edge != NULL;
                    out_edge = BBLIST_next(out_edge))
            {
                float freq = Freq(bb,BBLIST_item(out_edge));
                if (UNINIT_FREQ(freq) || ERROR_FREQ(freq))
                {
                    Set_Freq(bb,BBLIST_item(out_edge),
                             total_in.Value() - total_out.Value());
                    Propagate_Node_In(BBLIST_item(out_edge));
                    break;
                }
            }
        }
    }
}

// ------------------------------------------------------------------
// propagate the outcoming  edge's freq of bb
// ------------------------------------------------------------------
void EDGE_PROFILE::Propagate_Node_Out(BB* bb)
{
    FB_FREQ total_in,total_out;
    int uninit_in, uninit_out;
    total_in = total_out = FB_FREQ_ZERO;
    uninit_in = uninit_out = 0;
    BOOL has_out_edge = FALSE;

    // Check how many edges that is not inited
    for (BBLIST* out_edge = BB_succs(bb); out_edge != NULL;
            out_edge = BBLIST_next(out_edge))
    {
        has_out_edge = TRUE;
        float freq = Freq(bb,BBLIST_item(out_edge));
        if (UNINIT_FREQ(freq) || ERROR_FREQ(freq))
        {
            ++uninit_out;
            break;
        }
        else
            total_out += FB_FREQ(freq,TRUE);
    }

    // if all the incoming edges has value, then the BB's freq is equal to
    // total_in
    if (uninit_out == 0 && has_out_edge)
    {
        BB_freq(bb) = total_out.Value();
        // find how many outcoming edges that has not value. if just one
        // outcoming edge has not value, compute the outcoming edge's freq
        for (BBLIST* in_edge = BB_preds(bb); in_edge != NULL;
                in_edge = BBLIST_next(in_edge))
        {
            float freq = Freq(BBLIST_item(in_edge),bb);
            if (UNINIT_FREQ(freq) || ERROR_FREQ(freq))
                ++uninit_in;
            else
                total_in += FB_FREQ(freq,TRUE);
        }

        if (uninit_in == 1)
        {
            for (BBLIST* in_edge = BB_preds(bb); in_edge != NULL;
                    in_edge = BBLIST_next(in_edge))
            {
                float freq = Freq(BBLIST_item(in_edge),bb);
                if (UNINIT_FREQ(freq) || ERROR_FREQ(freq))
                {
                    Set_Freq(BBLIST_item(in_edge),bb,
                             total_out.Value() - total_in.Value());
                    Propagate_Node_Out(BBLIST_item(in_edge));
                    break;
                }
            }

        }
    }
}

// ------------------------------------------------------------------
// Compute the edge's freq, and the premise is that the edge has its
// probability. IF the edge's freq is equal to edge's probability,
// multiply the BB's freq
// ------------------------------------------------------------------
void Compute_Edge_Freq_Base_Prob()
{
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
    {
        for (BBLIST* out_edge = BB_succs(bb); out_edge != NULL;
                out_edge = BBLIST_next(out_edge))
        {
            if (ERROR_FREQ(BB_freq(bb)))
                DevWarn("there a bb with invalid freq,BB id %d",BB_id(bb));
            else if (ERROR_PROB(BBLIST_prob(out_edge)))
            {
                DevWarn("there a edge with invalid prob");
                BBLIST_prob(out_edge) = 0.0;
                BBLIST_freq(out_edge) = 0.0;
            }
            else
                BBLIST_freq(out_edge) = BBLIST_prob(out_edge) * BB_freq(bb);
        }
    }
}

// ------------------------------------------------------------------
// Compute the edge's prob, and the premise is that the edge has its
// freq. The edge's prob is equal to bb's freq divided by edge's freq
// ------------------------------------------------------------------

void Compute_Edge_Prob_Base_Freq()
{
    INT32 len = 1;
    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
    {
        BBLIST* out_edge =  BB_succs(bb);
        if (out_edge != NULL)
            len =  BBlist_Len(out_edge);
        for (out_edge = BB_succs(bb); out_edge != NULL;
                out_edge = BBLIST_next(out_edge))
        {
            if (ERROR_FREQ(BB_freq(bb)))
                DevWarn("there a bb with invalid freq,BB id %d",BB_id(bb));
            else if (ERROR_FREQ(BBLIST_freq(out_edge)))
                DevWarn("there a edge with invalid prob");
            else if (ZERO_FREQ(BB_freq(bb)))
            {
                BBLIST_prob(out_edge) = 1.0F / len;
            }
            else
                BBLIST_prob(out_edge) = BBLIST_freq(out_edge) / BB_freq(bb);
        }
    }
}

void Change_BB_Label(BB* bb,LABEL_IDX i)
{
    Set_Label_BB(i,bb);
    ANNOTATION* annt  = ANNOT_Get(BB_annotations(bb),ANNOT_LABEL);
    if (annt != NULL)
    {
        BB_annotations(bb) = ANNOT_Unlink(BB_annotations(bb), annt);
        BB_Add_Annotation(bb,ANNOT_LABEL,(void *)i);
    }
}

BB* First_Branch_BB()
{
    BB* bb = NULL;
    for (bb = REGION_First_BB; bb != NULL; bb =BB_next(bb))
    {
        if (BB_kind(bb) == BBKIND_LOGIF || BB_kind(bb) == BBKIND_VARGOTO ||
                BB_kind(bb) == BBKIND_INDGOTO)
            return bb;
    }
    return bb;
}

BOOL Result_Tn_In_BB(BB* bb, TN* tn)
{
    OP* opcode = bb->ops.first;
    int i;
    for (; opcode != NULL; opcode = OP_next(opcode))
    {
        for (i = 0; i < OP_results(opcode); i++)
        {
            if (tn == OP_result(opcode,i))
                return TRUE;
        }
    }
    return FALSE;
}

static BOOL
Opnd_Tn_In_BB(BB* bb, REGISTER reg, unsigned char type)
{
    OP* opcode = bb->ops.first;
    int i;
    for (; opcode != NULL; opcode = OP_next(opcode))
    {
        for (i = 0; i < OP_opnds(opcode); i++)
        {
            TN *tn = OP_opnd(opcode,i);
            if (TN_is_register(tn)) {
                if (type == 0 && TN_register_class(tn) == ISA_REGISTER_CLASS_integer && TN_register(tn) == reg)
                    return TRUE;
                else if (type == 1 && TN_register_class(tn) == ISA_REGISTER_CLASS_float && TN_register(tn) == reg)
                    return TRUE;
            }
        }
    }
    return FALSE;
}


LABEL_IDX Get_Br_Op_Tgt_Label(OP* branch_op)
{
    INT tn_num = OP_opnds(branch_op);
    for (int i = 0; i<tn_num; i++)
    {
        TN* tgt_tn = OP_opnd(branch_op, i);
        if (TN_is_label(tgt_tn))
        {
            return TN_label(tgt_tn);
        }
    }
    return LABEL_IDX_NONE;
}

// Return the sum of the procedure return register in BB bb
INT32 Get_Return_Reg_Sum(BB* bb)
{
    for (int i = 1; i >= 0; i--)
    {
        TN* return_result_reg =
            Build_Dedicated_TN(ISA_REGISTER_CLASS_integer,RETURN_REG + i, 8);
        if (Opnd_Tn_In_BB(bb,RETURN_REG + i,0))
            return i + 1;
    }
    return 0;
}

// return the sum of the procedure return float register in BB bb
INT32 Get_Float_Return_Reg_Sum(BB* bb)
{
    for (int i = 1 ; i >= 0; i--)
    {
        TN* f_return_result_reg =
            Build_Dedicated_TN(ISA_REGISTER_CLASS_float,FLOAT_RETURN_REG + 2*i, 0);
        if (Opnd_Tn_In_BB(bb,FLOAT_RETURN_REG+2*i,1))
            return i + 1;
    }
    return 0;
}
