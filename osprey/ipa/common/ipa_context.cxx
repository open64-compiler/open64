/*
 * IPA Context — transitional global definitions and lifecycle functions.
 *
 * This file is compiled by all IPA-family targets (ipa, inline, lw_inline).
 * The IPA_Options_Init function uses config_ipa.h globals which are only
 * available in the main IPA target, so it's guarded by IPA_CONTEXT_FULL_INIT.
 */

#include "ipa_context.h"

#include <cstring>		/* memset */
#include <cstdlib>		/* malloc, free */

/* ====================================================================
 * Transitional globals — needed by all IPA-family targets.
 * ====================================================================
 */
IPA_Context       *g_ipa_ctx     = NULL;


/*
 * The full init/fini functions are only available in the main IPA target
 * where config_ipa.h globals are linked in.  The inline and lw_inline
 * targets just need the g_ipa_ctx symbol for the macro-based accessors.
 */
#ifdef IPA_CONTEXT_FULL_INIT

#include "ipa_options.h"
#include "config_ipa.h"		/* all the IPA_Enable_* / INLINE_* globals */

const IPA_Options *g_ipa_options = NULL;

/* Internal mutable copy that g_ipa_options points to */
static IPA_Options *ipa_options_mutable = NULL;


/* ====================================================================
 * IPA_Options_Init:  snapshot every config_ipa.h global into the struct.
 * ====================================================================
 */
static void
IPA_Options_Init(void)
{
    if (ipa_options_mutable != NULL)
	return;

    ipa_options_mutable = (IPA_Options *) malloc(sizeof(IPA_Options));
    memset(ipa_options_mutable, 0, sizeof(IPA_Options));

    IPA_Options *o = ipa_options_mutable;

    /* --- Filenames --- */
    o->feedback_Filename   = Feedback_Filename;
    o->annotation_Filename = Annotation_Filename;

    /* --- Feature enable flags --- */
    o->enable_DFE              = IPA_Enable_DFE;
    o->enable_DFE_Set          = IPA_Enable_DFE_Set;
    o->enable_Inline           = IPA_Enable_Inline;
    o->enable_Picopt           = IPA_Enable_Picopt;
    o->enable_AutoGnum         = IPA_Enable_AutoGnum;
    o->enable_BarrierFarg      = IPA_Enable_BarrierFarg;
    o->enable_Opt_Alias        = IPA_Enable_Opt_Alias;
    o->enable_Simple_Alias     = IPA_Enable_Simple_Alias;
    o->enable_Addressing       = IPA_Enable_Addressing;
    o->enable_Readonly_Ref     = IPA_Enable_Readonly_Ref;
    o->enable_Cprop            = IPA_Enable_Cprop;
    o->enable_Cprop2           = IPA_Enable_Cprop2;
    o->enable_Assert           = IPA_Enable_Assert;
    o->enable_daVinci          = IPA_Enable_daVinci;
    o->enable_ipacom           = IPA_Enable_ipacom;
    o->enable_final_link       = IPA_Enable_final_link;
    o->enable_Memtrace         = IPA_Enable_Memtrace;
    o->enable_DST              = IPA_Enable_DST;
    o->enable_DCE              = IPA_Enable_DCE;
    o->enable_Exc              = IPA_Enable_Exc;
    o->enable_Recycle          = IPA_Enable_Recycle;
    o->enable_DVE              = IPA_Enable_DVE;
    o->enable_CGI              = IPA_Enable_CGI;
    o->enable_Copy_Prop        = IPA_Enable_Copy_Prop;
    o->enable_Padding          = IPA_Enable_Padding;
    o->common_Pad_Size         = IPA_Common_Pad_Size;
    o->enable_Split_Common     = IPA_Enable_Split_Common;
    o->enable_Cloning          = IPA_Enable_Cloning;
    o->enable_Partial_Inline   = IPA_Enable_Partial_Inline;
    o->echo_Commands           = IPA_Echo_Commands;
    o->enable_Lang             = IPA_Enable_Lang;
    o->enable_Preempt          = IPA_Enable_Preempt;
    o->enable_Flow_Analysis    = IPA_Enable_Flow_Analysis;
    o->enable_Array_Sections   = IPA_Enable_Array_Sections;
    o->enable_Array_Summary    = IPA_Enable_Array_Summary;
    o->enable_Scalar_Euse      = IPA_Enable_Scalar_Euse;
    o->enable_Scalar_Kill      = IPA_Enable_Scalar_Kill;
    o->enable_Common_Const     = IPA_Enable_Common_Const;
    o->enable_Relocatable_Opt  = IPA_Enable_Relocatable_Opt;
    o->enable_Feedback         = IPA_Enable_Feedback;
    o->enable_Alias_Class      = IPA_Enable_Alias_Class;
    o->debug_AC_Temp_Files     = IPA_Debug_AC_Temp_Files;
    o->enable_Reshape          = IPA_Enable_Reshape;
    o->enable_Preopt           = IPA_Enable_Preopt;
    o->enable_Preopt_Set       = IPA_Enable_Preopt_Set;
    o->enable_Siloed_Ref       = IPA_Enable_Siloed_Ref;
    o->enable_Siloed_Ref_Set   = IPA_Enable_Siloed_Ref_Set;

#ifdef KEY
    o->enable_Icall_Opt                = IPA_Enable_Icall_Opt;
    o->enable_EH_Region_Removal        = IPA_Enable_EH_Region_Removal;
    o->enable_Branch_Heuristic         = IPA_Enable_Branch_Heuristic;
    o->min_Branch_Prob                  = IPA_Min_Branch_Prob;
    o->check_Options                    = IPA_Check_Options;
    o->clone_List_Actions               = IPA_Clone_List_Actions;
    o->enable_Pure_Call_Opt             = IPA_Enable_Pure_Call_Opt;
    o->pure_Call_skip_before            = IPA_Pure_Call_skip_before;
    o->consult_Inliner_For_Icall_Opt   = IPA_Consult_Inliner_For_Icall_Opt;
    o->icall_Min_Freq                   = IPA_Icall_Min_Freq;
    o->enable_Source_PU_Order           = IPA_Enable_Source_PU_Order;
    o->enable_Struct_Opt                = IPA_Enable_Struct_Opt;
    o->enable_Global_As_Local           = IPA_Enable_Global_As_Local;
    o->update_Struct                    = IPA_Update_Struct;
#endif

    o->icall_Target_Min_Rate = IPA_Icall_Target_Min_Rate;

    /* --- Inlining heuristics --- */
    o->bloat_Factor        = IPA_Bloat_Factor;
    o->bloat_Factor_Set    = IPA_Bloat_Factor_Set;
    o->pU_Limit            = IPA_PU_Limit;
    o->pU_Limit_Set        = IPA_PU_Limit_Set;
    o->pU_Hard_Limit       = IPA_PU_Hard_Limit;
    o->pU_Hard_Limit_Set   = IPA_PU_Hard_Limit_Set;
    o->pU_Minimum_Size     = IPA_PU_Minimum_Size;
    o->small_Callee_Limit  = IPA_Small_Callee_Limit;
    o->max_Depth           = IPA_Max_Depth;
    o->force_Depth         = IPA_Force_Depth;
    o->force_Depth_Set     = IPA_Force_Depth_Set;
    o->min_Freq            = IPA_Min_Freq;
    o->rela_Freq           = IPA_Rela_Freq;
    o->min_Hotness         = IPA_Min_Hotness;
    o->use_Effective_Size  = IPA_Use_Effective_Size;

    /* --- Miscellaneous --- */
    o->enable_Merge_ty             = IPA_Enable_Merge_ty;
    o->max_Jobs                    = IPA_Max_Jobs;
    o->max_Jobs_Set                = IPA_Max_Jobs_Set;
    o->gspace                      = IPA_Gspace;
    o->user_gnum                   = IPA_user_gnum;
    o->extgot_Factor               = IPA_Extgot_Factor;
    o->num_Fortran_Intrinsics      = IPA_Num_Fortran_Intrinsics;
    o->has_Fortran                 = IPA_Has_Fortran;
    o->map_Limit                   = IPA_Map_Limit;
    o->enable_SP_Partition         = IPA_Enable_SP_Partition;
    o->enable_GP_Partition         = IPA_Enable_GP_Partition;
    o->space_Access_Mode           = IPA_Space_Access_Mode;
    o->group_Names                 = IPA_Group_Names;
    o->spec_Files                  = IPA_Spec_Files;
    o->skip                        = IPA_Skip;
    o->skip_Report                 = IPA_Skip_Report;
    o->enable_Keeplight            = IPA_Enable_Keeplight;
    o->enable_Cord                 = IPA_Enable_Cord;
    o->enable_Linearization        = IPA_Enable_Linearization;
    o->use_Intrinsic               = IPA_Use_Intrinsic;
    o->enable_Inline_Nested_PU     = IPA_Enable_Inline_Nested_PU;
    o->enable_Inline_Struct        = IPA_Enable_Inline_Struct;
    o->enable_Inline_Char_Array    = IPA_Enable_Inline_Char_Array;
    o->enable_Inline_Optional_Arg  = IPA_Enable_Inline_Optional_Arg;
    o->enable_Inline_Struct_Array_Actual = IPA_Enable_Inline_Struct_Array_Actual;
    o->enable_Inline_Var_Dim_Array = IPA_Enable_Inline_Var_Dim_Array;
    o->enable_Reorder              = IPA_Enable_Reorder;
    o->enable_AOT                  = IPA_Enable_AOT;

#ifdef KEY
    o->enable_PU_Reorder     = (IPA_OPT_PU_REORDER_SCHEME) IPA_Enable_PU_Reorder;
    o->enable_PU_Reorder_Set = IPA_Enable_PU_Reorder_Set;
    o->enable_Ctype          = IPA_Enable_Ctype;
    o->inline_Check_Compatibility =
        (IPA_OPT_CHECK_PARAM_COMPATIBILITY) INLINE_Check_Compatibility;
#endif

    o->max_Node_Clones         = IPA_Max_Node_Clones;
    o->max_Node_Clones_Set     = IPA_Max_Node_Clones_Set;
    o->max_Clone_Bloat         = IPA_Max_Clone_Bloat;
    o->max_Output_File_Size    = IPA_Max_Output_File_Size;
    o->output_File_Size        = IPA_Output_File_Size;
    o->max_Density             = IPA_Max_Density;
    o->enable_Old_Type_Merge   = IPA_Enable_Old_Type_Merge;

    /* --- Devirtualization --- */
    o->enable_Devirtualization          = IPA_Enable_Devirtualization;
    o->enable_Fast_Static_Analysis_VF   = IPA_Enable_Fast_Static_Analysis_VF;
    o->enable_Original_VF              = IPA_Enable_Original_VF;
    o->enable_New_VF                   = IPA_Enable_New_VF;
    o->inline_Original_VF              = IPA_Inline_Original_VF;
    o->inline_New_VF                   = IPA_Inline_New_VF;
    o->devirtualization_Input_File     = IPA_Devirtualization_Input_File;
    o->during_Original_VF              = IPA_During_Original_VF;
    o->during_New_VF                   = IPA_During_New_VF;

    /* --- Whole program / scale --- */
    o->enable_Whole_Program_Mode     = IPA_Enable_Whole_Program_Mode;
    o->enable_Whole_Program_Mode_Set = IPA_Enable_Whole_Program_Mode_Set;
    o->enable_Scale                  = IPA_Enable_Scale;

    /* --- INLINE group options --- */
    o->inline_Enable                       = INLINE_Enable;
    o->inline_All                          = INLINE_All;
    o->inline_Optimize_Alloca              = INLINE_Optimize_Alloca;
    o->inline_Enable_Copy_Prop             = INLINE_Enable_Copy_Prop;
    o->inline_Enable_Subst_Copy_Prop       = INLINE_Enable_Subst_Copy_Prop;
    o->inline_F90                          = INLINE_F90;
    o->inline_None                         = INLINE_None;
    o->inline_Exceptions                   = INLINE_Exceptions;
    o->inline_Keep_PU_Order                = INLINE_Keep_PU_Order;
    o->inline_List_Actions                 = INLINE_List_Actions;
    o->inline_Max_Pu_Size                  = INLINE_Max_Pu_Size;
    o->inline_Preemptible                  = INLINE_Preemptible;
    o->inline_Static                       = INLINE_Static;
    o->inline_Static_Set                   = INLINE_Static_Set;
    o->inline_Aggressive                   = INLINE_Aggressive;
    o->inline_First_Inline_Calls_In_Loops  = INLINE_First_Inline_Calls_In_Loops;
    o->inline_Enable_DFE                   = IPA_Enable_DFE; /* source is IPA_, not INLINE_ */
    o->inline_Enable_Split_Common          = INLINE_Enable_Split_Common;
    o->inline_Enable_Auto_Inlining         = INLINE_Enable_Auto_Inlining;
    o->inline_Enable_Restrict_Pointers     = INLINE_Enable_Restrict_Pointers;

#ifdef KEY
    o->inline_Recursive       = INLINE_Recursive;
    o->inline_Param_Mismatch  = INLINE_Param_Mismatch;
    o->inline_Type_Mismatch   = INLINE_Type_Mismatch;
    o->inline_Ignore_Bloat    = INLINE_Ignore_Bloat;
    o->inline_Callee_Limit    = INLINE_Callee_Limit;
#endif

    o->inline_List_Names               = INLINE_List_Names;
    o->inline_Spec_Files               = INLINE_Spec_Files;
    o->inline_Skip_After               = INLINE_Skip_After;
    o->inline_Skip_Before              = INLINE_Skip_Before;
    o->inline_Array_Bounds             = INLINE_Array_Bounds;
    o->inline_Use_Malloc_Mempool       = INLINE_Use_Malloc_Mempool;
    o->inline_Free_Malloc_Mempool      = INLINE_Free_Malloc_Mempool;
    o->inline_Inlined_Pu_Call_Graph    = INLINE_Inlined_Pu_Call_Graph;
    o->inline_Inlined_Pu_Call_Graph2   = INLINE_Inlined_Pu_Call_Graph2;
    o->inline_Get_Time_Info            = INLINE_Get_Time_Info;
    o->inline_Script_Name              = INLINE_Script_Name;
    o->inline_Enable_Script            = INLINE_Enable_Script;
    o->inline_Enable_Devirtualize      = INLINE_Enable_Devirtualize;

    g_ipa_options = ipa_options_mutable;
}


/* ====================================================================
 * IPA_Context_Init:  allocate and zero-fill the master context.
 * Calls IPA_Options_Init to snapshot the option globals.
 * ====================================================================
 */
void
IPA_Context_Init(void)
{
    if (g_ipa_ctx != NULL)
	return;

    g_ipa_ctx = (IPA_Context *) malloc(sizeof(IPA_Context));
    memset(g_ipa_ctx, 0, sizeof(IPA_Context));

    IPA_Options_Init();
}


/* ====================================================================
 * IPA_Context_Fini:  release the context and options structs.
 * ====================================================================
 */
void
IPA_Context_Fini(void)
{
    if (ipa_options_mutable != NULL) {
	free(ipa_options_mutable);
	ipa_options_mutable = NULL;
	g_ipa_options = NULL;
    }

    if (g_ipa_ctx != NULL) {
	free(g_ipa_ctx);
	g_ipa_ctx = NULL;
    }
}

#endif /* IPA_CONTEXT_FULL_INIT */
