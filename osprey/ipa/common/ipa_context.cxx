/*
 *
 * IPA Context — initialization and cleanup.
 *
 * These functions exist but are NOT called by any existing code — zero behavior
 * change.  They populate the new structs FROM the existing globals so that
 * later steps can incrementally switch readers to use the context.
 */

#include "ipa_context.h"
#include "ipa_options.h"
#include "config_ipa.h"		/* all the IPA_Enable_* / INLINE_* globals */

#include <cstring>		/* memset */
#include <cstdlib>		/* malloc, free */

/* ====================================================================
 * Transitional globals
 * ====================================================================
 */
IPA_Context       *g_ipa_ctx     = NULL;
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
    o->Feedback_Filename   = Feedback_Filename;
    o->Annotation_Filename = Annotation_Filename;

    /* --- Feature enable flags --- */
    o->Enable_DFE              = IPA_Enable_DFE;
    o->Enable_DFE_Set          = IPA_Enable_DFE_Set;
    o->Enable_Inline           = IPA_Enable_Inline;
    o->Enable_Picopt           = IPA_Enable_Picopt;
    o->Enable_AutoGnum         = IPA_Enable_AutoGnum;
    o->Enable_BarrierFarg      = IPA_Enable_BarrierFarg;
    o->Enable_Opt_Alias        = IPA_Enable_Opt_Alias;
    o->Enable_Simple_Alias     = IPA_Enable_Simple_Alias;
    o->Enable_Addressing       = IPA_Enable_Addressing;
    o->Enable_Readonly_Ref     = IPA_Enable_Readonly_Ref;
    o->Enable_Cprop            = IPA_Enable_Cprop;
    o->Enable_Cprop2           = IPA_Enable_Cprop2;
    o->Enable_Assert           = IPA_Enable_Assert;
    o->Enable_daVinci          = IPA_Enable_daVinci;
    o->Enable_ipacom           = IPA_Enable_ipacom;
    o->Enable_final_link       = IPA_Enable_final_link;
    o->Enable_Memtrace         = IPA_Enable_Memtrace;
    o->Enable_DST              = IPA_Enable_DST;
    o->Enable_DCE              = IPA_Enable_DCE;
    o->Enable_Exc              = IPA_Enable_Exc;
    o->Enable_Recycle          = IPA_Enable_Recycle;
    o->Enable_DVE              = IPA_Enable_DVE;
    o->Enable_CGI              = IPA_Enable_CGI;
    o->Enable_Copy_Prop        = IPA_Enable_Copy_Prop;
    o->Enable_Padding          = IPA_Enable_Padding;
    o->Common_Pad_Size         = IPA_Common_Pad_Size;
    o->Enable_Split_Common     = IPA_Enable_Split_Common;
    o->Enable_Cloning          = IPA_Enable_Cloning;
    o->Enable_Partial_Inline   = IPA_Enable_Partial_Inline;
    o->Echo_Commands           = IPA_Echo_Commands;
    o->Enable_Lang             = IPA_Enable_Lang;
    o->Enable_Preempt          = IPA_Enable_Preempt;
    o->Enable_Flow_Analysis    = IPA_Enable_Flow_Analysis;
    o->Enable_Array_Sections   = IPA_Enable_Array_Sections;
    o->Enable_Array_Summary    = IPA_Enable_Array_Summary;
    o->Enable_Scalar_Euse      = IPA_Enable_Scalar_Euse;
    o->Enable_Scalar_Kill      = IPA_Enable_Scalar_Kill;
    o->Enable_Common_Const     = IPA_Enable_Common_Const;
    o->Enable_Relocatable_Opt  = IPA_Enable_Relocatable_Opt;
    o->Enable_Feedback         = IPA_Enable_Feedback;
    o->Enable_Alias_Class      = IPA_Enable_Alias_Class;
    o->Debug_AC_Temp_Files     = IPA_Debug_AC_Temp_Files;
    o->Enable_Reshape          = IPA_Enable_Reshape;
    o->Enable_Preopt           = IPA_Enable_Preopt;
    o->Enable_Preopt_Set       = IPA_Enable_Preopt_Set;
    o->Enable_Siloed_Ref       = IPA_Enable_Siloed_Ref;
    o->Enable_Siloed_Ref_Set   = IPA_Enable_Siloed_Ref_Set;

#ifdef KEY
    o->Enable_Icall_Opt                = IPA_Enable_Icall_Opt;
    o->Enable_EH_Region_Removal        = IPA_Enable_EH_Region_Removal;
    o->Enable_Branch_Heuristic         = IPA_Enable_Branch_Heuristic;
    o->Min_Branch_Prob                  = IPA_Min_Branch_Prob;
    o->Check_Options                    = IPA_Check_Options;
    o->Clone_List_Actions               = IPA_Clone_List_Actions;
    o->Enable_Pure_Call_Opt             = IPA_Enable_Pure_Call_Opt;
    o->Pure_Call_skip_before            = IPA_Pure_Call_skip_before;
    o->Consult_Inliner_For_Icall_Opt   = IPA_Consult_Inliner_For_Icall_Opt;
    o->Icall_Min_Freq                   = IPA_Icall_Min_Freq;
    o->Enable_Source_PU_Order           = IPA_Enable_Source_PU_Order;
    o->Enable_Struct_Opt                = IPA_Enable_Struct_Opt;
    o->Enable_Global_As_Local           = IPA_Enable_Global_As_Local;
    o->Update_Struct                    = IPA_Update_Struct;
#endif

    o->Icall_Target_Min_Rate = IPA_Icall_Target_Min_Rate;

    /* --- Inlining heuristics --- */
    o->Bloat_Factor        = IPA_Bloat_Factor;
    o->Bloat_Factor_Set    = IPA_Bloat_Factor_Set;
    o->PU_Limit            = IPA_PU_Limit;
    o->PU_Limit_Set        = IPA_PU_Limit_Set;
    o->PU_Hard_Limit       = IPA_PU_Hard_Limit;
    o->PU_Hard_Limit_Set   = IPA_PU_Hard_Limit_Set;
    o->PU_Minimum_Size     = IPA_PU_Minimum_Size;
    o->Small_Callee_Limit  = IPA_Small_Callee_Limit;
    o->Max_Depth           = IPA_Max_Depth;
    o->Force_Depth         = IPA_Force_Depth;
    o->Force_Depth_Set     = IPA_Force_Depth_Set;
    o->Min_Freq            = IPA_Min_Freq;
    o->Rela_Freq           = IPA_Rela_Freq;
    o->Min_Hotness         = IPA_Min_Hotness;
    o->Use_Effective_Size  = IPA_Use_Effective_Size;

    /* --- Miscellaneous --- */
    o->Enable_Merge_ty             = IPA_Enable_Merge_ty;
    o->Max_Jobs                    = IPA_Max_Jobs;
    o->Max_Jobs_Set                = IPA_Max_Jobs_Set;
    o->Gspace                      = IPA_Gspace;
    o->user_gnum                   = IPA_user_gnum;
    o->Extgot_Factor               = IPA_Extgot_Factor;
    o->Num_Fortran_Intrinsics      = IPA_Num_Fortran_Intrinsics;
    o->Has_Fortran                 = IPA_Has_Fortran;
    o->Map_Limit                   = IPA_Map_Limit;
    o->Enable_SP_Partition         = IPA_Enable_SP_Partition;
    o->Enable_GP_Partition         = IPA_Enable_GP_Partition;
    o->Space_Access_Mode           = IPA_Space_Access_Mode;
    o->Group_Names                 = IPA_Group_Names;
    o->Spec_Files                  = IPA_Spec_Files;
    o->Skip                        = IPA_Skip;
    o->Skip_Report                 = IPA_Skip_Report;
    o->Enable_Keeplight            = IPA_Enable_Keeplight;
    o->Enable_Cord                 = IPA_Enable_Cord;
    o->Enable_Linearization        = IPA_Enable_Linearization;
    o->Use_Intrinsic               = IPA_Use_Intrinsic;
    o->Enable_Inline_Nested_PU     = IPA_Enable_Inline_Nested_PU;
    o->Enable_Inline_Struct        = IPA_Enable_Inline_Struct;
    o->Enable_Inline_Char_Array    = IPA_Enable_Inline_Char_Array;
    o->Enable_Inline_Optional_Arg  = IPA_Enable_Inline_Optional_Arg;
    o->Enable_Inline_Struct_Array_Actual = IPA_Enable_Inline_Struct_Array_Actual;
    o->Enable_Inline_Var_Dim_Array = IPA_Enable_Inline_Var_Dim_Array;
    o->Enable_Reorder              = IPA_Enable_Reorder;
    o->Enable_AOT                  = IPA_Enable_AOT;

#ifdef KEY
    o->Enable_PU_Reorder     = (IPA_OPT_PU_REORDER_SCHEME) IPA_Enable_PU_Reorder;
    o->Enable_PU_Reorder_Set = IPA_Enable_PU_Reorder_Set;
    o->Enable_Ctype          = IPA_Enable_Ctype;
    o->Inline_Check_Compatibility =
        (IPA_OPT_CHECK_PARAM_COMPATIBILITY) INLINE_Check_Compatibility;
#endif

    o->Max_Node_Clones         = IPA_Max_Node_Clones;
    o->Max_Node_Clones_Set     = IPA_Max_Node_Clones_Set;
    o->Max_Clone_Bloat         = IPA_Max_Clone_Bloat;
    o->Max_Output_File_Size    = IPA_Max_Output_File_Size;
    o->Output_File_Size        = IPA_Output_File_Size;
    o->Max_Density             = IPA_Max_Density;
    o->Enable_Old_Type_Merge   = IPA_Enable_Old_Type_Merge;

    /* --- Devirtualization --- */
    o->Enable_Devirtualization          = IPA_Enable_Devirtualization;
    o->Enable_Fast_Static_Analysis_VF   = IPA_Enable_Fast_Static_Analysis_VF;
    o->Enable_Original_VF              = IPA_Enable_Original_VF;
    o->Enable_New_VF                   = IPA_Enable_New_VF;
    o->Inline_Original_VF              = IPA_Inline_Original_VF;
    o->Inline_New_VF                   = IPA_Inline_New_VF;
    o->Devirtualization_Input_File     = IPA_Devirtualization_Input_File;
    o->During_Original_VF              = IPA_During_Original_VF;
    o->During_New_VF                   = IPA_During_New_VF;

    /* --- Whole program / scale --- */
    o->Enable_Whole_Program_Mode     = IPA_Enable_Whole_Program_Mode;
    o->Enable_Whole_Program_Mode_Set = IPA_Enable_Whole_Program_Mode_Set;
    o->Enable_Scale                  = IPA_Enable_Scale;

    /* --- INLINE group options --- */
    o->Inline_Enable                       = INLINE_Enable;
    o->Inline_All                          = INLINE_All;
    o->Inline_Optimize_Alloca              = INLINE_Optimize_Alloca;
    o->Inline_Enable_Copy_Prop             = INLINE_Enable_Copy_Prop;
    o->Inline_Enable_Subst_Copy_Prop       = INLINE_Enable_Subst_Copy_Prop;
    o->Inline_F90                          = INLINE_F90;
    o->Inline_None                         = INLINE_None;
    o->Inline_Exceptions                   = INLINE_Exceptions;
    o->Inline_Keep_PU_Order                = INLINE_Keep_PU_Order;
    o->Inline_List_Actions                 = INLINE_List_Actions;
    o->Inline_Max_Pu_Size                  = INLINE_Max_Pu_Size;
    o->Inline_Preemptible                  = INLINE_Preemptible;
    o->Inline_Static                       = INLINE_Static;
    o->Inline_Static_Set                   = INLINE_Static_Set;
    o->Inline_Aggressive                   = INLINE_Aggressive;
    o->Inline_First_Inline_Calls_In_Loops  = INLINE_First_Inline_Calls_In_Loops;
    o->Inline_Enable_DFE                   = IPA_Enable_DFE;
    o->Inline_Enable_Split_Common          = INLINE_Enable_Split_Common;
    o->Inline_Enable_Auto_Inlining         = INLINE_Enable_Auto_Inlining;
    o->Inline_Enable_Restrict_Pointers     = INLINE_Enable_Restrict_Pointers;

#ifdef KEY
    o->Inline_Recursive       = INLINE_Recursive;
    o->Inline_Param_Mismatch  = INLINE_Param_Mismatch;
    o->Inline_Type_Mismatch   = INLINE_Type_Mismatch;
    o->Inline_Ignore_Bloat    = INLINE_Ignore_Bloat;
    o->Inline_Callee_Limit    = INLINE_Callee_Limit;
#endif

    o->Inline_List_Names               = INLINE_List_Names;
    o->Inline_Spec_Files               = INLINE_Spec_Files;
    o->Inline_Skip_After               = INLINE_Skip_After;
    o->Inline_Skip_Before              = INLINE_Skip_Before;
    o->Inline_Array_Bounds             = INLINE_Array_Bounds;
    o->Inline_Use_Malloc_Mempool       = INLINE_Use_Malloc_Mempool;
    o->Inline_Free_Malloc_Mempool      = INLINE_Free_Malloc_Mempool;
    o->Inline_Inlined_Pu_Call_Graph    = INLINE_Inlined_Pu_Call_Graph;
    o->Inline_Inlined_Pu_Call_Graph2   = INLINE_Inlined_Pu_Call_Graph2;
    o->Inline_Get_Time_Info            = INLINE_Get_Time_Info;
    o->Inline_Script_Name              = INLINE_Script_Name;
    o->Inline_Enable_Script            = INLINE_Enable_Script;
    o->Inline_Enable_Devirtualize      = INLINE_Enable_Devirtualize;

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
