/*
 *
 * IPA Options context struct — encapsulates the ~140 config_ipa.h globals
 * into a single struct for eventual threading through IPA passes.
 *
 */

#ifndef cxx_ipa_options_INCLUDED
#define cxx_ipa_options_INCLUDED

#ifndef defs_INCLUDED
#include "defs.h"		/* BOOL, INT, INT32, UINT32 */
#endif

struct option_list;

#ifdef KEY
/* Enums duplicated from config_ipa.h so this header is self-contained.
 * The canonical definitions remain in config_ipa.h; these must stay in sync.
 */
typedef enum {
    IPA_OPT_REORDER_DISABLE       = 0,
    IPA_OPT_REORDER_BY_NODE_FREQ  = 1,
    IPA_OPT_REORDER_BY_EDGE_FREQ  = 2,
    IPA_OPT_REORDER_BY_BFS        = 3
} IPA_OPT_PU_REORDER_SCHEME;

typedef enum {
    IPA_OPT_STRICT_CHECK  = 0,
    IPA_OPT_RELAXED_CHECK = 1,
    IPA_OPT_AGGRESSIVE    = 2
} IPA_OPT_CHECK_PARAM_COMPATIBILITY;
#endif /* KEY */

/* ====================================================================
 * IPA_Options: mirrors every extern in config_ipa.h.
 * Grouped by category for readability.
 * ====================================================================
 */
struct IPA_Options {

    /* --- Filenames --- */
    char *Feedback_Filename;
    char *Annotation_Filename;

    /* --- Feature enable flags --- */
    BOOL Enable_DFE;
    BOOL Enable_DFE_Set;
    BOOL Enable_Inline;
    BOOL Enable_Picopt;
    BOOL Enable_AutoGnum;
    BOOL Enable_BarrierFarg;
    BOOL Enable_Opt_Alias;
    BOOL Enable_Simple_Alias;
    BOOL Enable_Addressing;
    BOOL Enable_Readonly_Ref;
    BOOL Enable_Cprop;
    BOOL Enable_Cprop2;
    BOOL Enable_Assert;
    BOOL Enable_daVinci;
    BOOL Enable_ipacom;
    BOOL Enable_final_link;
    BOOL Enable_Memtrace;
    BOOL Enable_DST;
    BOOL Enable_DCE;
    BOOL Enable_Exc;
    BOOL Enable_Recycle;
    BOOL Enable_DVE;
    BOOL Enable_CGI;
    BOOL Enable_Copy_Prop;
    BOOL Enable_Padding;
    UINT32 Common_Pad_Size;
    BOOL Enable_Split_Common;
    BOOL Enable_Cloning;
    BOOL Enable_Partial_Inline;
    BOOL Echo_Commands;
    BOOL Enable_Lang;
    BOOL Enable_Preempt;
    BOOL Enable_Flow_Analysis;
    BOOL Enable_Array_Sections;
    BOOL Enable_Array_Summary;
    BOOL Enable_Scalar_Euse;
    BOOL Enable_Scalar_Kill;
    BOOL Enable_Common_Const;
    BOOL Enable_Relocatable_Opt;
    BOOL Enable_Feedback;
    BOOL Enable_Alias_Class;
    BOOL Debug_AC_Temp_Files;
    BOOL Enable_Reshape;
    BOOL Enable_Preopt;
    BOOL Enable_Preopt_Set;
    BOOL Enable_Siloed_Ref;
    BOOL Enable_Siloed_Ref_Set;

#ifdef KEY
    BOOL Enable_Icall_Opt;
    BOOL Enable_EH_Region_Removal;
    BOOL Enable_Branch_Heuristic;
    float Min_Branch_Prob;
    BOOL Check_Options;
    BOOL Clone_List_Actions;
    BOOL Enable_Pure_Call_Opt;
    INT32 Pure_Call_skip_before;
    BOOL Consult_Inliner_For_Icall_Opt;
    UINT32 Icall_Min_Freq;
    BOOL Enable_Source_PU_Order;
    UINT32 Enable_Struct_Opt;
    UINT32 Enable_Global_As_Local;
    UINT32 Update_Struct;
#endif /* KEY */

    UINT32 Icall_Target_Min_Rate;

    /* --- Inlining heuristics --- */
    UINT32 Bloat_Factor;
    BOOL   Bloat_Factor_Set;
    UINT32 PU_Limit;
    BOOL   PU_Limit_Set;
    UINT32 PU_Hard_Limit;
    BOOL   PU_Hard_Limit_Set;
    UINT32 PU_Minimum_Size;
    UINT32 Small_Callee_Limit;
    UINT32 Max_Depth;
    UINT32 Force_Depth;
    BOOL   Force_Depth_Set;
    UINT32 Min_Freq;
    UINT32 Rela_Freq;
    UINT32 Min_Hotness;
    BOOL   Use_Effective_Size;

    /* --- Miscellaneous --- */
    BOOL   Enable_Merge_ty;
    UINT32 Max_Jobs;
    BOOL   Max_Jobs_Set;
    UINT32 Gspace;
    UINT32 user_gnum;
    UINT32 Extgot_Factor;
    UINT32 Num_Fortran_Intrinsics;
    BOOL   Has_Fortran;
    UINT32 Map_Limit;
    BOOL   Enable_SP_Partition;
    BOOL   Enable_GP_Partition;
    BOOL   Space_Access_Mode;

    struct option_list *Group_Names;
    struct option_list *Spec_Files;
    struct option_list *Skip;
    BOOL   Skip_Report;

    BOOL   Enable_Keeplight;
    BOOL   Enable_Cord;
    BOOL   Enable_Linearization;
    BOOL   Use_Intrinsic;
    BOOL   Enable_Inline_Nested_PU;
    BOOL   Enable_Inline_Struct;
    BOOL   Enable_Inline_Char_Array;
    BOOL   Enable_Inline_Optional_Arg;
    BOOL   Enable_Inline_Struct_Array_Actual;
    BOOL   Enable_Inline_Var_Dim_Array;
    BOOL   Enable_Reorder;
    BOOL   Enable_AOT;

#ifdef KEY
    IPA_OPT_PU_REORDER_SCHEME Enable_PU_Reorder;
    BOOL   Enable_PU_Reorder_Set;
    BOOL   Enable_Ctype;
    IPA_OPT_CHECK_PARAM_COMPATIBILITY Inline_Check_Compatibility;
#endif /* KEY */

    UINT32 Max_Node_Clones;
    BOOL   Max_Node_Clones_Set;
    UINT32 Max_Clone_Bloat;
    UINT32 Max_Output_File_Size;
    INT32  Output_File_Size;
    UINT32 Max_Density;

    BOOL   Enable_Old_Type_Merge;

    /* --- Devirtualization --- */
    BOOL Enable_Devirtualization;
    BOOL Enable_Fast_Static_Analysis_VF;
    BOOL Enable_Original_VF;
    BOOL Enable_New_VF;
    BOOL Inline_Original_VF;
    BOOL Inline_New_VF;
    const char *Devirtualization_Input_File;
    BOOL During_Original_VF;
    BOOL During_New_VF;

    /* --- Whole program / scale --- */
    BOOL Enable_Whole_Program_Mode;
    BOOL Enable_Whole_Program_Mode_Set;
    BOOL Enable_Scale;

    /* --- INLINE group options --- */
    BOOL   Inline_Enable;
    BOOL   Inline_All;
    BOOL   Inline_Optimize_Alloca;
    BOOL   Inline_Enable_Copy_Prop;
    BOOL   Inline_Enable_Subst_Copy_Prop;
    BOOL   Inline_F90;
    BOOL   Inline_None;
    BOOL   Inline_Exceptions;
    BOOL   Inline_Keep_PU_Order;
    BOOL   Inline_List_Actions;
    UINT32 Inline_Max_Pu_Size;
    BOOL   Inline_Preemptible;
    BOOL   Inline_Static;
    BOOL   Inline_Static_Set;
    BOOL   Inline_Aggressive;
    BOOL   Inline_First_Inline_Calls_In_Loops;
    BOOL   Inline_Enable_DFE;
    BOOL   Inline_Enable_Split_Common;
    BOOL   Inline_Enable_Auto_Inlining;
    BOOL   Inline_Enable_Restrict_Pointers;

#ifdef KEY
    BOOL   Inline_Recursive;
    BOOL   Inline_Param_Mismatch;
    BOOL   Inline_Type_Mismatch;
    BOOL   Inline_Ignore_Bloat;
    UINT32 Inline_Callee_Limit;
#endif /* KEY */

    struct option_list *Inline_List_Names;
    struct option_list *Inline_Spec_Files;
    UINT32 Inline_Skip_After;
    UINT32 Inline_Skip_Before;
    BOOL   Inline_Array_Bounds;
    BOOL   Inline_Use_Malloc_Mempool;
    BOOL   Inline_Free_Malloc_Mempool;
    BOOL   Inline_Inlined_Pu_Call_Graph;
    BOOL   Inline_Inlined_Pu_Call_Graph2;
    BOOL   Inline_Get_Time_Info;
    char  *Inline_Script_Name;
    BOOL   Inline_Enable_Script;
    BOOL   Inline_Enable_Devirtualize;
};

/* Transitional global — will be removed once all call-sites are threaded */
extern const IPA_Options *g_ipa_options;

#endif /* cxx_ipa_options_INCLUDED */
