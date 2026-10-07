/*
 * IPA Options context struct — encapsulates the ~140 config_ipa.h globals
 * into a single struct for eventual threading through IPA passes.
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
    char *feedback_Filename;
    char *annotation_Filename;

    /* --- Feature enable flags --- */
    BOOL enable_DFE;
    BOOL enable_DFE_Set;
    BOOL enable_Inline;
    BOOL enable_Picopt;
    BOOL enable_AutoGnum;
    BOOL enable_BarrierFarg;
    BOOL enable_Opt_Alias;
    BOOL enable_Simple_Alias;
    BOOL enable_Addressing;
    BOOL enable_Readonly_Ref;
    BOOL enable_Cprop;
    BOOL enable_Cprop2;
    BOOL enable_Assert;
    BOOL enable_daVinci;
    BOOL enable_ipacom;
    BOOL enable_final_link;
    BOOL enable_Memtrace;
    BOOL enable_DST;
    BOOL enable_DCE;
    BOOL enable_Exc;
    BOOL enable_Recycle;
    BOOL enable_DVE;
    BOOL enable_CGI;
    BOOL enable_Copy_Prop;
    BOOL enable_Padding;
    UINT32 common_Pad_Size;
    BOOL enable_Split_Common;
    BOOL enable_Cloning;
    BOOL enable_Partial_Inline;
    BOOL echo_Commands;
    BOOL enable_Lang;
    BOOL enable_Preempt;
    BOOL enable_Flow_Analysis;
    BOOL enable_Array_Sections;
    BOOL enable_Array_Summary;
    BOOL enable_Scalar_Euse;
    BOOL enable_Scalar_Kill;
    BOOL enable_Common_Const;
    BOOL enable_Relocatable_Opt;
    BOOL enable_Feedback;
    BOOL enable_Alias_Class;
    BOOL debug_AC_Temp_Files;
    BOOL enable_Reshape;
    BOOL enable_Preopt;
    BOOL enable_Preopt_Set;
    BOOL enable_Siloed_Ref;
    BOOL enable_Siloed_Ref_Set;

#ifdef KEY
    BOOL enable_Icall_Opt;
    BOOL enable_EH_Region_Removal;
    BOOL enable_Branch_Heuristic;
    float min_Branch_Prob;
    BOOL check_Options;
    BOOL clone_List_Actions;
    BOOL enable_Pure_Call_Opt;
    INT32 pure_Call_skip_before;
    BOOL consult_Inliner_For_Icall_Opt;
    UINT32 icall_Min_Freq;
    BOOL enable_Source_PU_Order;
    UINT32 enable_Struct_Opt;
    UINT32 enable_Global_As_Local;
    UINT32 update_Struct;
#endif /* KEY */

    UINT32 icall_Target_Min_Rate;

    /* --- Inlining heuristics --- */
    UINT32 bloat_Factor;
    BOOL   bloat_Factor_Set;
    UINT32 pU_Limit;
    BOOL   pU_Limit_Set;
    UINT32 pU_Hard_Limit;
    BOOL   pU_Hard_Limit_Set;
    UINT32 pU_Minimum_Size;
    UINT32 small_Callee_Limit;
    UINT32 max_Depth;
    UINT32 force_Depth;
    BOOL   force_Depth_Set;
    UINT32 min_Freq;
    UINT32 rela_Freq;
    UINT32 min_Hotness;
    BOOL   use_Effective_Size;

    /* --- Miscellaneous --- */
    BOOL   enable_Merge_ty;
    UINT32 max_Jobs;
    BOOL   max_Jobs_Set;
    UINT32 gspace;
    UINT32 user_gnum;
    UINT32 extgot_Factor;
    UINT32 num_Fortran_Intrinsics;
    BOOL   has_Fortran;
    UINT32 map_Limit;
    BOOL   enable_SP_Partition;
    BOOL   enable_GP_Partition;
    BOOL   space_Access_Mode;

    struct option_list *group_Names;
    struct option_list *spec_Files;
    struct option_list *skip;
    BOOL   skip_Report;

    BOOL   enable_Keeplight;
    BOOL   enable_Cord;
    BOOL   enable_Linearization;
    BOOL   use_Intrinsic;
    BOOL   enable_Inline_Nested_PU;
    BOOL   enable_Inline_Struct;
    BOOL   enable_Inline_Char_Array;
    BOOL   enable_Inline_Optional_Arg;
    BOOL   enable_Inline_Struct_Array_Actual;
    BOOL   enable_Inline_Var_Dim_Array;
    BOOL   enable_Reorder;
    BOOL   enable_AOT;

#ifdef KEY
    IPA_OPT_PU_REORDER_SCHEME enable_PU_Reorder;
    BOOL   enable_PU_Reorder_Set;
    BOOL   enable_Ctype;
    IPA_OPT_CHECK_PARAM_COMPATIBILITY inline_Check_Compatibility;
#endif /* KEY */

    UINT32 max_Node_Clones;
    BOOL   max_Node_Clones_Set;
    UINT32 max_Clone_Bloat;
    UINT32 max_Output_File_Size;
    INT32  output_File_Size;
    UINT32 max_Density;

    BOOL   enable_Old_Type_Merge;

    /* --- Devirtualization --- */
    BOOL enable_Devirtualization;
    BOOL enable_Fast_Static_Analysis_VF;
    BOOL enable_Original_VF;
    BOOL enable_New_VF;
    BOOL inline_Original_VF;
    BOOL inline_New_VF;
    const char *devirtualization_Input_File;
    BOOL during_Original_VF;
    BOOL during_New_VF;

    /* --- Whole program / scale --- */
    BOOL enable_Whole_Program_Mode;
    BOOL enable_Whole_Program_Mode_Set;
    BOOL enable_Scale;

    /* --- INLINE group options --- */
    BOOL   inline_Enable;
    BOOL   inline_All;
    BOOL   inline_Optimize_Alloca;
    BOOL   inline_Enable_Copy_Prop;
    BOOL   inline_Enable_Subst_Copy_Prop;
    BOOL   inline_F90;
    BOOL   inline_None;
    BOOL   inline_Exceptions;
    BOOL   inline_Keep_PU_Order;
    BOOL   inline_List_Actions;
    UINT32 inline_Max_Pu_Size;
    BOOL   inline_Preemptible;
    BOOL   inline_Static;
    BOOL   inline_Static_Set;
    BOOL   inline_Aggressive;
    BOOL   inline_First_Inline_Calls_In_Loops;
    BOOL   inline_Enable_DFE;
    BOOL   inline_Enable_Split_Common;
    BOOL   inline_Enable_Auto_Inlining;
    BOOL   inline_Enable_Restrict_Pointers;

#ifdef KEY
    BOOL   inline_Recursive;
    BOOL   inline_Param_Mismatch;
    BOOL   inline_Type_Mismatch;
    BOOL   inline_Ignore_Bloat;
    UINT32 inline_Callee_Limit;
#endif /* KEY */

    struct option_list *inline_List_Names;
    struct option_list *inline_Spec_Files;
    UINT32 inline_Skip_After;
    UINT32 inline_Skip_Before;
    BOOL   inline_Array_Bounds;
    BOOL   inline_Use_Malloc_Mempool;
    BOOL   inline_Free_Malloc_Mempool;
    BOOL   inline_Inlined_Pu_Call_Graph;
    BOOL   inline_Inlined_Pu_Call_Graph2;
    BOOL   inline_Get_Time_Info;
    char  *inline_Script_Name;
    BOOL   inline_Enable_Script;
    BOOL   inline_Enable_Devirtualize;
};

/* Transitional global — will be removed once all call-sites are threaded */
extern const IPA_Options *g_ipa_options;

#endif /* cxx_ipa_options_INCLUDED */
