/*
 *
 * IPA Context — master struct that will eventually replace all IPA
 * module-level globals.  Sub-structs group related state by subsystem.
 *
 * Heavy types are forward-declared to keep this header lightweight.
 */

#ifndef cxx_ipa_context_INCLUDED
#define cxx_ipa_context_INCLUDED

#ifndef defs_INCLUDED
#include "defs.h"		/* BOOL, INT, INT32, UINT32 */
#endif

#ifndef mempool_INCLUDED
#include "mempool.h"		/* MEM_POOL */
#endif

#ifndef fb_freq_INCLUDED
#include "fb_freq.h"		/* FB_FREQ */
#endif

#include <cstdio>		/* FILE */

/* Forward declarations — avoids pulling in heavy headers */
class IPA_CALL_GRAPH;
class IPA_CLASS_HIERARCHY;
class IPA_PCG;
class IP_ALIAS_CLASSIFICATION;
class daVinci;
/* COMMON_SNODE_TBL is a typedef (HASH_TABLE<...>); forward-declare as void* */
struct Field_pos_;
typedef struct Field_pos_ Field_pos;
struct REORDER_CAND;
struct MERGED_ACCESS;

/* We need the vector type for MERGED_ACCESS_VECTOR */
#include <vector>
typedef std::vector<struct MERGED_ACCESS *> MERGED_ACCESS_VECTOR;


/* ====================================================================
 * Sub-structs — each groups the globals from one IPA subsystem.
 * ====================================================================
 */

/* --- Feedback file descriptors (ipa_feedback.h) --- */
struct IPA_Feedback_State {
    FILE *exl_fd;		/* exclusion list */
    FILE *dve_fd;		/* dead variable elimination */
    FILE *dfe_fd;		/* dead function elimination */
    FILE *prg_fd;		/* pragma file */
    FILE *con_fd;		/* constant propagation */
};

/* --- Struct optimization (ipa_struct_opt.h) --- */
#ifndef mtypes_INCLUDED
#include "mtypes.h"		/* TYPE_ID (UINT8) */
#endif
struct IPA_Struct_Opt_State {
    Field_pos *field_layout;
    INT split_count;
    TYPE_ID complete_struct_relayout_type_id;
    TYPE_ID struct_with_field_pointing_to_complete_struct_relayout_type_id[32];
    int struct_with_field_pointing_to_complete_struct_relayout_field_num[32];
    int num_structs_with_field_pointing_to_complete_struct_relayout;
};

/* --- Call-graph visualization (ipc_daVinci.h) --- */
struct IPA_Visualization_State {
    daVinci *cg_display;
};

/* --- Array section analysis (ipa_section_prop.h) --- */
struct IPA_Array_Section_State {
    MEM_POOL array_prop_pool;
    BOOL trace_ipa_sections;
};

/* --- Structure field reordering (ipa_reorder.h) --- */
struct IPA_Reorder_State {
    MERGED_ACCESS_VECTOR *merged_access;
    MEM_POOL local_pool;
    REORDER_CAND *candidate_ptr;
};

/* --- Class hierarchy graph (ipa_chg.h, ipa_pcg.h) --- */
struct IPA_CHG_State {
    IPA_CLASS_HIERARCHY *class_hierarchy;
    IPA_PCG *concurrency_graph;
};

/* --- Common block state (ipa_pad.h) --- */
struct IPA_Common_State {
    void *common_table;		/* COMMON_SNODE_TBL* — cast when migrated */
    INT pad_count;
};

/* --- Inlining statistics (ipa_inline.h, ipa_cg.h) --- */
struct IPA_Inline_Stats {
    INT total_prog_size;
    INT total_inlined;
    INT total_not_inlined;
    INT total_must_inlined;
    INT total_must_not_inlined;

    UINT32 orig_prog_weight;
    UINT32 total_dead_function_weight;
    UINT32 orig_prog_wn_count;
    UINT32 prog_wn_count;
    UINT32 total_dead_function_wn_count;

    FB_FREQ total_call_freq;
    FB_FREQ total_cycle_count;
    FB_FREQ total_cycle_count_2;
};

/* --- Constant propagation state (ipa_cprop.h) --- */
struct IPA_Cprop_State {
    MEM_POOL cprop_pool;
    MEM_POOL local_pool;
    MEM_POOL global_pool;
    INT constant_count;
    UINT32 max_total_clones;
    UINT32 num_total_clones;
};

/* --- Alias analysis state (ipaa.h) --- */
struct IPA_Alias_Analysis_State {
    /* Placeholder — will be populated when we migrate IPAA globals */
};

/* --- Type/symbol merge state --- */
struct IPA_Merge_State {
    /* Placeholder — will be populated when we migrate merge globals */
};

/* --- Compile state (ipc_compile.cxx) --- */
struct IPA_Compile_State {
    /* Placeholder — will be populated when we migrate compile globals */
};


/* ====================================================================
 * IPA_Context: the master context struct.
 * ====================================================================
 */
struct IPA_Context {

    /* --- Core pointers --- */
    IPA_CALL_GRAPH     *call_graph;
    BOOL                call_graph_built;
#ifdef KEY
    IPA_CALL_GRAPH     *graph_undirected;
#endif
    BOOL                opt_options_inconsistent;

    /* --- Per-subsystem state --- */
    IPA_Feedback_State       feedback;
    IPA_Struct_Opt_State     struct_opt;
    IPA_Visualization_State  visualization;
    IPA_Array_Section_State  array_section;
    IPA_Reorder_State        reorder;
    IPA_CHG_State            chg;
    IPA_Common_State         common;
    IPA_Inline_Stats         inline_stats;
    IPA_Cprop_State          cprop;
    IPA_Alias_Analysis_State alias_analysis;
    IPA_Merge_State          merge;
    IPA_Compile_State        compile;
};


/* Transitional global — will be removed once all call-sites are threaded */
extern IPA_Context *g_ipa_ctx;

/* Lifecycle functions (defined in ipa_context.cxx) */
extern void IPA_Context_Init(void);
extern void IPA_Context_Fini(void);

#endif /* cxx_ipa_context_INCLUDED */
