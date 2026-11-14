/* Copyright (C) 2026 Open64 Project */
/* Backend-owned single-entry Q1 buffer ABI selection. No model-name dispatch,
 * Python, external recipe tool, JIT or source interface-table reset.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. The standard terminal writer
 * omits source-only DSL interfaces only after canonical lowering succeeds. */
#include "quantized_driver.h"
#include "../vho/tensor_q1_program.h"
#include "config.h"
#include "config_opt.h"
#include "config_dsl.h"
#include "pu_info.h"
#include "stab.h"
#include "wn.h"
#include "wn_util.h"
#include "ir_bread.h"
#include "dsl_image_service.h"
#include "dsl_gatekeeper.h"
#include "errors.h"
#include <unistd.h>
#include <string>
namespace {
ST_IDX resident=ST_IDX_ZERO;
bool admitted=false;
VHO_TENSOR_PROGRAM program;
SRCPOS q1_source_pos=0;
/* Preserve the actual source file/line without registering another DST entry. */
SRCPOS source(WN *node) {
    SRCPOS pos=WN_Get_Linenum(node);if(pos && SRCPOS_filenum(pos) && SRCPOS_linenum(pos)) return pos;
    if(WN_operator(node)==OPR_BLOCK) {for(WN *s=WN_first(node);s;s=WN_next(s)) {pos=source(s);if(pos) return pos;}}
    else for(INT i=0;i<WN_kid_count(node);++i) if(WN_kid(node,i)) {pos=source(WN_kid(node,i));if(pos) return pos;}
    return 0;
}
/* Precise admission refusal preserves ordinary backend routing. */
BOOL refuse(FILE *file,const char *why) {
    if(file) fprintf(file,"AI_Q1_ROUTE 1 REFUSED reason=%s fallback=ordinary_backend native_mutation=false\n",why);
    return FALSE;
}
}
/* The explicit ABI opt-in authorizes exporting a standalone source entry as
 * disjoint storage buffers plus status. Existing call edges are never changed. */
BOOL BE_DSL_q1_prepare_file(PU_Info *pu,BOOL allowed,FILE *file) {
    if(!VHO_DSL_Q1_High || !strcmp(VHO_DSL_Q1_High,"off")) return FALSE;
    if(strcmp(VHO_DSL_Q1_High,"buffer-v1")) return refuse(file,"unsupported_abi");
    if(!allowed || !pu || PU_Info_next(pu) || PU_Info_child(pu) || DSL_Call_Image_Callsite_Count()) return refuse(file,"scope_or_call_edges");
    if(OPT_Fast_Math || Roundoff_Level!=ROUNDOFF_NONE || IEEE_Arithmetic!=IEEE_ACCURATE) {if(file) fprintf(file,"AI_Q1_ROUTE 1 NUMERIC fast_math=%d roundoff=%d ieee=%d\n",(int)OPT_Fast_Math,(int)Roundoff_Level,(int)IEEE_Arithmetic);return refuse(file,"strict_numeric_required");}
    for(unsigned i=0;i<WT_SUBSECTIONS;++i) if(i!=WT_TREE && i!=WT_SYMTAB && PU_Info_state(pu,i)!=Subsect_Missing)
        return refuse(file,"input_auxiliary_state");
    if(VHO_DSL_Q1_Checkpoint) {
        const char *suffixes[]={".source.before.B",".high.after.B",".preopt-0.before.B",".preopt-0.after.B"};
        for(unsigned i=0;i<4;++i) {
            std::string path=std::string(VHO_DSL_Q1_Checkpoint)+suffixes[i];
            if(!access(path.c_str(),F_OK) || !access((path+".tmp").c_str(),F_OK)) return refuse(file,"checkpoint_exists");
        }
    }
    Read_Local_Info(MEM_pu_nz_pool_ptr,pu);Current_PU_Info=pu;Current_pu=&PU_Info_pu(pu);
    Current_scope=PU_lexical_level(*Current_pu);Current_Map_Tab=PU_Info_maptab(pu);resident=PU_Info_proc_sym(pu);
    bool valid=VHO_tensor_prepare_quantized_program(pu,&program,NULL),q1=false;
    if(valid) for(unsigned i=0;i<program.operations.size();++i) {
        const VHO_TENSOR_OPERATION &op=program.operations[i];
        if(op.name=="common.round_even" || op.name=="common.gather" || op.name=="common.pack_u4" ||
           op.name=="common.unpack_u4" || op.name=="common.convert") q1=true;
    }
    q1_source_pos=source(PU_Info_tree_ptr(pu));valid=valid && q1_source_pos;
    // Read_Local_Info leaves a mapped-image pointer here; replace it with the
    // driver's saved scope before the ordinary per-PU setup restores it.
    Set_PU_Info_symtab_ptr(pu,NULL);
    Save_Local_Symtab(CURRENT_SYMTAB,pu);
    Current_scope=GLOBAL_SYMTAB;Current_pu=NULL;Current_PU_Info=NULL;Current_Map_Tab=NULL;
    if(!valid || !q1) return refuse(file,"source_contract");
    admitted=true;if(file) fprintf(file,"AI_Q1_ROUTE 1 ADMITTED abi=disjoint-storage-status-v1 operations=%lu search=false\n",(unsigned long)program.operations.size());
    return TRUE;
}
/* Identifies only the driver's resident source, including refused admissions. */
BOOL BE_DSL_q1_resident(PU_Info *pu) {return pu && resident && PU_Info_proc_sym(pu)==resident;}
/* This controls writer selection, never candidate eligibility. */
BOOL BE_DSL_q1_terminal() {return admitted;}
/* All recoverable gates precede native construction. The original entry and
 * ABI remain untouched on refusal; invariant/OOM failures after commit are
 * compiler failures, never ordinary fallback with a partially changed ABI. */
WN *BE_DSL_q1_route(PU_Info *pu,WN *tree,FILE *file) {
    if(!admitted || !BE_DSL_q1_resident(pu)) return tree;
    FmtAssert(Current_PU_Info==pu && PU_Info_tree_ptr(pu)==tree && VHO_tensor_validate_quantized_high(program,file), ("Q1 route lost active source binding"));
    extern bool VHO_q1_checkpoint(WN *,const char *,bool);
    FmtAssert(VHO_q1_checkpoint(tree,"source.before",false),("Q1 source checkpoint failed"));
    ST *function=WN_st(tree);
    WN *empty=WN_CreateEntry(0,function,WN_CreateBlock(),WN_CreateBlock(),WN_CreateBlock());
    WN_Set_Linenum(empty,WN_Get_Linenum(tree));Set_PU_Info_tree_ptr(pu,empty);
    FmtAssert(VHO_tensor_materialize_quantized_high(program,q1_source_pos,file),("Q1 semantic construction failed after admission"));
    WN *high=PU_Info_tree_ptr(pu);
    FmtAssert(DSL_Gatekeeper_Verify_No_Native_PU(pu,file) && VHO_q1_checkpoint(high,"high.after",true),("Q1 High verification/checkpoint failed"));
    if(file) fprintf(file,"AI_Q1_ROUTE 1 APPLIED buffers_and_status=%d source_symbol=%s search=false\n",WN_num_formals(high),ST_name(function));
    return high;
}
