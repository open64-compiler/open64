/* Copyright (C) 2026 Open64 Project */
/* Active-PU selected High transaction: detached search has finished. Reuse
 * PREOPT_DUONLY, LNO parent/loop/access/dependence/reduction builders and the
 * established journal/string-savepoint and child-checkpoint protocols.
 * H3 fixed-winner physical model and full-recipe binding are revalidated
 * before mutation. Designs: doc/AI-COMPILER-SEARCH-M4-ATOMIC-HIGH-v1.0.md and
 * doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md. No GPU lowering. */
#include "dsl_high_apply.h"
#include "config_dsl.h"
#include "dsl_high_binding_keys.h"
#include "../vho/tensor_selected_key.h"
#include "dsl_high_admission.h"
#include "lnopt_main.h"
#include "lnoutils.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "optimizer.h"
#include "dep_graph.h"
#include "reduc.h"
#include "debug.h"
#include "region_util.h"
#include "pu_info.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "config.h"
#include "config_opt.h"
#include "flags.h"
#include "glob.h"
#include "ir_bwrite.h"
#include "../vho/dsl_gemm_occupancy.h"
/* Historical names, domain-neutral string-table savepoint implementation.
 * Reuse the existing exported service without adding a be.so API/dependency.
 * No CKKS image/event/cryptographic operation is used by this transaction. */
#include "dsl_ckks_expand_internal.h"
#include <stdexcept>
#include <sstream>
#include <cstdlib>
#include <unistd.h>
#include <sys/wait.h>

extern BOOL LNO_enabled;
extern WN *Current_Func_Node;
extern WN_MAP Stmt_Dependence_Map;
extern WN_MAP Safe_Spec_Map;

namespace {
/* Options belong to native application, never the detached search closure. */
std::string DSL_apply_option(const char *name) {
    OPTION_GROUP *group=Get_Command_Line_Group(Common_Option_Groups,"LNO");
    if(group) for(OPTION_DESC *o=OGROUP_options(group);ODESC_kind(o)!=OVK_COUNT;++o)
        if(ODESC_name(o) && !strcmp(ODESC_name(o),name) && ODESC_kind(o)==OVK_NAME) {
            const char *value=*(const char **)ODESC_variable(o);return value?value:"";
        }
    return "";
}
/* Test faults are absent from production; no option/JSON can inject mutation. */
bool DSL_apply_fault(unsigned stage) {
#ifdef LNO_DSL_HIGH_APPLY_TEST
    const char *value=getenv("OPEN64_HIGH_APPLY_TEST");
    return value && strtoul(value,NULL,10)==stage;
#else
    return false;
#endif
}
/* Test-only stale-selection perturbation operates on detached records. */
void DSL_apply_test_input(DSL_HIGH_SEARCH_INPUT &input, DSL_HIGH_SEARCH_REPORT &report, DSL_HIGH_EPOCH &epoch) {
#ifdef LNO_DSL_HIGH_APPLY_TEST
    if(DSL_apply_fault(10)) epoch.Before_mutation();
    if(DSL_apply_fault(11)) input.source_key+="stale";
    if(DSL_apply_fault(12)) input.model.values[0]++;
    if(DSL_apply_fault(13)) input.gpu.scratch_budget++;
    if(DSL_apply_fault(14)) input.controls.global_proposals=1;
    if(DSL_apply_fault(15)) report.exploration.winner=99;
    if(DSL_apply_fault(16)) input.allow_reassociate=false;
    if(DSL_apply_fault(17)) for(unsigned i=0;i<report.exploration.candidates.size();++i)
        if(i!=report.exploration.winner && report.exploration.candidates[i].eligible) {
            report.exploration.winner=report.production.exploration.winner=i;break;
        }
    if(DSL_apply_fault(18)) ++report.exploration.candidates[report.exploration.winner].policy;
    if(DSL_apply_fault(19)) ++input.movement_profile.scenarios[0].cache_hit_ppm;
    if(DSL_apply_fault(20)) report.reuse=DSL_HIGH_REUSE_FACTS();
    if(DSL_apply_fault(21)) report.exploration.candidates[report.exploration.winner].descriptor.group=2;
    if(DSL_apply_fault(22)) ++report.exploration.candidates[report.exploration.winner].descriptor.column;
    if(DSL_apply_fault(23)) report.h3_model_key+="stale";
    if(DSL_apply_fault(24)) report.exploration.candidates[report.exploration.winner].h3.binding+="stale";
    if(DSL_apply_fault(25)) {
        ++report.exploration.candidates[report.exploration.winner].h3.rank_ns;
        ++report.production.exploration.candidates[report.exploration.winner].h3.rank_ns;
    }
    if(DSL_apply_fault(26)) report.data_movement=DSL_MOVEMENT_FACTS();
#endif
}
/* Copy an ordinary High image in a child: the existing writer consumes native
 * subsection state. No new PU is traversed or selected; parent state survives. */
bool DSL_apply_checkpoint(WN *root,const std::string &path) {
    if(path.empty()) return true;
    std::string temporary=path+".tmp";
    if(access(path.c_str(),F_OK)==0 || access(temporary.c_str(),F_OK)==0) return false;
    fflush(NULL);pid_t child=fork();if(child<0) return false;
    if(!child) {
        PU_Info image=*Current_PU_Info;
        PU_Info_next(&image)=NULL;PU_Info_child(&image)=NULL;
        Set_PU_Info_tree_ptr(&image,root);Set_PU_Info_state(&image,WT_TREE,Subsect_InMem);
        Set_PU_Info_depgraph_ptr(&image,NULL);Set_PU_Info_state(&image,WT_DEPGRAPH,Subsect_Missing);
        Irb_File_Name=const_cast<char *>(temporary.c_str());
        if(!Open_Output_Info(Irb_File_Name)) _exit(1);
        Write_PU_Info(&image);Write_Global_Info(&image);Close_Output_Info();
        if(link(temporary.c_str(),path.c_str())) _exit(1);
        _exit(unlink(temporary.c_str())?1:0);
    }
    int status=0;return waitpid(child,&status,0)==child && WIFEXITED(status) && WEXITSTATUS(status)==0;
}
struct DSL_HIGH_TABLE_JOURNAL {
    std::vector<std::vector<ST> > symbols;
    std::vector<TY> types;
    std::vector<PREG> pregs;
    std::vector<ST_TENSOR_METADATA_STORE> metadata;
    std::vector<TY_DSL_KV> attributes;
    unsigned labels,tcons;
    PU pu;PU_Info info;
    DSL_CKKS_STRTAB_SAVEPOINT strings;
    WN_MAP_TAB *maptab;
    std::vector<WN_MAP_ID> free_ids[WN_MAP_CATEGORIES];
    INT32 last_id[WN_MAP_CATEGORIES];
    std::vector<char> map_bytes[WN_MAP_CATEGORIES][WN_MAP_MAX];
    bool used[WN_MAP_MAX];
    /* Native map storage uses the established kind-specific element width. */
    size_t Map_width(unsigned map) const {
        return maptab->_kind[map]==WN_MAP_KIND_INT32?sizeof(INT32):
            maptab->_kind[map]==WN_MAP_KIND_INT64?sizeof(INT64):sizeof(void *);
    }
    /* Snapshot existing rows plus append boundaries, before any construction. */
    DSL_HIGH_TABLE_JOURNAL() : labels(LABEL_Table_Size(CURRENT_SYMTAB)),tcons(TCON_Table_Size()),
        pu(*Current_pu),info(*Current_PU_Info) {
        maptab=Current_Map_Tab;
        for(unsigned m=0;m<WN_MAP_MAX;++m) used[m]=maptab->_is_used[m];
        for(unsigned c=0;c<WN_MAP_CATEGORIES;++c) {
            last_id[c]=maptab->_last_map_id[c];
            if(maptab->_free_list_count[c]) free_ids[c].assign(maptab->_free_list[c],
                maptab->_free_list[c]+maptab->_free_list_count[c]);
            for(unsigned m=0;m<WN_MAP_MAX;++m) if(used[m] && maptab->_map_size[c][m]) {
                char *bytes=(char *)maptab->_mapping[c][m];
                map_bytes[c][m].assign(bytes,bytes+maptab->_map_size[c][m]*Map_width(m));
            }
        }
        for(unsigned l=GLOBAL_SYMTAB;l<=CURRENT_SYMTAB;++l) {
            std::vector<ST> rows;
            for(unsigned i=0;i<ST_Table_Size(l);++i) rows.push_back((*Scope_tab[l].st_tab)[i]);
            symbols.push_back(rows);
        }
        for(unsigned i=0;i<St_tensor_metadata.Size();++i) metadata.push_back(St_tensor_metadata[i]);
        for(unsigned i=0;i<Tensor_dsl_kv_table.Size();++i) attributes.push_back(Tensor_dsl_kv_table[i]);
        for(unsigned i=0;i<TY_Table_Size();++i) types.push_back(Ty_Table[i]);
        for(unsigned i=0;i<PREG_Table_Size(CURRENT_SYMTAB);++i) pregs.push_back((*Scope_tab[CURRENT_SYMTAB].preg_tab)[i]);
        if(!DSL_CKKS_Strtab_Save(&strings)) throw std::runtime_error("string_savepoint");
    }
    /* Builders reuse existing types/TCONs. Unexpected cache-extending activity
     * is a compiler invariant failure, not an unsafe truncate-and-continue. */
    void Check_cache_boundary() {
        FmtAssert(TY_Table_Size()==types.size() && TCON_Table_Size()==tcons &&
            ST_Table_Size(GLOBAL_SYMTAB)==symbols[0].size(),("selected High extended a global interning cache"));
    }
    /* Restore original row bytes and local append boundaries, then rebuild the
     * existing string hash. No dangling interned type/constant ID is created. */
    void Restore() {
        Check_cache_boundary();
        /* All staged roots are abandoned and their analysis maps are gone.
         * Restore source-map values and allocator IDs before ordinary LNO can
         * allocate again. Extra backing capacity remains zero and unreachable;
         * no staged node ID is exposed in fallback mapped images. */
        FmtAssert(Current_Map_Tab==maptab,("selected High changed map-table ownership"));
        for(unsigned c=0;c<WN_MAP_CATEGORIES;++c) {
            maptab->_last_map_id[c]=last_id[c];
            FmtAssert(maptab->_free_list_size[c]>=free_ids[c].size(),("map free-list rollback capacity"));
            for(unsigned i=0;i<free_ids[c].size();++i) maptab->_free_list[c][i]=free_ids[c][i];
            maptab->_free_list_count[c]=free_ids[c].size();
            for(unsigned m=0;m<WN_MAP_MAX;++m) if(used[m]) {
                FmtAssert(maptab->_is_used[m],("selected High retired a source map"));
                size_t size=maptab->_map_size[c][m]*Map_width(m);
                FmtAssert(size>=map_bytes[c][m].size(),("selected High shrank a source map"));
                if(size) memset(maptab->_mapping[c][m],0,size);
                if(!map_bytes[c][m].empty()) memcpy(maptab->_mapping[c][m],&map_bytes[c][m][0],map_bytes[c][m].size());
            }
        }
        for(unsigned l=GLOBAL_SYMTAB;l<=CURRENT_SYMTAB;++l) {
            const std::vector<ST> &rows=symbols[l-GLOBAL_SYMTAB];
            while(ST_Table_Size(l)>rows.size()) Scope_tab[l].st_tab->Delete_last();
            for(unsigned i=0;i<rows.size();++i) (*Scope_tab[l].st_tab)[i]=rows[i];
        }
        for(unsigned i=0;i<types.size();++i) Ty_Table[i]=types[i];
        Reset_PREG_Table_Size(CURRENT_SYMTAB,pregs.size());
        for(unsigned i=0;i<pregs.size();++i) (*Scope_tab[CURRENT_SYMTAB].preg_tab)[i]=pregs[i];
        while(LABEL_Table_Size(CURRENT_SYMTAB)>labels) Scope_tab[CURRENT_SYMTAB].label_tab->Delete_last();
        St_tensor_metadata.Delete_down_to(metadata.size());Tensor_dsl_kv_table.Delete_down_to(attributes.size());
        for(unsigned i=0;i<metadata.size();++i) St_tensor_metadata[i]=metadata[i];
        for(unsigned i=0;i<attributes.size();++i) Tensor_dsl_kv_table[i]=attributes[i];
        *Current_pu=pu;*Current_PU_Info=info;
        FmtAssert(DSL_CKKS_Strtab_Restore(&strings),("selected High string rollback failed"));
    }
};
struct DSL_HIGH_NATIVE_ANALYSES {
    WN_MAP parent,info,precom,array,statement,safe;
    DU_MANAGER *du;ALIAS_MANAGER *alias;ARRAY_DIRECTED_GRAPH16 *graph;REDUCTION_MANAGER *reductions;
    WN *root;
    /* Capture borrowed owning-phase handles; no native pointer escapes LNO. */
    DSL_HIGH_NATIVE_ANALYSES() : parent(Parent_Map),info(LNO_Info_Map),precom(LNO_Precom_Map),
        array(Array_Dependence_Map),statement(Stmt_Dependence_Map),safe(Safe_Spec_Map),
        du(Du_Mgr),alias(Alias_Mgr),graph(Array_Dependence_Graph),reductions(red_manager),root(Current_Func_Node) {}
    /* Install an independent map/manager bundle for the staged selected tree. */
    void Stage(WN *tree) {
        WN_MAP maps[6];for(unsigned i=0;i<6;++i) maps[i]=WN_MAP_UNDEFINED;
        DU_MANAGER *next_du=NULL;ALIAS_MANAGER *next_alias=NULL;
        try {
            for(unsigned i=0;i<6;++i) {
                maps[i]=WN_MAP_Create(&LNO_default_pool);
                if(maps[i]==WN_MAP_UNDEFINED) throw std::runtime_error("selected_map_capacity");
            }
            next_du=Create_Du_Manager(MEM_pu_nz_pool_ptr);
            next_alias=Create_Alias_Manager(MEM_pu_nz_pool_ptr,tree);
        } catch(...) {
            if(next_alias) Delete_Alias_Manager(next_alias,MEM_pu_nz_pool_ptr);
            if(next_du) Delete_Du_Manager(next_du,MEM_pu_nz_pool_ptr);
            for(unsigned i=0;i<6;++i) if(maps[i]!=WN_MAP_UNDEFINED) WN_MAP_Delete(maps[i]);
            throw;
        }
        Parent_Map=maps[0];LNO_Info_Map=maps[1];LNO_Precom_Map=maps[2];Array_Dependence_Map=maps[3];
        Stmt_Dependence_Map=maps[4];Safe_Spec_Map=maps[5];WN_SimpParentMap=Parent_Map;
        Current_Func_Node=tree;Du_Mgr=next_du;Alias_Mgr=next_alias;
        Array_Dependence_Graph=NULL;red_manager=NULL;
    }
    /* Restore borrowed original handles after destroying only staged analyses. */
    void Install() const {
        Parent_Map=parent;LNO_Info_Map=info;LNO_Precom_Map=precom;Array_Dependence_Map=array;
        Stmt_Dependence_Map=statement;Safe_Spec_Map=safe;WN_SimpParentMap=parent;
        Du_Mgr=du;Alias_Mgr=alias;Array_Dependence_Graph=graph;red_manager=reductions;Current_Func_Node=root;
    }
    /* Retire one unreachable bundle through its original owning services. */
    void Destroy() const {
        if(reductions) CXX_DELETE(reductions,&LNO_default_pool);
        if(graph) CXX_DELETE(graph,&LNO_default_pool);
        Delete_Du_Manager(du,MEM_pu_nz_pool_ptr);Delete_Alias_Manager(alias,MEM_pu_nz_pool_ptr);
        WN_MAP_Delete(parent);WN_MAP_Delete(info);WN_MAP_Delete(precom);WN_MAP_Delete(array);
        WN_MAP_Delete(statement);WN_MAP_Delete(safe);
    }
};
/* Independently admit the fixed winner, not a request to try another candidate.
 * Recheck source/model/resource/control identities, current driver H1c evidence
 * and native epoch before any selected mutation; refusal retains ordinary LNO. */
bool DSL_apply_preflight(WN *root,DSL_HIGH_EPOCH &epoch,const DSL_HIGH_BINDINGS &bindings,
    const DSL_HIGH_SNAPSHOT &snapshot,const DSL_HIGH_SEARCH_INPUT &input,const DSL_HIGH_SEARCH_REPORT &report,
    VHO_TENSOR_PROGRAM &selected,std::string &reason) {
    if(!report.controlled || !report.production.has_winner) return reason="no_selected_incumbent",false;
    if(!LNO_DSL_revalidate_high(root,Du_Mgr,Alias_Mgr,Array_Dependence_Graph,epoch,bindings,snapshot,reason) ||
        !LNO_DSL_validate_search_input(snapshot,bindings,input,reason)) return false;
    if(report.reuse.Available() && !report.reuse.Is_current(snapshot,epoch,bindings,input.algorithm))
        return reason="reuse_context_changed",false;
    if(report.data_movement.Available()) {
        DSL_MOVEMENT_FACTS fresh;
        if(!LNO_DSL_capture_movement(snapshot,epoch,bindings,input.algorithm,report.reuse,fresh,reason) ||
            !report.data_movement.Same_content(fresh) || !report.data_movement.Is_current(fresh.Capture().binding,true))
            return reason="movement_source_facts_changed",false;
    }
    if(input.movement_enabled && (!report.movement || !report.reuse.Available() ||
        report.movement_key!=LNO_DSL_movement_binding(input,report.reuse))) return reason="movement_binding_changed",false;
    unsigned free_maps=0;
    for(unsigned i=0;i<WN_MAP_MAX;++i) if(!Current_Map_Tab->_is_used[i]) ++free_maps;
    if(free_maps<48) return reason="analysis_map_capacity",false;
    if(Roundoff_Level<ROUNDOFF_ASSOC) return reason="application_numerical_policy",false;
    if(Cur_PU_Feedback) return reason="feedback_not_supported",false;
    if(PU_has_region(Get_Current_PU())) return reason="nested_regions_not_supported",false;
    if(PU_Info_child(Current_PU_Info)) return reason="nested_PUs_not_supported",false;
    DSL_HIGH_SEARCH_REPORT keys;LNO_DSL_search_report_keys(input,keys);
    if(report.manifest_key!=keys.manifest_key || report.resource_key!=keys.resource_key || report.model_key!=keys.model_key ||
        report.control_key!=VHO_mla_search_control_key(input.controls)) return reason="selected_binding_mismatch",false;
    unsigned winner=report.exploration.winner;
    if(winner>=report.exploration.candidates.size() || report.exploration.candidates.size()!=report.production.exploration.candidates.size() ||
        winner!=report.production.exploration.winner)
        return reason="selected_ordinal_mismatch",false;
    const VHO_MLA_SEARCH_CANDIDATE &candidate=report.exploration.candidates[winner];
    const VHO_MLA_SEARCH_CANDIDATE &sealed=report.production.exploration.candidates[winner];
    if(candidate.policy!=sealed.policy || candidate.width!=sealed.width || candidate.scratch!=sealed.scratch || candidate.total!=sealed.total)
        return reason="selected_candidate_changed",false;
    if(input.movement_enabled && (candidate.descriptor.group!=1 || !candidate.movement.modeled ||
        candidate.movement.descriptor_key!=VHO_mla_movement_descriptor_key(candidate) ||
        candidate.movement.descriptor_key!=sealed.movement.descriptor_key)) return reason="movement_candidate_unimplemented_or_changed",false;
    if(!candidate.evaluated || !candidate.eligible) return reason="selected_candidate_ineligible",false;
    if((VHO_DSL_H3_Search!=NULL)!=(!report.h3_model_key.empty())) return reason="h3_model_presence_changed",false;
    if(!report.h3_model_key.empty()) {
        DSL_H3_PLAN recipe;DSL_H3_ESTIMATE fresh;
        if(!VHO_DSL_H3_Search || VHO_DSL_H3_Search->Model().key!=report.h3_model_key ||
            !candidate.h3.complete || candidate.h3.key!=sealed.h3.key || candidate.h3.binding!=sealed.h3.binding || candidate.h3.rank_ns!=sealed.h3.rank_ns ||
            !VHO_mla_h3_plan(candidate,input.algorithm,input.gpu,report.data_movement,recipe,reason) ||
            !VHO_h3_estimate(report.data_movement,VHO_DSL_H3_Search->Model(),recipe,fresh,reason) ||
            fresh.key!=candidate.h3.key || fresh.binding!=candidate.h3.binding || fresh.rank_ns!=candidate.h3.rank_ns)
            return reason="h3_selected_model_or_estimate_changed",false;
    }
    for(unsigned i=0;i<report.exploration.candidates.size();++i) {
        const VHO_MLA_SEARCH_CANDIDATE &other=report.exploration.candidates[i];
        if(other.evaluated && other.eligible && (VHO_mla_candidate_rank(other)<VHO_mla_candidate_rank(candidate) || (VHO_mla_candidate_rank(other)==VHO_mla_candidate_rank(candidate) && i<winner)))
            return reason="selected_incumbent_not_best",false;
    }
    selected=input.algorithm;selected.fuse=selected.cooperative=selected.fuse_attention=selected.register_latent=true;
    selected.chunk_policy=candidate.policy;selected.split_width=candidate.width;
    VHO_TENSOR_SCHEDULE schedule;
    if(!VHO_tensor_schedule(selected,&schedule,NULL)) return reason="selected_schedule_invalid",false;
    const VHO_TENSOR_ATTENTION &attention=schedule.attention;
    UINT64 chunks=candidate.width?(attention.length+candidate.width-1)/candidate.width:1;
    if(chunks>1 && chunks*attention.rank>attention.length) return reason="private_tuple_capacity",false;
    UINT64 scratch=chunks>1?4*attention.batch*attention.heads*chunks*(attention.rank+2):0;
    if(scratch!=candidate.scratch || scratch>input.gpu.scratch_budget) return reason="selected_scratch_mismatch",false;
    for(unsigned i=0;i<schedule.kernels.size();++i) {
        const VHO_TENSOR_KERNEL &kernel=schedule.kernels[i];DSL_GEMM_OCCUPANCY_RESULT occupancy;
        if(!input.gpu.device.Fits_CTA(kernel.threads,input.gpu.registers_per_thread,VHO_tensor_shared_bytes(selected,kernel)) ||
            !DSL_gemm_occupancy(input.gpu.resources,32,input.gpu.sms,kernel.threads,4*input.gpu.registers_per_thread,
            VHO_tensor_shared_bytes(selected,kernel),VHO_tensor_grid(selected,kernel),&occupancy) || !occupancy.resident)
            return reason="selected_resource_refusal",false;
    }
    reason="closed_MLA_semantics_and_partition_proved";return true;
}
}

/* Application is separately opt-in. Native generation happens once, after the
 * observer's isolation guard and fresh preflight. Every late failure restores
 * original native rows/root/RID/analyses; rollback never reopens search. */
bool LNO_DSL_apply_selected_high(WN *&root,DU_MANAGER *&du,ALIAS_MANAGER *&alias,
    DSL_HIGH_EPOCH &epoch,const DSL_HIGH_BINDINGS &bindings,const DSL_HIGH_SNAPSHOT &snapshot,
    const DSL_HIGH_SEARCH_INPUT &original_input,const DSL_HIGH_SEARCH_REPORT &original_report,FILE *trace) {
    std::string mode=DSL_apply_option("high_apply");
    if(mode.empty() || mode=="off") return false;
    std::string reason;VHO_TENSOR_PROGRAM selected;
    DSL_HIGH_SEARCH_INPUT input=original_input;DSL_HIGH_SEARCH_REPORT report=original_report;
    DSL_apply_test_input(input,report,epoch);
    if(mode!="on" || DSL_apply_option("high_search")!="on" ||
        !DSL_apply_preflight(root,epoch,bindings,snapshot,input,report,selected,reason)) {
        if(trace) fprintf(trace,"AI_HIGH_APPLY 1 REFUSED reason=%s attempts=0 fallback=ordinary_lno\n",reason.empty()?"application_controls":reason.c_str());
        return false;
    }
    std::string checkpoint=DSL_apply_option("high_apply_checkpoint");
    if(!DSL_apply_checkpoint(root,checkpoint.empty()?"":checkpoint+".before.B")) {
        if(trace) fprintf(trace,"AI_HIGH_APPLY 1 REFUSED reason=before_checkpoint attempts=0 fallback=ordinary_lno\n");
        return false;
    }
    DSL_HIGH_TABLE_JOURNAL journal;DSL_HIGH_NATIVE_ANALYSES previous;
    WN *original=root,*staged=NULL;RID *rid=REGION_get_rid(root);RID saved_rid=*rid;
    bool staged_analyses=false;BOOL enabled=LNO_enabled;
    BOOL simplify=WN_Simplifier_Enable(FALSE);
    epoch.Before_mutation();
    if(trace) fprintf(trace,"AI_HIGH_APPLY 1 INVALIDATE old_snapshot_current=false attempts=1 candidates_materialized=1\n");
    try {
        WN *body=VHO_tensor_build_selected_high(selected,root);
        std::string explanation=Index_To_Str(WN_GetComment(WN_first(body)));
        if(!report.h3_model_key.empty()) explanation+=" H3 chose this existing executable recipe using H2 source/reuse facts and the pinned H20 physical-unit model "+report.h3_model_key+". Traffic, spills and latency are explicit model estimates, not GPU measurements; candidate exploration generated no WHIRL.";
        if(report.movement) explanation+=" MR2 selected an implemented G=1 plan under the explicit hierarchy/reuse profile; grouped-head candidates are analysis-only. Cache/HBM traffic and cycle costs are scenario estimates, not GPU measurements. The normalized per-head partial tuple and stable merge remain unchanged.";
        staged=WN_COPY_Tree(root);WN_func_body(staged)=body;
        if(DSL_apply_fault(1)) throw std::runtime_error("test_after_construction");
        previous.Stage(staged);staged_analyses=true;
        WB_LNO_Initialize(staged,Du_Mgr,Alias_Mgr,WBC_DISABLE);
        REGION_new_wn(staged,original);
        staged=Pre_Optimizer(PREOPT_DUONLY_PHASE,staged,Du_Mgr,Alias_Mgr);
        /* PREOPT rebuilds executable nodes and drops COMMENTs. Reattach the
         * exact selected explanation after its emitter, before verification. */
        WN_INSERT_BlockBefore(WN_func_body(staged),WN_first(WN_func_body(staged)),WN_CreateComment(explanation.c_str()));
        Current_Func_Node=staged;WB_LNO_Initialize(staged,Du_Mgr,Alias_Mgr,WBC_DISABLE);LWN_Parentize(staged);
        if(!Mark_Code(staged,FALSE,TRUE)) throw std::runtime_error("selected_loop_summaries");
        LNO_Build_Access(staged,&LNO_default_pool);
        if(DSL_apply_fault(2)) throw std::runtime_error("test_after_DU_alias_access");
        if(!Build_Array_Dependence_Graph(staged) || DSL_apply_fault(3)) throw std::runtime_error("selected_dependence_rebuild");
        red_manager=CXX_NEW(REDUCTION_MANAGER(&LNO_default_pool),&LNO_default_pool);
        red_manager->Build(staged,TRUE,FALSE);red_manager->Build(staged,FALSE,TRUE,Array_Dependence_Graph);
        DSL_HIGH_SNAPSHOT rebuilt;
        if(!LNO_DSL_admit_common_high(staged,Du_Mgr,Alias_Mgr,Array_Dependence_Graph,NULL) ||
            !LNO_DSL_capture_high(staged,Du_Mgr,Alias_Mgr,Array_Dependence_Graph,epoch,bindings,rebuilt,reason) ||
            !LNO_DSL_revalidate_high(staged,Du_Mgr,Alias_Mgr,Array_Dependence_Graph,epoch,bindings,rebuilt,reason))
            throw std::runtime_error("selected_analysis_verification/"+reason);
        /* Persist only fixed decisions and full rebind identities through the
         * existing metadata image. Future lowering verifies these obligations;
         * it must not infer a different width/policy or reopen search. */
        ST_IDX owner=WN_st_idx(staged);
        ST_tensor_bind_metadata(owner,"tensor.high.selected.schema",report.movement?"lno-selected-mla-v2":"lno-selected-mla-v1");
        if(report.movement) {
            ST_tensor_bind_metadata(owner,"tensor.high.selected.head_group","1");
            std::string seal="mla-movement-selected-v1;"+report.exploration.candidates[report.exploration.winner].movement.descriptor_key+";"+report.movement_key;
            ST_tensor_bind_metadata(owner,"tensor.high.selected.movement",seal.c_str());
        }
        ST_tensor_bind_metadata(owner,"tensor.high.selected.source",rebuilt.Source_key().c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.facts",rebuilt.Facts_key().c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.resources",report.resource_key.c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.model",report.model_key.c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.controls",report.control_key.c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.numerical",input.numeric.c_str());
        std::ostringstream width,policy,winner;
        width<<selected.split_width;policy<<selected.chunk_policy;winner<<report.exploration.winner;
        ST_tensor_bind_metadata(owner,"tensor.high.selected.width",width.str().c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.policy",policy.str().c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.winner",winner.str().c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.selected.scratch","private-role0-formals; capacity-proved");
        if (!input.gpu.device.Cache_key().empty())
            ST_tensor_bind_metadata(owner,"tensor.high.selected.device",input.gpu.device.Cache_key().c_str());
        ST_tensor_bind_metadata(owner,"tensor.high.production.input",LNO_DSL_production_input_key(input).c_str());
        /* The bridge must compare this exact native tree before translating the
         * closed selected algorithm. Analysis keys alone omit COMMENTs and do
         * not certify the physical High exit, which the bridge checks later. */
        ST_tensor_bind_metadata(owner,"tensor.high.accepted.schema","mla-selected-native-v1");
        ST_tensor_bind_metadata(owner,"tensor.high.accepted.key",VHO_tensor_selected_key(staged).c_str());
        journal.Check_cache_boundary();
        if(DSL_apply_fault(4)) throw std::runtime_error("test_before_commit");
        if(!DSL_apply_checkpoint(staged,checkpoint.empty()?"":checkpoint+".after.B"))
            throw std::runtime_error("after_checkpoint");
        WN_Simplifier_Enable(simplify);
        root=staged;du=Du_Mgr;alias=Alias_Mgr;
        Set_PU_Info_tree_ptr(Current_PU_Info,root);
        WB_LNO_Initialize(root,du,alias,WBC_DISABLE);
        previous.Destroy();
        if(trace) fprintf(trace,"AI_HIGH_APPLY 1 COMMITTED winner=%u width=%u policy=%u attempts=1 candidates_materialized=1 "
            "legality=closed_DAG_partition_and_numerical_contract parent=rebuilt DU=rebuilt alias=rebuilt access=rebuilt "
            "dependence=rebuilt reductions=rebuilt snapshot=current legacy_LNO=bypassed applied=true\n",
            report.exploration.winner,selected.split_width,selected.chunk_policy);
        rebuilt.Print(trace,"SELECTED_REBUILT");return true;
    } catch(const std::exception &error) {
        WN_Simplifier_Enable(simplify);
        reason=error.what();
        if(staged_analyses) {DSL_HIGH_NATIVE_ANALYSES failed;failed.Destroy();previous.Install();}
        *rid=saved_rid;WN_MAP_Set(RID_map,original,rid);
        journal.Restore();LNO_enabled=enabled;root=original;du=previous.du;alias=previous.alias;
        WB_LNO_Initialize(root,du,alias,WBC_DU_AND_ARRAY);
        DSL_HIGH_SNAPSHOT restored;
        FmtAssert(LNO_DSL_capture_high(root,du,alias,Array_Dependence_Graph,epoch,bindings,restored,reason) &&
            restored.Same_content(snapshot),("selected High rollback did not restore source/analysis facts"));
        if(trace) fprintf(trace,"AI_HIGH_APPLY 1 ROLLED_BACK reason=%s attempts=1 candidates_materialized=1 "
            "source_equal=true ST_TY_restored=true analyses_equal=true old_snapshot_current=false fallback=ordinary_lno\n",
            error.what());
        return false;
    }
}
