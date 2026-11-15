/* Copyright (C) 2026 Open64 Project */
/* VHO H2 capture over the existing admitted common matmul/locality records.
 * Source logical requests and potential reuse are independent of D/S1/T2
 * transactions and stage costs; unknown physical service remains unknown.
 * No executable/native allocation or new algebra/alias proof is introduced.
 * H3 separately projects admitted work counts for physical estimation.
 * Designs: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md and
 * doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md and
 * doc/AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md. */
#include "dsl_movement_gemm.h"
#include "dsl_gemm_hierarchy.h"
#include "../com/dsl_movement_native.h"
#include "dsl_shape.h"
#include "config.h"
#include "config_dsl.h"
#include "pu_info.h"
#include <sstream>
#include <cstring>

namespace {
/* Frame a copied managed identity without retaining ST/descriptor pointers. */
template<class T> std::string VHO_DSL_movement_number(T value) {std::ostringstream out;out<<value;return out.str();}
}
/* Locality's existing alias state controls reuse permission. Same shape alone
 * never equates A/B/C storage, and target identity never supplies capacity. */
BOOL VHO_DSL_capture_gemm_movement(WN *scope,const DSL_TILE_SITE_RECORD &site,
    const DSL_TILE_PLAN_RECORD &tile,const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
    DSL_MOVEMENT_FACTS &out,std::string &reason)
{
    out=DSL_MOVEMENT_FACTS();
    if(!scope || !Current_PU_Info || PU_Info_tree_ptr(Current_PU_Info)!=scope || Opt_Level<3 || site.owner_pu_st!=WN_st_idx(scope) ||
        !tile.problem_m || !tile.problem_n || !tile.problem_k || tile.site_id!=site.id) {reason="movement_gemm_scope_or_geometry";return FALSE;}
    DSL_IR_NODE_RECORD node;DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if(!DSL_IR_Image_Get_Node(site.semantic_node_id,&node) || node.operand_count!=2 ||
        !DSL_IR_Image_Get_Opcode_Descriptor(node.opcode_descriptor_id,&descriptor) || descriptor.logical_operator!=OPR_DSLMATMUL) {
        reason="movement_gemm_source_join";return FALSE;
    }
    try {
        DSL_MOVEMENT_CAPTURE c;c.binding.family="GEMM";c.binding.owner=ST_name(WN_st(scope));
        c.binding.source_key=DSL_movement_native_key(scope,node.id);
        c.binding.device=DSL_GPU_PLAN_SNAPSHOT::Capture(VHO_DSL_GPU_Context);c.binding.device_key=c.binding.device.Cache_key();
        c.binding.controls="existing common matmul; F32; no transpose; source-order numerical permissions unchanged";
        c.work.owner="output(i,j), ordered k";c.work.domain="canonical-GEMM-source";c.work.width=tile.problem_k;
        if(!DSL_movement_mul(tile.problem_m,tile.problem_n,c.work.instances)) {reason="movement_gemm_work_overflow";return FALSE;}
        c.work.axes.push_back("i");c.work.extents.push_back(tile.problem_m);c.work.axes.push_back("j");c.work.extents.push_back(tile.problem_n);c.work.axes.push_back("k");c.work.extents.push_back(tile.problem_k);
        c.work.active_last=c.work.width;c.work.tail_policy="exact-source-domain; candidate-tile-tails-separate";c.work.numerical=c.binding.controls;
        uint64_t reads=0,writes=0,useful=0;std::ostringstream proof;
        proof<<node.id<<';'<<tile.problem_m<<';'<<tile.problem_n<<';'<<tile.problem_k;
        for(unsigned i=0;i<3;++i) {
            DSL_IR_VALUE_RECORD value;DSL_IR_VALUE_ID id=node.result_value_id;
            if(i<2) {DSL_IR_VALUE_REFERENCE_RECORD ref;if(!DSL_IR_Image_Get_Value_Reference(node.first_operand_reference_id+i,&ref) || ref.owner_node_id!=node.id || ref.ordinal!=i) throw std::runtime_error("movement_gemm_operand");id=ref.value_id;}
            if(!DSL_IR_Image_Get_Value(id,&value) || !TY_is_tensor_extension(value.ty) || TY_mtype(TY_tensor_element_ty(value.ty))!=MTYPE_F4) throw std::runtime_error("movement_gemm_F32");
            std::vector<uint64_t> shape;shape.push_back(i==1?tile.problem_k:tile.problem_m);shape.push_back(i==0?tile.problem_k:tile.problem_n);
            UINT64 native_shape[2];UINT32 rank=0;
            if(!DSL_Shape_Parse_Static_Dimensions(TY_tensor_attribute(value.ty,TY_TENSOR_SCHEMA_SHAPE),native_shape,2,&rank) || rank!=2 || native_shape[0]!=shape[0] || native_shape[1]!=shape[1]) throw std::runtime_error("movement_gemm_shape_changed");
            const char *layout=TY_tensor_attribute(value.ty,TY_TENSOR_SCHEMA_LAYOUT);
            bool layout_known=layout && (!strcmp(layout,"row_major") || !strcmp(layout,"row-major") || !strcmp(layout,"contiguous") || !strcmp(layout,"dense"));
            DSL_TENSOR_LOCALITY_FACT_RECORD fact;bool unique=DSL_tensor_locality_find_fact(locality,id,&fact) && fact.alias_state==DSL_TENSOR_ALIAS_PROVEN_UNIQUE;
            std::string name=VHO_DSL_movement_number(id),allocation=value.st?"ST/"+VHO_DSL_movement_number(value.st):"value/"+name;
            DSL_MOVEMENT_STORAGE storage=DSL_movement_dense_storage(name,allocation,"source-definition/"+name,i==0?"A":i==1?"B":"C",shape,4,unique);
            if(!layout_known) {storage.strides_known=false;storage.strides.clear();storage.layout=layout?layout:"unknown";}
            storage.first_use=i==2?"output-finalize":"source-ready";storage.last_use=i==2?"result-visible":"all-output-consumers";c.storages.push_back(storage);
            DSL_MOVEMENT_ACCESS access;access.counts_known=true;access.id=name+"/matmul";access.storage=name;access.version=storage.version;access.owner=c.work.owner;
            access.relation=i==0?"A[i,k]":i==1?"B[k,j]":"C[i,j]";access.extent=storage.bytes;access.multiplicity=i==0?tile.problem_n:i==1?tile.problem_m:1;
            access.reuse_scope=i==0?"output-column":i==1?"output-row":"single-output";access.reuse_proved=i<2 && unique && layout_known;
            if(i<2) {if(!DSL_movement_mul(storage.bytes,access.multiplicity,access.read_bytes) || !DSL_movement_add(reads,access.read_bytes,reads) || !DSL_movement_add(useful,storage.bytes,useful)) throw std::runtime_error("movement_gemm_traffic_overflow");DSL_movement_source_lifetime(c,name,storage.version,"invocation","existing-locality-and-pure-common-site");}
            else {access.write_bytes=storage.bytes;writes=storage.bytes;}c.accesses.push_back(access);
            proof<<';'<<id<<';'<<unique<<';'<<(layout?layout:"unknown");
        }
        c.binding.proof_key=proof.str();c.ledger.useful_reads=DSL_MOVEMENT_BYTES(useful);c.ledger.useful_writes=DSL_MOVEMENT_BYTES(writes);
        c.ledger.source_reads=DSL_MOVEMENT_BYTES(reads);c.ledger.source_writes=DSL_MOVEMENT_BYTES(writes);
        c.ledger.partial=DSL_MOVEMENT_BYTES(0);
        return DSL_MOVEMENT_FACTS::Create(c,out,reason);
    } catch(const std::exception &e) {reason=e.what();return FALSE;}
}

/* Reuse existing traffic/readiness count projection only; its cycle durations
 * are discarded. SIMT work is derived from the admitted tile, not a TC rate. */
bool VHO_DSL_gemm_h3_plan(const DSL_TILE_PLAN_RECORD &tile,const DSL_FETCH_PLAN_RECORD &fetch,
    const DSL_HARDWARE_DESCRIPTION_RECORD &hardware,DSL_H3_PLAN &out,std::string &reason)
{
    DSL_GEMM_READINESS_INPUT input;DSL_GEMM_READINESS_RESULT ignored;
    if(!VHO_DSL_fetch_readiness_cost(tile,fetch,hardware,&input,&ignored)) {reason="h3_gemm_count_projection";return false;}
    DSL_H3_PLAN plan;std::ostringstream id;id<<"gemm/t"<<tile.id<<"/f"<<fetch.id;plan.id=id.str();plan.executable=true;
    plan.capability="existing-SIMT-D-S1-T2";DSL_H3_STAGE stage;stage.id=plan.id;
    stage.ctas=input.ctas;stage.steps=input.steps;stage.threads=fetch.resources.threads_per_cta;
    stage.registers=(fetch.resources.register_bytes_per_thread+3)/4;stage.shared_allocation=fetch.resources.shared_bytes;
    stage.asynchronous=fetch.engine==DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC;stage.depth=stage.asynchronous?2:1;
    uint64_t mn,mk,kn,bytes,ops;
    if(!DSL_movement_mul(tile.problem_m,tile.problem_n,mn) || !DSL_movement_mul(tile.problem_m,tile.problem_k,mk) ||
        !DSL_movement_mul(tile.problem_k,tile.problem_n,kn) || !DSL_movement_add(mn,mk,bytes) ||
        !DSL_movement_add(bytes,kn,bytes) || !DSL_movement_mul(bytes,4,plan.live_bytes) ||
        !DSL_movement_mul(input.input_bytes,input.steps,stage.read_requests) ||
        !DSL_movement_mul(tile.cta_m,tile.cta_n,ops) || !DSL_movement_mul(ops,2,ops) ||
        !DSL_movement_mul(ops,tile.problem_k,stage.scalar_ops)) {reason="h3_gemm_count_overflow";return false;}
    stage.write_through_requests=input.output_bytes;stage.reusable_reads=stage.read_requests;stage.add_chain=stage.mul_chain=tile.problem_k;
    if(!(fetch.flags&DSL_FETCH_PLAN_FLAG_DIRECT)) {
        if(!DSL_movement_mul(stage.scalar_ops,4,bytes) || !DSL_movement_add(bytes,stage.read_requests,stage.shared_requests)) {reason="h3_gemm_shared_overflow";return false;}
        if(stage.asynchronous) {
            if(!DSL_movement_add(input.steps,1,stage.barriers)) {reason="h3_gemm_barrier_overflow";return false;}
        } else if(!DSL_movement_mul(fetch.barrier_count,input.steps,stage.barriers)) {reason="h3_gemm_barrier_overflow";return false;}
    }
    if(!DSL_movement_add(stage.scalar_ops,stage.read_requests/4,stage.issue_ops)) {reason="h3_gemm_issue_overflow";return false;}
    plan.stages.push_back(stage);out=plan;reason="projected";return true;
}

/* Count the same selected consumer the native builder emits. H2 source facts
 * are prerequisites, never rewritten to predicted shared traffic. Full fixed
 * geometry/layout/capability identity prevents equal-cost recipe aliasing. */
bool VHO_DSL_gemm_h4_plan(const DSL_TILE_PLAN_RECORD &tile,const DSL_FETCH_PLAN_RECORD &fetch,
    const DSL_HARDWARE_DESCRIPTION_RECORD &hardware,const DSL_MOVEMENT_FACTS &facts,DSL_H3_PLAN &out,std::string &reason)
{
    if(!facts.Available() || facts.Capture().binding.family!="GEMM" || facts.Capture().accesses.size()!=3 ||
        !facts.Capture().accesses[0].reuse_proved || !facts.Capture().accesses[1].reuse_proved ||
        facts.Capture().work.extents.size()!=3 || facts.Capture().work.extents[0]!=tile.problem_m ||
        facts.Capture().work.extents[1]!=tile.problem_n || facts.Capture().work.extents[2]!=tile.problem_k ||
        tile.legality!=DSL_OPT_LEGALITY_PROVEN || fetch.legality!=DSL_OPT_LEGALITY_PROVEN || fetch.resources.model!=1) {reason="h4_missing_admission";return false;}
    // Native semantic/type admission is the owner's earlier boundary. This
    // projection rechecks only copied resource/count evidence, with no TY read.
    DSL_FETCH_RESOURCE_RECORD resources;
    if(!VHO_DSL_fetch_resource_requirements(tile,fetch,&resources) ||
        memcmp(&resources,&fetch.resources,sizeof(resources))) {reason="h4_resource_recipe_changed";return false;}
    DSL_H3_PLAN plan;if(!VHO_DSL_gemm_h3_plan(tile,fetch,hardware,plan,reason)) return false;
    DSL_H3_STAGE &stage=plan.stages[0];
    if(!(fetch.flags&DSL_FETCH_PLAN_FLAG_DIRECT)) {
        DSL_GEMM_HIERARCHY_WORK work;
        if(!VHO_DSL_gemm_hierarchy_work(tile.cta_m,tile.cta_n,tile.cta_k,tile.thread_m,tile.thread_n,tile.problem_k,
            (fetch.flags&DSL_FETCH_PLAN_FLAG_RETAIN_B)!=0,work,reason)) return false;
        uint64_t requests,loads;
        if(!DSL_movement_add(work.shared_reads,work.shared_writes,stage.shared_requests) ||
            !DSL_movement_add(stage.read_requests,stage.write_through_requests,requests) ||
            !DSL_movement_add(requests/4,stage.shared_requests/4,loads) ||
            !DSL_movement_add(work.scalar_ops,loads,stage.issue_ops)) {reason="h4_issue_overflow";return false;}
        stage.scalar_ops=work.scalar_ops;
    }
    std::ostringstream identity;identity<<"h4/"<<plan.id<<"/"<<tile.problem_m<<','<<tile.problem_n<<','<<tile.problem_k<<','<<tile.source_descriptor_ty<<'/'<<tile.cta_m<<','<<tile.cta_n<<','<<tile.cta_k<<','
        <<tile.thread_m<<','<<tile.thread_n<<','<<tile.a_shared_layout<<','<<tile.b_shared_layout<<','<<tile.a_shared_stride<<','
        <<fetch.flags<<','<<fetch.stage_count<<','<<fetch.prefetch_distance<<','<<fetch.issue_point<<','<<fetch.arrival_barrier<<','<<fetch.wait_point;
    plan.id=stage.id=identity.str();plan.capability="H4-F32-D-S1-T2-and-scalar-B-retention";
    out=plan;reason="h4-owned-work";return true;
}
