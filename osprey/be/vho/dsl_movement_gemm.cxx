/* Copyright (C) 2026 Open64 Project */
/* VHO H2 capture over the existing admitted common matmul/locality records.
 * Source logical requests and potential reuse are independent of D/S1/T2
 * transactions and stage costs; unknown physical service remains unknown.
 * No executable/native allocation or new algebra/alias proof is introduced.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#include "dsl_movement_gemm.h"
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
