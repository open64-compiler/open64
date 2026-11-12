/* Copyright (C) 2026 Open64 Project */
/* Backend active-PU source identity adapter for H2. Reuses the common exact
 * tree/declaration key in explicit DSL-source mode and copies managed node,
 * operand and attribute rows through existing readers. No analysis/selection,
 * pointer retention or mapped layout change.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#ifndef dsl_movement_native_INCLUDED
#define dsl_movement_native_INCLUDED
#include "dsl_native_tree_key.h"
#include "../../common/ir_image/dsl_image_service.h"
/* Full owned source binding; throws on a missing managed join. Caller has
 * already established active-PU ownership and family semantic admissibility. */
inline std::string DSL_movement_native_key(WN *scope,DSL_IR_NODE_ID node_id) {
    std::ostringstream key;key<<DSL_NATIVE_TREE_KEY(true,true).Build(scope);
    DSL_IR_NODE_RECORD node;if(!DSL_IR_Image_Get_Node(node_id,&node)) throw std::runtime_error("movement_native_node");
    key<<";managed;"<<node.id<<';'<<node.opcode_descriptor_id<<';'<<node.flags<<';'<<node.result_value_id<<';'<<node.operand_count<<';'<<node.attribute_count<<';';
    for(UINT32 i=0;i<node.operand_count;++i) {
        DSL_IR_VALUE_REFERENCE_RECORD ref;
        if(!DSL_IR_Image_Get_Value_Reference(node.first_operand_reference_id+i,&ref) || ref.owner_node_id!=node.id || ref.ordinal!=i) throw std::runtime_error("movement_native_operand");
        key<<ref.value_id<<';';
    }
    for(UINT32 i=0;i<node.attribute_count;++i) {
        DSL_IR_ATTRIBUTE_RECORD a;if(!DSL_IR_Image_Get_Attribute(node.first_attribute_id+i,&a) || a.owner_node_id!=node.id) throw std::runtime_error("movement_native_attribute");
        std::string name=Index_To_Str(a.name),value=Index_To_Str(a.value);key<<a.value_kind<<';'<<a.flags<<';'<<name.size()<<':'<<name<<value.size()<<':'<<value<<';';
    }
    return key.str();
}
#endif
