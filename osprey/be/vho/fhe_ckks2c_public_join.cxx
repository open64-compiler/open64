/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: implement the read-only S6-0d D4 join from context-specialized
 * CKKS groups to the canonical public FHE ABI-v1 selector surface.  This file
 * constructs no calls and performs no tree/image mutation; those operations
 * remain behind the reviewed standard-call builder and generic grouped
 * lowering transaction.
 *
 * Design references:
 *   doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D4
 *   doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#include "fhe_ckks2c_public_join.h"
#include "../../common/ir_image/dsl_image_service.h"

#include <algorithm>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "dsl_ckks_event.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_plan.h"
#include "fhe_semantic_runtime_lower.h"
#include "fhe_sha256.h"
#include "strtab.h"

namespace {

struct Owner_Info {
  uint32_t identity_id;
  uint32_t owner_pu_st;
  std::string signature;
  uint32_t representative_identity_id;
  uint32_t representative_owner_pu_st;
};

struct Source_Key {
  std::string owner_signature;
  std::string value_name;
  uint32_t logical_operator;
  uint32_t operator_version;
  uint32_t result_ty;

  /* Order canonical sources without relying on mapped table allocation. */
  bool operator<(const Source_Key &other) const
  {
    if (owner_signature != other.owner_signature)
      return owner_signature < other.owner_signature;
    if (value_name != other.value_name)
      return value_name < other.value_name;
    if (logical_operator != other.logical_operator)
      return logical_operator < other.logical_operator;
    if (operator_version != other.operator_version)
      return operator_version < other.operator_version;
    return result_ty < other.result_ty;
  }
};

struct Source_Instance {
  uint32_t owner_pu_st;
  uint32_t node_id;
  uint32_t value_id;
  uint32_t identity_id;
  uint32_t logical_operator;
  uint32_t operator_version;
  uint32_t result_ty;
  std::string value_name;

  Source_Instance()
      : owner_pu_st(0), node_id(0), value_id(0), identity_id(0),
        logical_operator(0), operator_version(0), result_ty(0) {}
};

struct Canonical_Source {
  Source_Key key;
  Source_Instance representative;
  std::vector<Source_Instance> instances;
};

struct Canonical_Source_Less {
  /* Reproduce the original schedule's PU-then-source physical order. */
  bool operator()(const Canonical_Source &left,
                  const Canonical_Source &right) const
  {
    if (left.representative.identity_id != right.representative.identity_id)
      return left.representative.identity_id <
             right.representative.identity_id;
    return left.representative.node_id < right.representative.node_id;
  }
};

struct Source_Instance_Less {
  /* Keep clone contexts in stable PU-source identity order. */
  bool operator()(const Source_Instance &left,
                  const Source_Instance &right) const
  {
    return left.identity_id < right.identity_id;
  }
};

struct Group_Origin_Less {
  /* Order one source's expanded evaluations by persisted origin ordinal. */
  bool operator()(const VHO_FHE_CKKS2C_GROUP *left,
                  const VHO_FHE_CKKS2C_GROUP *right) const
  {
    return left->origin_static_ordinal < right->origin_static_ordinal;
  }
};

struct Image_Group_Key {
  uint32_t owner_pu_st;
  uint32_t source_value_id;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;
  uint32_t origin_static_ordinal;

  /* Typed event identity is stable across mapped table allocation. */
  bool operator<(const Image_Group_Key &other) const
  {
    if (owner_pu_st != other.owner_pu_st)
      return owner_pu_st < other.owner_pu_st;
    if (source_value_id != other.source_value_id)
      return source_value_id < other.source_value_id;
    if (context_pu_identity_id != other.context_pu_identity_id)
      return context_pu_identity_id < other.context_pu_identity_id;
    if (context_callsite_id != other.context_callsite_id)
      return context_callsite_id < other.context_callsite_id;
    return origin_static_ordinal < other.origin_static_ordinal;
  }
};

/* Emit one stable preflight diagnostic without changing caller output. */
bool Report(FILE *diagnostic, const char *message, uint32_t id)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHECKKS2C-JOIN-001: id=%u %s\n", id, message);
  return false;
}

/* Copy one required mapped string before any later table activity. */
bool Required_String(STR_IDX index, std::string *value)
{
  if (value == NULL || index == STR_IDX_ZERO || index >= STR_Table_Size())
    return false;
  const char *text = Index_To_Str(index);
  if (text == NULL || text[0] == 0)
    return false;
  *value = text;
  return true;
}

/* Build the exact structured interface signature for one PU identity. */
bool Owner_Signature(const DSL_PU_SOURCE_IDENTITY_RECORD &identity,
                     std::string *signature)
{
  if (signature == NULL || identity.id == 0 || identity.owner_pu_st == 0)
    return false;
  std::string definition;
  std::string module;
  std::string file;
  if (!Required_String(identity.canonical_definition_name, &definition) ||
      !Required_String(identity.defining_module, &module) ||
      !Required_String(identity.defining_file, &file))
    return false;
  std::map<uint32_t, uint32_t> formals;
  for (uint32_t id = 1; id <= DSL_PU_Interface_Image_Formal_Count(); ++id) {
    DSL_PU_FORMAL_RECORD formal;
    if (!DSL_PU_Interface_Image_Get_Formal(id, &formal))
      return false;
    if (formal.owner_pu_st != identity.owner_pu_st)
      continue;
    if (formal.formal_ty == 0 ||
        !formals.insert(std::make_pair(
            formal.formal_ordinal, formal.formal_ty)).second)
      return false;
  }
  if (formals.empty())
    return false;
  std::ostringstream stream;
  stream << definition << '\n' << module << '\n' << file << '\n'
         << identity.defining_line << '\n' << formals.size() << '\n';
  uint32_t ordinal = 0;
  for (std::map<uint32_t, uint32_t>::const_iterator formal = formals.begin();
       formal != formals.end(); ++formal, ++ordinal) {
    if (formal->first != ordinal)
      return false;
    stream << formal->first << ':' << formal->second << '\n';
  }
  *signature = stream.str();
  return true;
}

/* Canonicalize equivalent clone PUs through definition and formal TY facts. */
bool Collect_Owners(std::map<uint32_t, Owner_Info> *owners,
                    uint32_t *signature_count, FILE *diagnostic)
{
  if (owners == NULL || signature_count == NULL)
    return false;
  std::map<std::string, std::pair<uint32_t, uint32_t> > representatives;
  std::vector<Owner_Info> pending;
  for (uint32_t id = 1; id <= DSL_Call_Image_PU_Identity_Count(); ++id) {
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    Owner_Info owner;
    if (!DSL_Call_Image_Get_PU_Identity(id, &identity) || identity.id != id ||
        !Owner_Signature(identity, &owner.signature))
      return Report(diagnostic, "PU identity or interface is malformed", id);
    owner.identity_id = id;
    owner.owner_pu_st = identity.owner_pu_st;
    if (owners->count(owner.owner_pu_st) != 0)
      return Report(diagnostic, "PU owner identity is duplicated", id);
    std::map<std::string, std::pair<uint32_t, uint32_t> >::iterator prior =
        representatives.find(owner.signature);
    if (prior == representatives.end())
      representatives[owner.signature] =
          std::make_pair(owner.identity_id, owner.owner_pu_st);
    pending.push_back(owner);
  }
  if (pending.empty())
    return Report(diagnostic, "PU identity table is empty", 0);
  for (size_t index = 0; index < pending.size(); ++index) {
    const std::pair<uint32_t, uint32_t> representative =
        representatives[pending[index].signature];
    pending[index].representative_identity_id = representative.first;
    pending[index].representative_owner_pu_st = representative.second;
    (*owners)[pending[index].owner_pu_st] = pending[index];
  }
  *signature_count = representatives.size();
  return true;
}

/* Translate one retained source operator to its public ABI expansion. */
bool Operation_Kinds(uint32_t logical_operator,
                     std::vector<uint32_t> *kinds)
{
  if (kinds == NULL)
    return false;
  kinds->clear();
  switch (logical_operator) {
  case OPR_DSLCONV2D:
    kinds->push_back(VHO_FHE_RUNTIME_OP_CONV2D_PLAIN);
    break;
  case OPR_DSLRESIDUALADD:
    kinds->push_back(VHO_FHE_RUNTIME_OP_RESIDUAL_ADD);
    break;
  case OPR_DSLRELU:
    kinds->push_back(VHO_FHE_RUNTIME_OP_BOOTSTRAP);
    kinds->push_back(VHO_FHE_RUNTIME_OP_RELU_NORMALIZE);
    kinds->push_back(VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE);
    kinds->push_back(VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE);
    kinds->push_back(VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE);
    kinds->push_back(VHO_FHE_RUNTIME_OP_RELU_RECONSTRUCT);
    break;
  case OPR_DSLGLOBALAVGPOOL2D:
    kinds->push_back(VHO_FHE_RUNTIME_OP_AVERAGE_POOL);
    break;
  case OPR_DSLFLATTEN:
    kinds->push_back(VHO_FHE_RUNTIME_OP_LAYOUT_CONVERT);
    break;
  case OPR_DSLLINEAR:
    kinds->push_back(VHO_FHE_RUNTIME_OP_LINEAR_PLAIN);
    break;
  default:
    return false;
  }
  return true;
}

/* Collect dynamic source instances from private groups, then require one
 * representative disposition for every canonical source family.  Specialized
 * clones may omit duplicate disposition rows, but they never create a new
 * public selector.  Flatten is the sole disposition-only, zero-group source. */
bool Collect_Sources(const std::map<uint32_t, Owner_Info> &owners,
                     const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
                     std::vector<Canonical_Source> *sources,
                     FILE *diagnostic)
{
  if (sources == NULL)
    return false;
  typedef std::pair<uint32_t, uint32_t> Owner_Value;
  std::map<Source_Key, Canonical_Source> canonical;
  std::map<Owner_Value, Source_Key> instance_keys;
  std::set<Source_Key> authorized;
  for (size_t index = 0; index < groups.size(); ++index) {
    const VHO_FHE_CKKS2C_GROUP &group = groups[index];
    DSL_IR_NODE_RECORD node;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    std::map<uint32_t, Owner_Info>::const_iterator owner =
        owners.find(group.owner_pu_st);
    if (owner == owners.end())
      return Report(diagnostic, "private source owner is absent",
                    group.owner_pu_st);
    if (!DSL_IR_Image_Get_Node(group.source_node_id, &node) ||
        node.id != group.source_node_id)
      return Report(diagnostic, "private source node is absent",
                    group.source_node_id);
    if (!DSL_IR_Image_Get_Value(group.source_value_id, &value) ||
        value.id != group.source_value_id)
      return Report(diagnostic, "private source value is absent",
                    group.source_value_id);
    if (!DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode))
      return Report(diagnostic, "private source opcode is absent",
                    node.opcode_descriptor_id);
    if (node.result_value_id != group.source_value_id ||
        value.producer_node_id != node.id)
      return Report(diagnostic, "private source producer identity changed",
                    group.source_value_id);
    if (opcode.logical_operator != group.source_logical_operator ||
        opcode.version != group.source_operator_version)
      return Report(diagnostic, "private source opcode identity changed",
                    group.source_value_id);
    std::vector<uint32_t> kinds;
    std::string value_name;
    if (!Operation_Kinds(opcode.logical_operator, &kinds) ||
        opcode.logical_operator == OPR_DSLFLATTEN || opcode.version != 2 ||
        !Required_String(value.name, &value_name) ||
        value_name != group.source_value_name)
      return Report(diagnostic, "private source contract is unsupported",
                    index + 1);
    Source_Key key;
    key.owner_signature = owner->second.signature;
    key.value_name = value_name;
    key.logical_operator = opcode.logical_operator;
    key.operator_version = opcode.version;
    key.result_ty = value.ty;
    const Owner_Value owner_value(group.owner_pu_st, group.source_value_id);
    std::map<Owner_Value, Source_Key>::const_iterator prior =
        instance_keys.find(owner_value);
    if (prior != instance_keys.end()) {
      if (prior->second < key || key < prior->second)
        return Report(diagnostic, "private source identity changed", index + 1);
      continue;
    }
    instance_keys[owner_value] = key;
    Source_Instance instance;
    instance.owner_pu_st = group.owner_pu_st;
    instance.node_id = node.id;
    instance.value_id = value.id;
    instance.identity_id = owner->second.identity_id;
    instance.logical_operator = opcode.logical_operator;
    instance.operator_version = opcode.version;
    instance.result_ty = value.ty;
    instance.value_name = value_name;
    Canonical_Source &entry = canonical[key];
    entry.key = key;
    entry.instances.push_back(instance);
    if (instance.owner_pu_st == owner->second.representative_owner_pu_st) {
      if (entry.representative.owner_pu_st != 0)
        return Report(diagnostic, "canonical source is duplicated", index + 1);
      entry.representative = instance;
    }
  }
  for (uint32_t id = 1;
       id <= DSL_FHE_Plan_Conversion_Disposition_Count(); ++id) {
    DSL_FHE_CONVERSION_DISPOSITION_RECORD disposition;
    DSL_IR_NODE_RECORD node;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    std::map<uint32_t, Owner_Info>::const_iterator owner;
    if (!DSL_FHE_Plan_Get_Conversion_Disposition(id, &disposition) ||
        !DSL_IR_Image_Get_Node(disposition.source_node_id, &node) ||
        !DSL_IR_Image_Get_Value(disposition.result_value_id, &value) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode) ||
        node.id != disposition.source_node_id ||
        node.result_value_id != disposition.result_value_id ||
        value.id != disposition.result_value_id ||
        value.producer_node_id != node.id ||
        (owner = owners.find(disposition.owner_pu_st)) == owners.end())
      return Report(diagnostic, "source disposition join is malformed", id);
    std::vector<uint32_t> kinds;
    if (!Operation_Kinds(opcode.logical_operator, &kinds))
      continue;
    if (opcode.version != 2)
      return Report(diagnostic, "public source operator version is unsupported",
                    id);
    std::string value_name;
    if (!Required_String(value.name, &value_name))
      return Report(diagnostic, "public source name is absent", id);
    Source_Key key;
    key.owner_signature = owner->second.signature;
    key.value_name = value_name;
    key.logical_operator = opcode.logical_operator;
    key.operator_version = opcode.version;
    key.result_ty = value.ty;
    if (opcode.logical_operator == OPR_DSLFLATTEN) {
      if (instance_keys.count(Owner_Value(
              disposition.owner_pu_st, disposition.result_value_id)) != 0 ||
          owner->second.owner_pu_st !=
              owner->second.representative_owner_pu_st)
        return Report(diagnostic, "flatten owns a private or clone source", id);
      Source_Instance instance;
      instance.owner_pu_st = disposition.owner_pu_st;
      instance.node_id = node.id;
      instance.value_id = value.id;
      instance.identity_id = owner->second.identity_id;
      instance.logical_operator = opcode.logical_operator;
      instance.operator_version = opcode.version;
      instance.result_ty = value.ty;
      instance.value_name = value_name;
      Canonical_Source &entry = canonical[key];
      if (!entry.instances.empty())
        return Report(diagnostic, "flatten source is duplicated", id);
      entry.key = key;
      entry.instances.push_back(instance);
      entry.representative = instance;
    } else {
      std::map<Owner_Value, Source_Key>::const_iterator instance =
          instance_keys.find(Owner_Value(
              disposition.owner_pu_st, disposition.result_value_id));
      if (instance == instance_keys.end() || instance->second < key ||
          key < instance->second)
        return Report(diagnostic,
                      "disposition does not authorize a private source", id);
    }
    if (disposition.owner_pu_st ==
        owner->second.representative_owner_pu_st)
      authorized.insert(key);
  }
  if (canonical.empty())
    return Report(diagnostic, "no public semantic sources were retained", 0);
  for (std::map<Source_Key, Canonical_Source>::const_iterator entry =
           canonical.begin(); entry != canonical.end(); ++entry) {
    Canonical_Source copy = entry->second;
    if (copy.representative.owner_pu_st == 0 ||
        authorized.count(copy.key) != 1)
      return Report(diagnostic,
                    "canonical source lacks representative disposition", 0);
    std::sort(copy.instances.begin(), copy.instances.end(),
              Source_Instance_Less());
    sources->push_back(copy);
  }
  std::sort(sources->begin(), sources->end(), Canonical_Source_Less());
  return true;
}

/* Append one descriptor-selected dynamic binding to a private CKKS group. */
void Append_Group_Binding(const VHO_FHE_CKKS2C_GROUP &group,
                          const VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR &descriptor,
                          VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN *plan)
{
  VHO_FHE_CKKS2C_PUBLIC_BINDING binding;
  binding.binding_kind = VHO_FHE_CKKS2C_PUBLIC_PRIVATE_GROUP;
  binding.static_ordinal = descriptor.static_ordinal;
  binding.operation_kind = descriptor.operation_kind;
  binding.specialized_owner_pu_st = group.owner_pu_st;
  binding.specialized_source_value_id = group.source_value_id;
  binding.context_pu_identity_id = group.context_pu_identity_id;
  binding.context_callsite_id = group.context_callsite_id;
  binding.group_output_value_id = group.group_output_value_id;
  binding.group_identity_sha256 = group.identity_sha256;
  plan->bindings.push_back(binding);
}

/* Append the sole public representation event without inventing CKKS work. */
void Append_Representation_Binding(
    const Source_Instance &source,
    const VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR &descriptor,
    VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN *plan)
{
  VHO_FHE_CKKS2C_PUBLIC_BINDING binding;
  binding.binding_kind = VHO_FHE_CKKS2C_PUBLIC_REPRESENTATION_ONLY;
  binding.static_ordinal = descriptor.static_ordinal;
  binding.operation_kind = descriptor.operation_kind;
  binding.specialized_owner_pu_st = source.owner_pu_st;
  binding.specialized_source_value_id = source.value_id;
  binding.context_pu_identity_id = source.identity_id;
  binding.context_callsite_id = 0;
  binding.group_output_value_id = 0;
  plan->bindings.push_back(binding);
}

/* Serialize the completed join into one deterministic authentication digest. */
bool Join_Identity(const VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN &plan,
                   std::string *digest)
{
  if (digest == NULL)
    return false;
  std::ostringstream bytes;
  bytes << "open64.fhe.ckks2c.public-join.v1\n";
  for (size_t i = 0; i < plan.descriptors.size(); ++i) {
    const VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR &row = plan.descriptors[i];
    bytes << 'D' << '\n' << row.canonical_owner_pu_st << '\n'
          << row.canonical_source_node_id << '\n'
          << row.canonical_source_value_id << '\n'
          << row.static_ordinal << '\n' << row.operation_kind << '\n'
          << row.execution_multiplicity << '\n';
  }
  for (size_t i = 0; i < plan.bindings.size(); ++i) {
    const VHO_FHE_CKKS2C_PUBLIC_BINDING &row = plan.bindings[i];
    bytes << 'B' << '\n' << row.binding_kind << '\n'
          << row.static_ordinal << '\n' << row.operation_kind << '\n'
          << row.specialized_owner_pu_st << '\n'
          << row.specialized_source_value_id << '\n'
          << row.context_pu_identity_id << '\n'
          << row.context_callsite_id << '\n'
          << row.group_output_value_id << '\n'
          << row.group_identity_sha256 << '\n';
  }
  const std::string serialized = bytes.str();
  *digest = VHO_FHE_SHA256(
      reinterpret_cast<const unsigned char *>(serialized.data()),
      serialized.size());
  return digest->size() == 64;
}

/* Reconstruct the source/group keys needed for public ordinal selection
 * without opening any owner-local symbol table. Full group plans and hashes
 * remain the final collected-join authority. */
bool Collect_Image_Groups(std::vector<VHO_FHE_CKKS2C_GROUP> *groups,
                          FILE *diagnostic)
{
  if (groups == NULL || !DSL_CKKS_Event_Image_Validate(diagnostic))
    return false;
  std::map<Image_Group_Key, VHO_FHE_CKKS2C_GROUP> grouped;
  for (uint32_t id = 1; id <= DSL_CKKS_Event_Image_Count(); ++id) {
    DSL_CKKS_EVENT_RECORD event;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    std::string value_name;
    if (!DSL_CKKS_Event_Image_Get(id, &event) || event.id != id ||
        !DSL_IR_Image_Get_Value(event.source_value_id, &value) ||
        !DSL_IR_Image_Get_Node(event.source_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode) ||
        node.id != event.source_node_id ||
        node.result_value_id != value.id ||
        value.producer_node_id != node.id ||
        !Required_String(value.name, &value_name))
      return Report(diagnostic, "typed event source join failed", id);
    Image_Group_Key key = {event.owner_pu_st, event.source_value_id,
                           event.context_pu_identity_id,
                           event.context_callsite_id,
                           event.origin_static_ordinal};
    std::map<Image_Group_Key, VHO_FHE_CKKS2C_GROUP>::iterator found =
        grouped.find(key);
    if (found == grouped.end()) {
      VHO_FHE_CKKS2C_GROUP group;
      group.owner_pu_st = event.owner_pu_st;
      group.source_value_id = event.source_value_id;
      group.source_node_id = event.source_node_id;
      group.source_logical_operator = opcode.logical_operator;
      group.source_operator_version = opcode.version;
      group.source_value_name = value_name;
      group.context_pu_identity_id = event.context_pu_identity_id;
      group.context_callsite_id = event.context_callsite_id;
      group.origin_owner_pu_st = event.origin_owner_pu_st;
      group.origin_source_value_id = event.origin_source_value_id;
      group.origin_static_ordinal = event.origin_static_ordinal;
      group.group_output_value_id = 0;
      found = grouped.insert(std::make_pair(key, group)).first;
    }
    VHO_FHE_CKKS2C_GROUP &group = found->second;
    if (group.source_node_id != event.source_node_id ||
        group.source_logical_operator != opcode.logical_operator ||
        group.source_operator_version != opcode.version ||
        group.source_value_name != value_name ||
        group.origin_owner_pu_st != event.origin_owner_pu_st ||
        group.origin_source_value_id != event.origin_source_value_id)
      return Report(diagnostic, "typed event group identity changed", id);
    group.group_output_value_id = event.result_value_id;
  }
  if (grouped.empty())
    return Report(diagnostic, "typed event image has no public routes", 0);
  std::vector<VHO_FHE_CKKS2C_GROUP> candidate;
  for (std::map<Image_Group_Key, VHO_FHE_CKKS2C_GROUP>::const_iterator row =
           grouped.begin(); row != grouped.end(); ++row)
    candidate.push_back(row->second);
  groups->swap(candidate);
  return true;
}

}  // namespace

/* Build the immutable public selector route before owner-local PU mutation. */
bool VHO_FHE_CKKS2C_Build_Public_Routes_From_Image(
    std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> *routes,
    VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT *result, FILE *diagnostic)
{
  if (routes == NULL || result == NULL)
    return Report(diagnostic, "public route output is absent", 0);
  std::map<uint32_t, Owner_Info> owners;
  uint32_t canonical_pus = 0;
  std::vector<VHO_FHE_CKKS2C_GROUP> groups;
  std::vector<Canonical_Source> sources;
  if (!Collect_Owners(&owners, &canonical_pus, diagnostic) ||
      !Collect_Image_Groups(&groups, diagnostic) ||
      !Collect_Sources(owners, groups, &sources, diagnostic))
    return false;

  typedef std::pair<uint32_t, uint32_t> Owner_Value;
  std::map<Owner_Value, std::vector<const VHO_FHE_CKKS2C_GROUP *> >
      by_source;
  for (uint32_t index = 0; index < groups.size(); ++index)
    by_source[Owner_Value(groups[index].owner_pu_st,
                          groups[index].source_value_id)].push_back(
        &groups[index]);

  std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> candidate;
  uint32_t static_ordinal = 1;
  uint32_t representation_count = 0;
  for (uint32_t source_index = 0; source_index < sources.size();
       ++source_index) {
    const Canonical_Source &source = sources[source_index];
    std::vector<uint32_t> kinds;
    if (!Operation_Kinds(source.key.logical_operator, &kinds))
      return Report(diagnostic, "image route operator is unsupported", 0);
    const uint32_t first_public_ordinal = static_ordinal;
    static_ordinal += kinds.size();
    for (uint32_t instance_index = 0;
         instance_index < source.instances.size(); ++instance_index) {
      const Source_Instance &instance = source.instances[instance_index];
      std::vector<const VHO_FHE_CKKS2C_GROUP *> matched =
          by_source[Owner_Value(instance.owner_pu_st, instance.value_id)];
      if (source.key.logical_operator == OPR_DSLFLATTEN) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_VALUE_REFERENCE_RECORD operand;
        if (!matched.empty() || kinds.size() != 1 ||
            !DSL_IR_Image_Get_Node(instance.node_id, &node) ||
            node.id != instance.node_id || node.result_value_id !=
                instance.value_id || node.operand_count != 1 ||
            node.first_operand_reference_id == 0 ||
            !DSL_IR_Image_Get_Value_Reference(
                node.first_operand_reference_id, &operand) ||
            operand.owner_node_id != node.id || operand.ordinal != 0 ||
            operand.value_id == DSL_IR_VALUE_INVALID_ID ||
            operand.flags != 0 || operand.reserved != 0)
          return Report(diagnostic,
                        "flatten image route is malformed or owns private work",
                        instance.value_id);
        VHO_FHE_CKKS2C_PUBLIC_ROUTE route;
        route.binding_kind = VHO_FHE_CKKS2C_PUBLIC_REPRESENTATION_ONLY;
        route.specialized_owner_pu_st = instance.owner_pu_st;
        route.specialized_source_value_id = instance.value_id;
        route.context_pu_identity_id = instance.identity_id;
        route.context_callsite_id = 0;
        route.origin_static_ordinal = 0;
        route.public_static_ordinal = first_public_ordinal;
        route.operation_kind = kinds[0];
        route.representation_operand_value_id = operand.value_id;
        candidate.push_back(route);
        ++representation_count;
        continue;
      }
      if (matched.size() != kinds.size())
        return Report(diagnostic,
                      "image route group count does not match source",
                      instance.value_id);
      std::sort(matched.begin(), matched.end(), Group_Origin_Less());
      for (uint32_t offset = 0; offset < matched.size(); ++offset) {
        const VHO_FHE_CKKS2C_GROUP &group = *matched[offset];
        if (offset != 0 &&
            group.origin_static_ordinal !=
                matched[0]->origin_static_ordinal + offset)
          return Report(diagnostic, "image route ordinals are not dense",
                        instance.value_id);
        VHO_FHE_CKKS2C_PUBLIC_ROUTE route;
        route.binding_kind = VHO_FHE_CKKS2C_PUBLIC_PRIVATE_GROUP;
        route.specialized_owner_pu_st = group.owner_pu_st;
        route.specialized_source_value_id = group.source_value_id;
        route.context_pu_identity_id = group.context_pu_identity_id;
        route.context_callsite_id = group.context_callsite_id;
        route.origin_static_ordinal = group.origin_static_ordinal;
        route.public_static_ordinal = first_public_ordinal + offset;
        route.operation_kind = kinds[offset];
        route.representation_operand_value_id = 0;
        candidate.push_back(route);
      }
    }
  }
  if (candidate.size() != groups.size() + representation_count ||
      representation_count != 1)
    return Report(diagnostic, "image public route coverage is incomplete", 0);
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT counts;
  counts.canonical_pu_count = canonical_pus;
  counts.canonical_source_count = sources.size();
  counts.static_descriptor_count = static_ordinal - 1;
  counts.dynamic_binding_count = candidate.size();
  counts.private_group_binding_count = groups.size();
  counts.representation_binding_count = representation_count;
  routes->swap(candidate);
  *result = counts;
  return true;
}

/* Build the immutable whole-program public selector plan. */
bool VHO_FHE_CKKS2C_Build_Public_Join(
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN *plan,
    VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT *result,
    FILE *diagnostic)
{
  if (groups.empty() || plan == NULL || result == NULL)
    return Report(diagnostic, "public join input is absent", 0);
  std::map<uint32_t, Owner_Info> owners;
  uint32_t canonical_pus = 0;
  std::vector<Canonical_Source> sources;
  if (!Collect_Owners(&owners, &canonical_pus, diagnostic) ||
      !Collect_Sources(owners, groups, &sources, diagnostic))
    return false;

  typedef std::pair<uint32_t, uint32_t> Owner_Value;
  std::map<Owner_Value, std::vector<const VHO_FHE_CKKS2C_GROUP *> > by_source;
  std::set<std::string> group_identities;
  for (size_t index = 0; index < groups.size(); ++index) {
    const VHO_FHE_CKKS2C_GROUP &group = groups[index];
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    DSL_CALLSITE_METADATA_RECORD callsite;
    const bool identity_valid =
        DSL_Call_Image_Get_PU_Identity(
            group.context_pu_identity_id, &identity) &&
        identity.id == group.context_pu_identity_id &&
        identity.owner_pu_st == group.owner_pu_st;
    const bool context_valid = group.context_callsite_id == 0 ?
        identity_valid :
        identity_valid && DSL_Call_Image_Get_Callsite(
            group.context_callsite_id, &callsite) &&
        callsite.id == group.context_callsite_id &&
        callsite.callee_pu_st == group.owner_pu_st;
    if (group.owner_pu_st == 0 || group.source_value_id == 0 ||
        group.source_node_id == 0 || group.source_value_name.empty() ||
        group.origin_owner_pu_st != group.owner_pu_st ||
        group.origin_source_value_id != group.source_value_id ||
        group.origin_static_ordinal == 0 || group.group_output_value_id == 0 ||
        group.event_ids.empty() ||
        group.event_ids.size() != group.plan.steps.size() ||
        group.result_node_ids.size() != group.plan.steps.size() ||
        group.result_value_ids.size() != group.plan.steps.size() ||
        group.result_value_ids.back() != group.group_output_value_id ||
        !context_valid ||
        group.identity_sha256.size() != 64 ||
        !group_identities.insert(group.identity_sha256).second)
      return Report(diagnostic, "private group provenance is incomplete",
                    index + 1);
    by_source[Owner_Value(group.owner_pu_st, group.source_value_id)]
        .push_back(&group);
  }

  VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN candidate;
  std::set<std::string> consumed;
  uint32_t static_ordinal = 1;
  uint32_t representation_count = 0;
  for (size_t source_index = 0; source_index < sources.size(); ++source_index) {
    const Canonical_Source &source = sources[source_index];
    std::vector<uint32_t> kinds;
    if (!Operation_Kinds(source.key.logical_operator, &kinds))
      return Report(diagnostic, "canonical operation kind is unsupported", 0);
    const uint32_t multiplicity = source.instances.size();
    std::vector<VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR> descriptors;
    for (size_t kind = 0; kind < kinds.size(); ++kind) {
      VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR descriptor;
      descriptor.canonical_owner_pu_st =
          source.representative.owner_pu_st;
      descriptor.canonical_source_node_id = source.representative.node_id;
      descriptor.canonical_source_value_id = source.representative.value_id;
      descriptor.static_ordinal = static_ordinal++;
      descriptor.operation_kind = kinds[kind];
      descriptor.execution_multiplicity = multiplicity;
      descriptors.push_back(descriptor);
      candidate.descriptors.push_back(descriptor);
    }
    for (size_t instance_index = 0;
         instance_index < source.instances.size(); ++instance_index) {
      const Source_Instance &instance = source.instances[instance_index];
      std::vector<const VHO_FHE_CKKS2C_GROUP *> matched =
          by_source[Owner_Value(instance.owner_pu_st, instance.value_id)];
      if (source.key.logical_operator == OPR_DSLFLATTEN) {
        if (!matched.empty() || source.instances.size() != 1 ||
            descriptors.size() != 1 ||
            descriptors[0].operation_kind !=
                VHO_FHE_RUNTIME_OP_LAYOUT_CONVERT)
          return Report(diagnostic,
                        "representation-only source owns a private group",
                        instance.value_id);
        Append_Representation_Binding(instance, descriptors[0], &candidate);
        ++representation_count;
        continue;
      }
      if (matched.size() != kinds.size())
        return Report(diagnostic,
                      "source does not own its exact private group count",
                      instance.value_id);
      std::sort(matched.begin(), matched.end(), Group_Origin_Less());
      for (size_t offset = 0; offset < matched.size(); ++offset) {
        const VHO_FHE_CKKS2C_GROUP &group = *matched[offset];
        if (group.source_node_id != instance.node_id ||
            group.source_logical_operator != instance.logical_operator ||
            group.source_operator_version != instance.operator_version ||
            group.source_value_name != instance.value_name ||
            (offset != 0 && group.origin_static_ordinal !=
                                matched[0]->origin_static_ordinal + offset) ||
            !consumed.insert(group.identity_sha256).second)
          return Report(diagnostic, "private group source identity changed",
                        instance.value_id);
        Append_Group_Binding(group, descriptors[offset], &candidate);
      }
    }
  }
  if (consumed.size() != groups.size() || representation_count != 1)
    return Report(diagnostic,
                  "private or representation event coverage is incomplete",
                  0);
  if (!Join_Identity(candidate, &candidate.identity_sha256))
    return Report(diagnostic, "public join identity cannot be authenticated",
                  0);

  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT counts;
  counts.canonical_pu_count = canonical_pus;
  counts.canonical_source_count = sources.size();
  counts.static_descriptor_count = candidate.descriptors.size();
  counts.dynamic_binding_count = candidate.bindings.size();
  counts.private_group_binding_count = consumed.size();
  counts.representation_binding_count = representation_count;
  plan->descriptors.swap(candidate.descriptors);
  plan->bindings.swap(candidate.bindings);
  plan->identity_sha256 = candidate.identity_sha256;
  *result = counts;
  return true;
}

/* Render one complete public join after re-authenticating its exact rows. */
bool VHO_FHE_CKKS2C_Render_Public_Join(
    const VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN &plan,
    const VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT &result,
    std::string *text, FILE *diagnostic)
{
  std::string identity;
  if (text == NULL || plan.descriptors.empty() || plan.bindings.empty() ||
      result.static_descriptor_count != plan.descriptors.size() ||
      result.dynamic_binding_count != plan.bindings.size() ||
      result.dynamic_binding_count != result.private_group_binding_count +
                                          result.representation_binding_count ||
      !Join_Identity(plan, &identity) || identity != plan.identity_sha256)
    return Report(diagnostic, "public join report is inconsistent", 0);

  std::ostringstream output;
  output << "public_schedule.format=open64.ckks2c.public-schedule.v1\n"
         << "public_schedule.join_sha256=" << identity << "\n"
         << "public_schedule.canonical_pu_count="
         << result.canonical_pu_count << "\n"
         << "public_schedule.canonical_source_count="
         << result.canonical_source_count << "\n"
         << "public_schedule.static_descriptor_count="
         << result.static_descriptor_count << "\n"
         << "public_schedule.dynamic_binding_count="
         << result.dynamic_binding_count << "\n"
         << "public_schedule.private_group_binding_count="
         << result.private_group_binding_count << "\n"
         << "public_schedule.representation_binding_count="
         << result.representation_binding_count << "\n";
  for (uint32_t index = 0; index < plan.descriptors.size(); ++index) {
    const VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR &row = plan.descriptors[index];
    output << "public_descriptor[" << index << "].owner_pu="
           << row.canonical_owner_pu_st << "\n"
           << "public_descriptor[" << index << "].source_node="
           << row.canonical_source_node_id << "\n"
           << "public_descriptor[" << index << "].source_value="
           << row.canonical_source_value_id << "\n"
           << "public_descriptor[" << index << "].static_ordinal="
           << row.static_ordinal << "\n"
           << "public_descriptor[" << index << "].operation_kind="
           << row.operation_kind << "\n"
           << "public_descriptor[" << index << "].multiplicity="
           << row.execution_multiplicity << "\n";
  }
  for (uint32_t index = 0; index < plan.bindings.size(); ++index) {
    const VHO_FHE_CKKS2C_PUBLIC_BINDING &row = plan.bindings[index];
    output << "public_binding[" << index << "].kind="
           << row.binding_kind << "\n"
           << "public_binding[" << index << "].static_ordinal="
           << row.static_ordinal << "\n"
           << "public_binding[" << index << "].operation_kind="
           << row.operation_kind << "\n"
           << "public_binding[" << index << "].owner_pu="
           << row.specialized_owner_pu_st << "\n"
           << "public_binding[" << index << "].source_value="
           << row.specialized_source_value_id << "\n"
           << "public_binding[" << index << "].context_identity="
           << row.context_pu_identity_id << "\n"
           << "public_binding[" << index << "].callsite="
           << row.context_callsite_id << "\n"
           << "public_binding[" << index << "].group_output="
           << row.group_output_value_id << "\n"
           << "public_binding[" << index << "].group_sha256="
           << (row.group_identity_sha256.empty() ? "none" :
               row.group_identity_sha256) << "\n";
  }
  *text = output.str();
  return true;
}
