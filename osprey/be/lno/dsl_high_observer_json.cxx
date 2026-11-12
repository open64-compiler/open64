/* Copyright (C) 2026 Open64 Project */
/* Bounded read-only JSON input adapter for LNO-owned detached search records.
 * Uses repository RapidJSON headers; adds no library dependency to be.so.
 * Design: doc/AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md. */
#include "dsl_high_observer.h"
#include "../../rapidjson/document.h"
#include <fstream>
#include <set>
#include <stdexcept>

namespace {
/* Propagate stable input refusals; no native state exists in this adapter. */
void DSL_observer_require(bool valid) {
    if (!valid) throw std::runtime_error("binding_document_invalid");
}
/* Reject duplicates recursively before member lookup; bound nesting and nodes
 * even for unknown subtrees. RapidJSON validates UTF-8 during parsing. */
void DSL_observer_unique(const rapidjson::Value &value, unsigned depth, unsigned &nodes) {
    DSL_observer_require(depth <= 48 && ++nodes <= 200000);
    if (value.IsObject()) {
        std::set<std::string> names;
        for (rapidjson::Value::ConstMemberIterator i = value.MemberBegin(); i != value.MemberEnd(); ++i) {
            std::string name(i->name.GetString(), i->name.GetStringLength());
            DSL_observer_require(names.insert(name).second);
            DSL_observer_unique(i->value, depth + 1, nodes);
        }
    } else if (value.IsArray()) for (rapidjson::SizeType i = 0; i < value.Size(); ++i)
        DSL_observer_unique(value[i], depth + 1, nodes);
}
/* Exact object field sets prevent ignored policy or version extensions. */
void DSL_observer_object(const rapidjson::Value &value, unsigned fields) {
    DSL_observer_require(value.IsObject() && value.MemberCount() == fields);
}
/* Lookup without operator[] assertions on a missing untrusted field. */
const rapidjson::Value &DSL_observer_member(const rapidjson::Value &value, const char *name) {
    DSL_observer_require(value.IsObject() && value.HasMember(name)); return value[name];
}
/* Strings are bounded and NUL-free before any native metadata comparison. */
std::string DSL_observer_string(const rapidjson::Value &value, unsigned limit = 4096) {
    DSL_observer_require(value.IsString() && value.GetStringLength() > 0 && value.GetStringLength() <= limit);
    std::string text(value.GetString(), value.GetStringLength());
    DSL_observer_require(text.find('\0') == std::string::npos); return text;
}
/* Numeric fields never coerce strings, negative values, fractions or booleans. */
uint64_t DSL_observer_uint(const rapidjson::Value &value, uint64_t limit = UINT64_C(0xffffffff)) {
    DSL_observer_require(value.IsUint64() && value.GetUint64() <= limit); return value.GetUint64();
}
/* Decode the semantic source schema, excluding derived launch/kernel fields.
 * The producer's complete typed recipe key is the authoritative native join. */
VHO_TENSOR_PROGRAM DSL_observer_algorithm(const rapidjson::Value &value) {
    DSL_observer_object(value, 7 + (value.IsObject() && value.HasMember("tensor_cache_traits")));
    DSL_observer_require(DSL_observer_string(DSL_observer_member(value,"schema")) == "open64.high-algorithm.v1");
    VHO_TENSOR_PROGRAM program;
    program.source_pu = DSL_observer_string(DSL_observer_member(value,"source_pu"));
    program.source_file = DSL_observer_string(DSL_observer_member(value,"source_file"));
    program.opt_level = DSL_observer_uint(DSL_observer_member(value,"opt_level"));
    program.threads = DSL_observer_uint(DSL_observer_member(value,"threads"));
    const rapidjson::Value &values = DSL_observer_member(value,"values"), &operations = DSL_observer_member(value,"operations");
    DSL_observer_require(values.IsArray() && values.Size() > 0 && values.Size() <= 4096 &&
        operations.IsArray() && operations.Size() > 0 && operations.Size() <= 4096);
    for (unsigned i = 0; i < values.Size(); ++i) {
        const rapidjson::Value &entry = values[i]; DSL_observer_object(entry,6);
        VHO_TENSOR_VALUE tensor;
        tensor.name = DSL_observer_string(DSL_observer_member(entry,"name"));
        tensor.dtype = DSL_observer_string(DSL_observer_member(entry,"dtype"));
        tensor.role = DSL_observer_uint(DSL_observer_member(entry,"role"));
        tensor.ordinal = DSL_observer_uint(DSL_observer_member(entry,"ordinal"));
        tensor.storage = DSL_observer_uint(DSL_observer_member(entry,"storage"));
        const rapidjson::Value &shape = DSL_observer_member(entry,"shape");
        DSL_observer_require(shape.IsArray() && shape.Size() > 0 && shape.Size() <= 16);
        for (unsigned j = 0; j < shape.Size(); ++j) tensor.shape.push_back(DSL_observer_uint(shape[j],16777216));
        program.values.push_back(tensor);
    }
    if(value.HasMember("tensor_cache_traits")) {
        const rapidjson::Value &traits=DSL_observer_member(value,"tensor_cache_traits");
        DSL_observer_object(traits,2);
        DSL_observer_require(DSL_observer_string(DSL_observer_member(traits,"schema"))=="open64.tensor-cache-traits.v1");
        const rapidjson::Value &records=DSL_observer_member(traits,"values");
        DSL_observer_require(records.IsArray() && records.Size()>0 && records.Size()<=values.Size());
        for(unsigned i=0;i<records.Size();++i) {
            const rapidjson::Value &record=records[i];DSL_observer_object(record,2);
            unsigned id=DSL_observer_uint(DSL_observer_member(record,"value"));
            std::string trait=DSL_observer_string(DSL_observer_member(record,"trait"));
            DSL_observer_require(id<program.values.size() && program.values[id].cache_trait.empty() &&
                DSL_tensor_cache_trait_known(trait.c_str()));
            program.values[id].cache_trait=trait;
        }
    }
    for (unsigned i = 0; i < operations.Size(); ++i) {
        const rapidjson::Value &entry = operations[i]; DSL_observer_object(entry,7);
        VHO_TENSOR_OPERATION operation;
        operation.name = DSL_observer_string(DSL_observer_member(entry,"name"));
        operation.version = DSL_observer_uint(DSL_observer_member(entry,"version"));
        operation.source_node = DSL_observer_uint(DSL_observer_member(entry,"source_node"));
        operation.source_line = DSL_observer_uint(DSL_observer_member(entry,"source_line"));
        operation.output = DSL_observer_uint(DSL_observer_member(entry,"output"));
        const rapidjson::Value &inputs = DSL_observer_member(entry,"inputs"), &attributes = DSL_observer_member(entry,"attributes");
        DSL_observer_require(inputs.IsArray() && inputs.Size() > 0 && inputs.Size() <= 3 &&
            attributes.IsObject() && attributes.MemberCount() <= 16);
        for (unsigned j = 0; j < inputs.Size(); ++j) operation.inputs.push_back(DSL_observer_uint(inputs[j]));
        for (rapidjson::Value::ConstMemberIterator j = attributes.MemberBegin(); j != attributes.MemberEnd(); ++j)
            operation.attributes[DSL_observer_string(j->name)] = DSL_observer_string(j->value);
        program.operations.push_back(operation);
    }
    return program;
}
/* Reuse existing resource schema and validators, with explicit provenance. */
VHO_TENSOR_GPU_PROFILE DSL_observer_gpu(const rapidjson::Value &value) {
    DSL_observer_object(value,9);
    DSL_observer_require(DSL_observer_string(DSL_observer_member(value,"schema")) == "open64.mla-gpu-profile.v1" &&
        DSL_observer_string(DSL_observer_member(value,"provenance")) == "provided-capacities-estimated-registers");
    VHO_TENSOR_GPU_PROFILE gpu;
    gpu.id = DSL_observer_string(DSL_observer_member(value,"id"));
    gpu.architecture = DSL_observer_string(DSL_observer_member(value,"architecture"));
    gpu.sms = DSL_observer_uint(DSL_observer_member(value,"sms"));
    gpu.registers_per_thread = DSL_observer_uint(DSL_observer_member(value,"registers_per_thread_estimate"));
    gpu.target_ctas_per_sm = DSL_observer_uint(DSL_observer_member(value,"target_ctas_per_sm"));
    gpu.scratch_budget = DSL_observer_uint(DSL_observer_member(value,"scratch_budget_bytes"),UINT64_C(1)<<32);
    const rapidjson::Value &resources = DSL_observer_member(value,"resources"); DSL_observer_object(resources,12);
    const char *names[] = {"max_blocks","max_warps","max_threads","max_threads_per_cta","registers_per_sm",
        "max_registers_per_thread","register_quantum","register_warp_quantum","shared_capacity","shared_budget","shared_quantum","shared_reserved"};
    DSL_HARDWARE_RESOURCE_RECORD &record = gpu.resources;
    uint64_t *fields[] = {&record.max_blocks,&record.max_warps,&record.max_threads,&record.max_threads_per_cta,&record.registers_per_sm,
        &record.max_registers_per_thread,&record.register_quantum,&record.register_warp_quantum,&record.shared_capacity,
        &record.shared_budget,&record.shared_quantum,&record.shared_reserved};
    for (unsigned i = 0; i < 12; ++i) *fields[i] = DSL_observer_uint(DSL_observer_member(resources,names[i]),UINT64_C(1)<<32);
    return gpu;
}
/* The exact model version/unit and every positive timing are explicit. */
VHO_MLA_COST_PROFILE DSL_observer_model(const rapidjson::Value &value) {
    DSL_observer_object(value,6);
    DSL_observer_require(DSL_observer_string(DSL_observer_member(value,"schema")) == "open64.mla-cost-profile.v1" &&
        DSL_observer_string(DSL_observer_member(value,"unit")) == "reference_cycles");
    VHO_MLA_COST_PROFILE model;
    model.id = DSL_observer_string(DSL_observer_member(value,"id"));
    model.architecture = DSL_observer_string(DSL_observer_member(value,"architecture"));
    model.provenance = DSL_observer_string(DSL_observer_member(value,"provenance"));
    const rapidjson::Value &parameters = DSL_observer_member(value,"parameters"); DSL_observer_object(parameters,VHO_MLA_COST_FIELDS);
    for (unsigned i = 0; i < VHO_MLA_COST_FIELDS; ++i)
        model.values[i] = DSL_observer_uint(DSL_observer_member(parameters,VHO_mla_cost_names[i]),1000000000);
    return model;
}
/* Hierarchy assumptions are an explicit v3-only extension. Static policies:
 * conservative L1 exclusion, write-through stores, no deliberate spills or
 * overlap, and an immutable materializer capability registry. */
VHO_MLA_MOVEMENT_PROFILE DSL_observer_hierarchy(const rapidjson::Value &v) {
    DSL_observer_object(v,13);
    DSL_observer_require(DSL_observer_string(DSL_observer_member(v,"schema"))=="open64.mla-hierarchy-profile.v1" &&
        DSL_observer_string(DSL_observer_member(v,"unit"))=="reference_cycles" &&
        DSL_observer_string(DSL_observer_member(v,"capabilities"))==VHO_mla_movement_capabilities());
    VHO_MLA_MOVEMENT_PROFILE p;
    p.id=DSL_observer_string(DSL_observer_member(v,"id"),96);
    p.architecture=DSL_observer_string(DSL_observer_member(v,"architecture"),96);
    p.provenance=DSL_observer_string(DSL_observer_member(v,"provenance"),96);
    p.objective=DSL_observer_string(DSL_observer_member(v,"objective"),96);
    p.hbm_rate=DSL_observer_uint(DSL_observer_member(v,"hbm_bytes_per_cycle"),1000000000);
    p.l2_rate=DSL_observer_uint(DSL_observer_member(v,"l2_bytes_per_cycle"),1000000000);
    p.sector_bytes=DSL_observer_uint(DSL_observer_member(v,"sector_bytes"),128);
    p.request_amplification_ppm=DSL_observer_uint(DSL_observer_member(v,"request_amplification_ppm"),8000000);
    p.shared_conflict_ppm=DSL_observer_uint(DSL_observer_member(v,"shared_conflict_ppm"),8000000);
    const rapidjson::Value &rows=DSL_observer_member(v,"scenarios");
    DSL_observer_require(rows.IsArray() && rows.Size()>0 && rows.Size()<=4);
    for(unsigned i=0;i<rows.Size();++i) {
        const rapidjson::Value &row=rows[i];DSL_observer_object(row,4);
        VHO_MLA_CACHE_SCENARIO c;c.id=DSL_observer_string(DSL_observer_member(row,"id"),96);
        c.weight=DSL_observer_uint(DSL_observer_member(row,"weight"),1000000);
        c.cache_hit_ppm=DSL_observer_uint(DSL_observer_member(row,"cache_l2_hit_ppm"),1000000);
        c.other_hit_ppm=DSL_observer_uint(DSL_observer_member(row,"other_l2_hit_ppm"),1000000);
        p.scenarios.push_back(c);
    }
    DSL_observer_require(VHO_mla_movement_profile_valid(p,NULL));return p;
}
/* Decode the versioned global/per-run/composite controls without borrowing
 * JSON state. Hard safety caps remain mandatory even if budget is not named. */
VHO_MLA_SEARCH_CONTROL DSL_observer_controls(const rapidjson::Value &value,bool movement) {
    DSL_observer_object(value,15);
    DSL_observer_require(DSL_observer_string(DSL_observer_member(value,"schema")) == (movement?"open64.mla-search-controls.v3":"open64.mla-search-controls.v2"));
    VHO_MLA_SEARCH_CONTROL control;
    control.space=DSL_observer_string(DSL_observer_member(value,"space"));
    control.mode=DSL_observer_string(DSL_observer_member(value,"mode"));
    control.global_proposals=DSL_observer_uint(DSL_observer_member(value,"global_proposal_limit"),movement?576:192);
    control.global_unique=DSL_observer_uint(DSL_observer_member(value,"global_unique_limit"),movement?36:12);
    control.global_time_us=DSL_observer_uint(DSL_observer_member(value,"global_time_limit_us"),UINT64_C(3600000000));
    control.patience=DSL_observer_uint(DSL_observer_member(value,"patience"),movement?36:12);
    control.minimum_gain=DSL_observer_uint(DSL_observer_member(value,"minimum_gain_cycles"),1000000000);
    control.target=DSL_observer_uint(DSL_observer_member(value,"target_cycles"),UINT64_C(1000000000000000000));
    control.gap_ppm=DSL_observer_uint(DSL_observer_member(value,"gap_ppm"),1000000);
    control.remaining_invocations=DSL_observer_uint(DSL_observer_member(value,"remaining_invocations"),UINT64_C(1000000000000));
    control.evaluation_cost=DSL_observer_uint(DSL_observer_member(value,"evaluation_cost_cycles"),UINT64_C(1000000000000000000));
    const rapidjson::Value &runs=DSL_observer_member(value,"runs"),&stopping=DSL_observer_member(value,"stopping");
    DSL_observer_require(runs.IsArray() && runs.Size()>0 && runs.Size()<=16 && stopping.IsArray() && stopping.Size()>0 && stopping.Size()<=6);
    control.runs.clear();control.stopping.clear();
    for(unsigned i=0;i<runs.Size();++i) {
        const rapidjson::Value &entry=runs[i];DSL_observer_object(entry,4);
        VHO_MLA_SEARCH_RUN_CONTROL run;
        run.id=DSL_observer_string(DSL_observer_member(entry,"id"),96);
        run.strategy=DSL_observer_string(DSL_observer_member(entry,"strategy"),96);
        run.seed=DSL_observer_uint(DSL_observer_member(entry,"seed"));
        run.proposal_limit=DSL_observer_uint(DSL_observer_member(entry,"proposal_limit"),movement?36:12);control.runs.push_back(run);
    }
    for(unsigned i=0;i<stopping.Size();++i) control.stopping.push_back(DSL_observer_string(stopping[i],96));
    const rapidjson::Value &conversion=DSL_observer_member(value,"economic_conversion");
    if(!conversion.IsNull()) {
        DSL_observer_object(conversion,4);
        DSL_observer_require(DSL_observer_string(DSL_observer_member(conversion,"schema"))=="open64.search-cost-conversion.v1");
        control.economic_conversion=true;
        control.host_us=DSL_observer_uint(DSL_observer_member(conversion,"host_microseconds"),UINT64_C(1000000000000));
        control.cycles_per_us=DSL_observer_uint(DSL_observer_member(conversion,"reference_cycles_per_microsecond"),1000000000);
        control.conversion_provenance=DSL_observer_string(DSL_observer_member(conversion,"provenance"),96);
    }
    return control;
}
}

/* Parse once into private owned data. Every failure leaves an empty output and
 * an ordinary-LNO skip reason; no candidate receives document or native handles. */
bool LNO_DSL_read_search_input(const char *path, DSL_HIGH_SEARCH_INPUT &output, std::string &reason) {
    output = DSL_HIGH_SEARCH_INPUT();
    try {
        std::ifstream input(path,std::ios::binary);
        if (!input.good()) return reason = "binding_document_unavailable", false;
        input.seekg(0,std::ios::end); std::streamoff size = input.tellg();
        DSL_observer_require(size > 0 && size <= 16*1024*1024); input.seekg(0);
        std::string text((size_t)size,'\0'); input.read(&text[0],size); DSL_observer_require(input.good());
        DSL_observer_require(text.find('\0') == std::string::npos);
        rapidjson::Document document;
        document.Parse<rapidjson::kParseValidateEncodingFlag | rapidjson::kParseIterativeFlag>(text.c_str());
        DSL_observer_require(!document.HasParseError());
        unsigned nodes = 0; DSL_observer_unique(document,0,nodes);
        std::string schema=DSL_observer_string(DSL_observer_member(document,"schema"));
        bool movement=schema=="open64.lno-high-search.v3";
        bool production=movement || schema=="open64.lno-high-search.v2";
        DSL_observer_require(production || schema=="open64.lno-high-search.v1");
        DSL_observer_object(document,movement?14:production?13:12);
        DSL_HIGH_SEARCH_INPUT result;
        result.production_controls=production;result.movement_enabled=movement;
        if(movement) result.movement_profile=DSL_observer_hierarchy(DSL_observer_member(document,"hierarchy"));
        if(production) result.controls=DSL_observer_controls(DSL_observer_member(document,"search_controls"),movement);
        result.unit = DSL_observer_string(DSL_observer_member(document,"unit"));
        result.function = DSL_observer_string(DSL_observer_member(document,"function"));
        result.source_key = DSL_observer_string(DSL_observer_member(document,"source_key"),8*1024*1024);
        result.facts_key = DSL_observer_string(DSL_observer_member(document,"facts_key"),8*1024*1024);
        result.numeric = DSL_observer_string(DSL_observer_member(document,"numeric"));
        result.host_target = DSL_observer_string(DSL_observer_member(document,"host_target"));
        const rapidjson::Value &permission = DSL_observer_member(document,"allow_reassociate");
        DSL_observer_require(permission.IsBool()); result.allow_reassociate = permission.GetBool();
        result.algorithm = DSL_observer_algorithm(DSL_observer_member(document,"algorithm"));
        result.gpu = DSL_observer_gpu(DSL_observer_member(document,"gpu"));
        result.model = DSL_observer_model(DSL_observer_member(document,"model"));
        const rapidjson::Value &buffers = DSL_observer_member(document,"buffers");
        DSL_observer_require(buffers.IsArray() && buffers.Size() > 0 && buffers.Size() <= 4096);
        for (unsigned i = 0; i < buffers.Size(); ++i) result.buffers.push_back(DSL_observer_uint(buffers[i]));
        output = result; reason = "decoded"; return true;
    } catch (const std::exception &error) { reason = error.what(); return false; }
}
