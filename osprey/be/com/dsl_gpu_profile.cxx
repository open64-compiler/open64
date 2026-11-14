/* Copyright (C) 2026 Open64 Project */
/* Driver-owned H1b import and immutable context construction. Repository
 * RapidJSON handles syntax; the existing dependency-free byte hash handles
 * content identity. No GPU probing, native IR mutation, cost policy or new
 * shared dependency. Design: doc/AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md. */
#include "dsl_gpu_profile.h"
#include "fhe_sha256.h"
#include "rapidjson/document.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/writer.h"
#include <memory>
#include <limits>
#include <set>
#include <string.h>

namespace {
typedef rapidjson::Value JSON_VALUE;
static const char *const fact_members[] = { "lower", "value", "upper", "unit", "provenance", "evidence" };
static const char *const resource_members[] = { "id", "revision", "device", "architecture", "partition_kind", "partition_id", "inventory_id", "observed_at", "values" };
static const char *const execution_members[] = { "id", "revision", "device", "architecture", "resource_id", "resource_revision", "partition_id", "toolchain", "model", "observed_at", "conditions", "values" };
static const char *const condition_members[] = { "clock", "power", "ecc", "cache", "workload" };
static const char *const budget_members[] = { "id", "revision", "resource_id", "resource_revision", "partition_id", "limit_bytes", "reserved_bytes", "usable_bytes" };
static const char *const profile_members[] = { "schema", "device", "architecture", "device_revision", "contract", "toolchain", "resources", "execution", "budget" };
static const size_t profile_limit = 256 * 1024;
static const char *resource_names[DSL_GPU_RESOURCE_COUNT] = {
    "visible_sms", "warp_lanes", "physical_hbm_bytes", "visible_hbm_bytes",
    "l2_bytes", "registers_per_sm", "shared_bytes_per_sm",
    "shared_bytes_per_cta_default", "shared_bytes_per_cta_optin",
    "max_threads_per_sm", "max_threads_per_cta", "max_blocks_per_sm",
    "register_quantum", "shared_quantum"
};
static const char *resource_units[DSL_GPU_RESOURCE_COUNT] = {
    "count", "count", "bytes", "bytes", "bytes", "registers", "bytes",
    "bytes", "bytes", "count", "count", "count", "registers", "bytes"
};
static const char *execution_names[DSL_GPU_EXECUTION_COUNT] = {
    "hbm_read_rate", "hbm_write_rate", "hbm_mixed_rate", "l2_rate",
    "shared_rate_per_sm", "simt_rate_per_sm", "memory_startup_ns",
    "add_dependent_ns", "mul_dependent_ns", "exp_dependent_ns",
    "div_dependent_ns", "launch_ns", "async_startup_ns", "dequant_rate_per_sm"
};
static const char *execution_units[DSL_GPU_EXECUTION_COUNT] = {
    "bytes/second", "bytes/second", "bytes/second", "bytes/second",
    "bytes/second/SM", "operations/second/SM", "nanoseconds", "nanoseconds",
    "nanoseconds", "nanoseconds", "nanoseconds", "nanoseconds", "nanoseconds",
    "operations/second/SM"
};
static const char *budget_names[DSL_GPU_BUDGET_COUNT] = {
    "limit_bytes", "reserved_bytes", "usable_bytes"
};

/* Diagnose a structural/binding refusal without printing imported data. */
static bool refuse(FILE *diagnostics, const char *where)
{
    if (diagnostics) fprintf(diagnostics,
        "DSL_GPU_PROFILE-001: invalid or incompatible %s\n", where);
    return false;
}

/* Accept exact object members once each; unknown, duplicate or missing keys fail. */
static bool members(const JSON_VALUE &value,
    const std::set<std::string> &expected, FILE *diagnostics, const char *where)
{
    if (!value.IsObject() || value.MemberCount() != expected.size())
        return refuse(diagnostics, where);
    std::set<std::string> seen;
    for (JSON_VALUE::ConstMemberIterator it = value.MemberBegin();
         it != value.MemberEnd(); ++it) {
        const std::string name(it->name.GetString(), it->name.GetStringLength());
        if (!expected.count(name) || !seen.insert(name).second)
            return refuse(diagnostics, where);
    }
    return true;
}

/* Fixed-schema convenience delegates to the same exact member-set check. */
template<size_t count>
static bool object(const JSON_VALUE &value, const char *const (&keys)[count],
    FILE *diagnostics, const char *where)
{
    return members(value, std::set<std::string>(keys, keys + count), diagnostics, where);
}

/* Compare the full UTF-8 string, including its length, against an exact token. */
static bool equals(const JSON_VALUE &value, const std::string &expected)
{
    return value.IsString() && value.GetStringLength() == expected.size() &&
        memcmp(value.GetString(), expected.data(), expected.size()) == 0;
}

/* Copy a bounded printable identifier. Null is explicit unknown when allowed;
 * embedded NUL, whitespace and control bytes never truncate owned identity. */
static bool token(const JSON_VALUE &value, std::string *output, bool unknown,
    FILE *diagnostics, const char *where)
{
    if (unknown && value.IsNull()) { output->clear(); return true; }
    if (!value.IsString() || !value.GetStringLength() || value.GetStringLength() > 256)
        return refuse(diagnostics, where);
    for (unsigned i = 0; i < value.GetStringLength(); ++i) {
        const unsigned char c = value.GetString()[i];
        if (c == 0 || (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') &&
            !(c >= '0' && c <= '9') && !strchr("_-.:/+@", c)))
            return refuse(diagnostics, where);
    }
    output->assign(value.GetString(), value.GetStringLength());
    return true;
}

/* Validate the declared whole-second UTC spelling, not clock truth or freshness. */
static bool timestamp(const JSON_VALUE &value, std::string *output,
    FILE *diagnostics, const char *where)
{
    if (!token(value, output, true, diagnostics, where)) return false;
    if (output->empty()) return true;
    const std::string &s = *output;
    if (s.size() != 20 || s[4] != '-' || s[7] != '-' || s[10] != 'T' ||
        s[13] != ':' || s[16] != ':' || s[19] != 'Z') return refuse(diagnostics, where);
    for (unsigned i = 0; i < s.size(); ++i)
        if (i != 4 && i != 7 && i != 10 && i != 13 && i != 16 && i != 19 &&
            (s[i] < '0' || s[i] > '9')) return refuse(diagnostics, where);
    const unsigned month = unsigned(s[5]-'0')*10+s[6]-'0';
    const unsigned day = unsigned(s[8]-'0')*10+s[9]-'0';
    const unsigned hour = unsigned(s[11]-'0')*10+s[12]-'0';
    const unsigned minute = unsigned(s[14]-'0')*10+s[15]-'0';
    const unsigned second = unsigned(s[17]-'0')*10+s[18]-'0';
    return (month && month <= 12 && day && day <= 31 && hour < 24 &&
            minute < 60 && second <= 60) || refuse(diagnostics, where);
}

/* Revisions are positive exact uint32 generations, never floats or booleans. */
static bool revision(const JSON_VALUE &value, unsigned *output,
    FILE *diagnostics, const char *where)
{
    if (!value.IsUint() || !value.GetUint()) return refuse(diagnostics, where);
    *output = value.GetUint(); return true;
}

/* Classify declared evidence without treating it as hardware certification. */
static bool provenance(const JSON_VALUE &value, DSL_GPU_FACT_PROVENANCE *output)
{
    const char *names[] = { "unknown", "queried", "documented", "estimated",
                            "synthetic", "measured" };
    for (unsigned i = 0; i < sizeof(names)/sizeof(names[0]); ++i)
        if (equals(value, names[i])) { *output = DSL_GPU_FACT_PROVENANCE(i); return true; }
    return false;
}

/* Require fully explicit unknowns or finite, ordered, sourced integer bounds. */
static bool resource_fact(const JSON_VALUE &value, const char *unit, bool allow_zero,
    DSL_GPU_RESOURCE_FACT *fact, FILE *diagnostics, const char *where)
{
    if (!object(value, fact_members,
                diagnostics, where) || !equals(value["unit"], unit) ||
        !provenance(value["provenance"], &fact->provenance)) return refuse(diagnostics, where);
    if (fact->provenance == DSL_GPU_FACT_UNKNOWN) {
        if (!value["lower"].IsNull() || !value["value"].IsNull() ||
            !value["upper"].IsNull() || !equals(value["evidence"], ""))
            return refuse(diagnostics, where);
        return true;
    }
    if (fact->provenance == DSL_GPU_FACT_MEASURED ||
        !value["lower"].IsUint64() || !value["value"].IsUint64() ||
        !value["upper"].IsUint64() ||
        !token(value["evidence"], &fact->evidence, false, diagnostics, where))
        return refuse(diagnostics, where);
    fact->lower = value["lower"].GetUint64(); fact->value = value["value"].GetUint64();
    fact->upper = value["upper"].GetUint64();
    if ((!allow_zero && !fact->lower) || fact->lower > fact->value ||
        fact->value > fact->upper ||
        ((fact->provenance == DSL_GPU_FACT_QUERIED || fact->provenance == DSL_GPU_FACT_DOCUMENTED) &&
         fact->lower != fact->upper)) return refuse(diagnostics, where);
    fact->known = true; return true;
}

/* Reject NaN, infinities and negative physical quantities using C++98 limits. */
static bool finite_nonnegative(double value)
{
    return value >= 0 && value <= std::numeric_limits<double>::max();
}

/* Rates and dependent/startup nanoseconds are distinct typed physical quantities.
 * Zero latency may be known; every rate bound must be strictly positive. */
static bool execution_fact(const JSON_VALUE &value, const char *unit, bool rate,
    DSL_GPU_EXECUTION_FACT *fact, FILE *diagnostics, const char *where)
{
    if (!object(value, fact_members,
                diagnostics, where) || !equals(value["unit"], unit) ||
        !provenance(value["provenance"], &fact->provenance)) return refuse(diagnostics, where);
    if (fact->provenance == DSL_GPU_FACT_UNKNOWN) {
        if (!value["lower"].IsNull() || !value["value"].IsNull() ||
            !value["upper"].IsNull() || !equals(value["evidence"], ""))
            return refuse(diagnostics, where);
        return true;
    }
    if ((fact->provenance != DSL_GPU_FACT_MEASURED && fact->provenance != DSL_GPU_FACT_ESTIMATED &&
         fact->provenance != DSL_GPU_FACT_SYNTHETIC) ||
        !value["lower"].IsNumber() || !value["value"].IsNumber() || !value["upper"].IsNumber() ||
        !token(value["evidence"], &fact->evidence, false, diagnostics, where)) return refuse(diagnostics, where);
    fact->lower = value["lower"].GetDouble(); fact->value = value["value"].GetDouble();
    fact->upper = value["upper"].GetDouble();
    if (!finite_nonnegative(fact->lower) || !finite_nonnegative(fact->value) || !finite_nonnegative(fact->upper) ||
        fact->lower < 0 || (rate && fact->lower == 0) || fact->lower > fact->value ||
        fact->value > fact->upper) return refuse(diagnostics, where);
    fact->known = true; return true;
}

/* Use the already linked byte-hash utility; no new hash engine or crypto library. */
static std::string sha256(const std::string &bytes)
{
    return VHO_FHE_SHA256(reinterpret_cast<const unsigned char *>(bytes.data()), bytes.size());
}

/* Retain content order in section keys: reordering can invalidate conservatively,
 * while changing only execution data leaves the resource section key unchanged. */
static std::string serialized(const JSON_VALUE &value)
{
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    value.Accept(writer);
    return std::string(buffer.GetString(), buffer.GetSize());
}

/* Verify exact lowercase SHA spelling before any file is opened. */
static bool digest_valid(const char *digest)
{
    if (!digest || strlen(digest) != 64) return false;
    for (unsigned i = 0; i < 64; ++i)
        if (!(digest[i] >= '0' && digest[i] <= '9') &&
            !(digest[i] >= 'a' && digest[i] <= 'f')) return false;
    return true;
}

/* Read once with a hard cap, closing on every path. Digest pinning covers the
 * exact bytes read; later file changes cannot alter a sealed snapshot. */
static bool read_file(const char *path, std::string *bytes, FILE *diagnostics)
{
    FILE *file = fopen(path, "rb");
    if (!file) return refuse(diagnostics, "gpu_profile file open");
    char block[4096]; size_t count;
    while ((count = fread(block, 1, sizeof(block), file)) != 0) {
        if (bytes->size() > profile_limit || count > profile_limit - bytes->size()) {
            fclose(file); return refuse(diagnostics, "gpu_profile 256KiB size limit");
        }
        bytes->append(block, count);
    }
    const bool read = !ferror(file); const bool closed = fclose(file) == 0;
    return (read && closed && !bytes->empty() && bytes->find('\0') == std::string::npos) ||
        refuse(diagnostics, "gpu_profile read/empty/NUL data");
}
}

/* Only this backend service can populate private context fields. The object
 * stays unpublished until syntax, bindings, facts, budgets and keys all pass. */
class DSL_GPU_PROFILE_IMPORTER {
    /* Verify a repeated resource generation/partition join; no implicit rebinding. */
    static bool Resource_binding(const JSON_VALUE &value,
        const DSL_GPU_DEVICE_CONTEXT &context, FILE *diagnostics)
    {
        return (equals(value["resource_id"], context._resource_id) &&
                value["resource_revision"].IsUint() &&
                value["resource_revision"].GetUint() == context._resource_revision &&
                equals(value["partition_id"], context._partition_id)) ||
            refuse(diagnostics, "resource revision/partition binding");
    }

    /* Copy only exact resource fields, checking structural limits conservatively. */
    static bool Read_resources(const JSON_VALUE &value,
        DSL_GPU_DEVICE_CONTEXT *context, FILE *diagnostics)
    {
        if (!object(value, resource_members,
                    diagnostics, "resources schema") ||
            !equals(value["device"], context->_device) ||
            !equals(value["architecture"], context->_architecture) ||
            !token(value["id"], &context->_resource_id, false, diagnostics, "resource id") ||
            !revision(value["revision"], &context->_resource_revision, diagnostics, "resource revision") ||
            !token(value["partition_kind"], &context->_partition_kind, false, diagnostics, "partition kind") ||
            (context->_partition_kind != "full" && context->_partition_kind != "mig") ||
            !token(value["partition_id"], &context->_partition_id, false, diagnostics, "partition id") ||
            !token(value["inventory_id"], &context->_inventory_id, false, diagnostics, "inventory id") ||
            !timestamp(value["observed_at"], &context->_resource_observed_at, diagnostics, "resource observation"))
            return refuse(diagnostics, "resource identity");
        std::set<std::string> fields(resource_names, resource_names + DSL_GPU_RESOURCE_COUNT);
        if (!members(value["values"], fields, diagnostics, "resource fields")) return false;
        for (unsigned i = 0; i < DSL_GPU_RESOURCE_COUNT; ++i) {
            DSL_GPU_RESOURCE_FACT &fact = context->_resources[i];
            if (!resource_fact(value["values"][resource_names[i]], resource_units[i], false,
                               &fact, diagnostics, resource_names[i])) return false;
            if (fact.known) ++context->_resource_known;
            if (fact.provenance == DSL_GPU_FACT_QUERIED && context->_resource_observed_at.empty())
                return refuse(diagnostics, "queried resource observation");
        }
        const unsigned pairs[][2] = {
            {DSL_GPU_VISIBLE_HBM_BYTES, DSL_GPU_PHYSICAL_HBM_BYTES},
            {DSL_GPU_SHARED_BYTES_PER_CTA_DEFAULT, DSL_GPU_SHARED_BYTES_PER_CTA_OPTIN},
            {DSL_GPU_SHARED_BYTES_PER_CTA_OPTIN, DSL_GPU_SHARED_BYTES_PER_SM},
            {DSL_GPU_MAX_THREADS_PER_CTA, DSL_GPU_MAX_THREADS_PER_SM}
        };
        for (unsigned i = 0; i < sizeof(pairs)/sizeof(pairs[0]); ++i) {
            const DSL_GPU_RESOURCE_FACT &a = context->_resources[pairs[i][0]];
            const DSL_GPU_RESOURCE_FACT &b = context->_resources[pairs[i][1]];
            if (a.known && b.known && a.upper > b.lower)
                return refuse(diagnostics, "resource limit bounds");
        }
        return true;
    }

    /* Validate calibration applicability and preserve measurements/estimates and
     * conditions separately. Missing execution is explicitly unbound. */
    static bool Read_execution(const JSON_VALUE &value,
        DSL_GPU_DEVICE_CONTEXT *context, FILE *diagnostics)
    {
        if (value.IsNull()) return true;
        if (!object(value, execution_members, diagnostics, "execution schema") ||
            !equals(value["device"], context->_device) ||
            !equals(value["architecture"], context->_architecture) ||
            !equals(value["toolchain"], context->_toolchain) ||
            !Resource_binding(value, *context, diagnostics) ||
            !token(value["id"], &context->_execution_id, false, diagnostics, "execution id") ||
            !revision(value["revision"], &context->_execution_revision, diagnostics, "execution revision") ||
            !token(value["model"], &context->_model, false, diagnostics, "execution model") ||
            !timestamp(value["observed_at"], &context->_execution_observed_at, diagnostics, "execution observation") ||
            !object(value["conditions"], condition_members, diagnostics, "execution conditions"))
            return refuse(diagnostics, "execution identity");
        bool complete_conditions = !context->_execution_observed_at.empty();
        for (unsigned i = 0; i < 5; ++i) {
            if (!token(value["conditions"][condition_members[i]], &context->_conditions[i], true,
                       diagnostics, "execution condition")) return false;
            complete_conditions = complete_conditions && !context->_conditions[i].empty();
        }
        std::set<std::string> fields(execution_names, execution_names + DSL_GPU_EXECUTION_COUNT);
        if (!members(value["values"], fields, diagnostics, "execution fields")) return false;
        for (unsigned i = 0; i < DSL_GPU_EXECUTION_COUNT; ++i) {
            DSL_GPU_EXECUTION_FACT &fact = context->_execution[i];
            if (!execution_fact(value["values"][execution_names[i]], execution_units[i],
                i < 6 || i == DSL_GPU_DEQUANT_RATE_PER_SM, &fact, diagnostics, execution_names[i])) return false;
            if (fact.known) ++context->_execution_known;
            if (fact.provenance == DSL_GPU_FACT_MEASURED && !complete_conditions)
                return refuse(diagnostics, "measured execution conditions/observation");
        }
        context->_execution_bound = true;
        return true;
    }

    /* Copy an independent workload budget and prove only fully known, bounded
     * relationships. Unknown capacity/budget does not grant an allocation. */
    static bool Read_budget(const JSON_VALUE &value,
        DSL_GPU_DEVICE_CONTEXT *context, FILE *diagnostics)
    {
        if (!object(value, budget_members, diagnostics, "budget schema") ||
            !Resource_binding(value, *context, diagnostics) ||
            !token(value["id"], &context->_budget_id, false, diagnostics, "budget id") ||
            !revision(value["revision"], &context->_budget_revision, diagnostics, "budget revision")) return false;
        bool known = context->_resources[DSL_GPU_VISIBLE_HBM_BYTES].known;
        for (unsigned i = 0; i < DSL_GPU_BUDGET_COUNT; ++i) {
            DSL_GPU_RESOURCE_FACT &fact = context->_budget[i];
            if (!resource_fact(value[budget_names[i]], "bytes", true, &fact, diagnostics, budget_names[i])) return false;
            known = known && fact.known;
            const DSL_GPU_RESOURCE_FACT &capacity = context->_resources[DSL_GPU_VISIBLE_HBM_BYTES];
            if (fact.known && capacity.known && fact.upper > capacity.lower)
                return refuse(diagnostics, "budget exceeds visible capacity");
        }
        const DSL_GPU_RESOURCE_FACT &limit = context->_budget[DSL_GPU_BUDGET_LIMIT];
        const DSL_GPU_RESOURCE_FACT &reserved = context->_budget[DSL_GPU_BUDGET_RESERVED];
        const DSL_GPU_RESOURCE_FACT &usable = context->_budget[DSL_GPU_BUDGET_USABLE];
        if (limit.known && ((reserved.known && reserved.upper > limit.lower) ||
                            (usable.known && usable.upper > limit.lower))) return refuse(diagnostics, "budget limit");
        if (reserved.known && usable.known) {
            if (reserved.upper > std::numeric_limits<uint64_t>::max() - usable.upper)
                return refuse(diagnostics, "budget addition overflow");
            if (limit.known && reserved.upper + usable.upper > limit.lower)
                return refuse(diagnostics, "reserved plus usable budget");
        }
        context->_budget_bounds_proved = known;
        return true;
    }
public:
    /* Resolve into private owned storage, validate every binding before sealing,
     * and atomically replace output only on success. No global/native mutation. */
    static BOOL Resolve(const DSL_HARDWARE_TARGET_SELECTION &target,
        const char *path, const char *expected_sha256, const char *expected_toolchain,
        DSL_GPU_DEVICE_CONTEXT_OWNER *output, FILE *diagnostics)
    {
        if (!output) return FALSE;
        DSL_HARDWARE_TARGET_SELECTION checked;
        if (!target.architecture || !target.device.name || !target.device.device_id || target.revision != 1 ||
            !DSL_hardware_target_resolve(target.architecture, target.device.name, &checked, NULL) ||
            checked.arch_id != target.arch_id || checked.device.device_id != target.device.device_id ||
            checked.device.revision != target.device.revision)
            return refuse(diagnostics, "H1a device/ISA identity");
        std::auto_ptr<DSL_GPU_DEVICE_CONTEXT> pending(new DSL_GPU_DEVICE_CONTEXT());
        pending->_device = checked.device.name; pending->_architecture = checked.architecture;
        pending->_device_id = checked.device.device_id; pending->_device_revision = checked.device.revision;
        const std::string identity = pending->_device + "\n" + pending->_architecture + "\n1";
        if (!path) {
            if (expected_sha256 || expected_toolchain) return refuse(diagnostics, "profile companion without file");
            pending->_input_sha256 = "none";
            pending->_resource_sha256 = sha256(identity + "\nunknown-resources");
            pending->_execution_sha256 = "none"; pending->_budget_sha256 = "none";
            pending->_context_sha256 = sha256(identity + "\nidentity-only");
        } else {
            if (!*path || !digest_valid(expected_sha256) || !expected_toolchain || !*expected_toolchain)
                return refuse(diagnostics, "profile path/digest/toolchain companions");
            std::string bytes;
            if (!read_file(path, &bytes, diagnostics)) return FALSE;
            if (sha256(bytes) != expected_sha256) return refuse(diagnostics, "input SHA-256 pin");
            rapidjson::Document document;
            document.Parse<rapidjson::kParseIterativeFlag | rapidjson::kParseValidateEncodingFlag |
                           rapidjson::kParseFullPrecisionFlag>(bytes.data(), bytes.size());
            if (document.HasParseError() || !object(document,
                profile_members, diagnostics, "profile schema"))
                return refuse(diagnostics, "profile JSON syntax/schema");
            if (!equals(document["schema"], "open64.gpu-device-profile.v1") ||
                !equals(document["contract"], "open64.gpu-profile.sm90.v1") ||
                !equals(document["device"], pending->_device) ||
                !equals(document["architecture"], pending->_architecture) ||
                !document["device_revision"].IsUint() || document["device_revision"].GetUint() != pending->_device_revision ||
                !equals(document["toolchain"], expected_toolchain) ||
                !token(document["toolchain"], &pending->_toolchain, false, diagnostics, "target toolchain"))
                return refuse(diagnostics, "profile device/ISA/revision/contract/toolchain binding");
            if (!Read_resources(document["resources"], pending.get(), diagnostics) ||
                !Read_execution(document["execution"], pending.get(), diagnostics) ||
                !Read_budget(document["budget"], pending.get(), diagnostics)) return FALSE;
            // Keys expose separate dependency boundaries; input pin binds the complete snapshot.
            pending->_input_sha256 = expected_sha256;
            pending->_resource_sha256 = sha256(identity + "\n" + pending->_toolchain + "\n" + serialized(document["resources"]));
            pending->_execution_sha256 = pending->_execution_bound ?
                sha256(pending->_resource_sha256 + "\n" + serialized(document["execution"])) : "none";
            pending->_budget_sha256 = sha256(pending->_resource_sha256 + "\n" + serialized(document["budget"]));
            pending->_context_sha256 = sha256(pending->_input_sha256 + "\n" + pending->_resource_sha256 + "\n" +
                pending->_execution_sha256 + "\n" + pending->_budget_sha256);
            pending->_imported = true;
        }
        output->Publish(pending.release());
        return TRUE;
    }
};

/* Public driver service delegates to the private builder; callers retain the
 * old context and all native/output state if any import check refuses. */
BOOL DSL_gpu_profile_resolve(const DSL_HARDWARE_TARGET_SELECTION &target,
    const char *path, const char *expected_sha256, const char *expected_toolchain,
    DSL_GPU_DEVICE_CONTEXT_OWNER *output, FILE *diagnostics)
{
    return DSL_GPU_PROFILE_IMPORTER::Resolve(target, path, expected_sha256,
                                            expected_toolchain, output, diagnostics);
}
