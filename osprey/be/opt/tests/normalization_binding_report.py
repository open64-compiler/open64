"""LN1-e joins native source/target meaning to exact full-tensor witnesses.

Design: doc/AI-COMPILER-LAYERNORM-INTEGRATED-BINDING-v0.1.md.
Evidence-only: no reduction import, v1 promotion, selected plan or kernel.
"""
import hashlib
import json
from pathlib import Path
import random

import normalization_oracle as fp
from normalization_ref_trace_report import audit as audit_source
from normalization_trace_report import audit as audit_v1
from normalization_target_trace_report import audit as audit_target, ARCHITECTURES
from run_normalization_oracle_review import audit as audit_numbers, expected_states, source_lines, requests

REGIMES = ("finite", "cancellation", "subnormal", "exceptional")


def require(condition, reason):
    """Keep review gates active under optimized Python as well."""
    if not condition:
        raise ValueError(reason)


def sha256(path):
    """Hash actual retained bytes, not an asserted digest or filename."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def key(fact):
    """Native IDs are scoped to this image and PU, never globally unique."""
    return tuple(fact[name] for name in ("pu", "node", "result"))


def join_contracts(source, target, architecture):
    """Require exact source joins and one baseline/stale/unsafe query per v2.

    Every v1 must have its own source-unbound refusal, not an invented mean or
    numerical sample. The target service checked all native snapshot fields;
    its trace is audited against real PREOPT identities, not a metadata label.
    """
    require(architecture in ARCHITECTURES, "unsupported architecture")
    facts = audit_source(source)
    legacy = audit_v1(source) if "AI_NORM 1 FORM " in source else []
    queries = audit_target(target, source)
    expected = {key(fact): ["0", "2", "6"] for fact in facts}
    expected.update({key(fact): ["1"] for fact in legacy})
    require(len(expected) == len(facts) + len(legacy), "duplicate source identity")
    seen = {identity: [] for identity in expected}
    for query in queries:
        identity = key(query)
        require(identity in seen and query["arch"] == architecture, "foreign source or target")
        require(all(query[name] == value for name, value in {
            "route": "1", "toolkit": "12080", "ptx": "807", "environment": "1",
            "fast_math": "0", "roundoff": "0", "ieee": "1"}.items()), "target request drift")
        status = query["status"]
        require(query["source_match"] == ("0" if status == "2" else "1") and
                query["fp_options"] == ("1" if status == "6" else "0"), "wrong negative witness")
        seen[identity].append(status)
    require(seen == expected, "missing, duplicate or reordered target query coverage")
    return facts, legacy, queries


def tensor_inputs(fact, regime):
    """Create full X/gamma/beta tensors from native domains and operand IDs.

    Equal value IDs get one shared payload. Multiple source nodes sharing the
    same operands get identical inputs in a regime even if epsilon differs.
    This is explicit test input binding, not a framework-source equivalence proof.
    """
    rows, width = int(fact["rows"]), int(fact["width"])
    require(regime in REGIMES and rows <= 32 and width <= 1024, "test fixture resource bound")
    rng = random.Random(0x4c4e3145 + rows * 1024 + width)
    x = [(rng.getrandbits(1) << 31) | (rng.randrange(116, 131) << 23) | rng.getrandbits(23)
         for _ in range(rows * width)]
    gamma = [0x3f800000 if k % 2 else 0xbf800001 for k in range(width)]
    beta = [0 if k % 2 else fp.SIGN for k in range(width)]
    if regime == "cancellation":
        pattern = [0x4b800000, 0x3f800000, 0xcb800000] + [0] * max(0, width-3)
        x = [pattern[k] for _ in range(rows) for k in range(width)]
    elif regime == "subnormal":
        x = [(1 if k % 2 else fp.SIGN | 1) for k in range(rows * width)]
    elif regime == "exceptional":
        edge = [0x7f800001, fp.INF, fp.SIGN | fp.INF, fp.MAX, fp.SIGN, 0, 1]
        x = [edge[(r+k) % len(edge)] for r in range(rows) for k in range(width)]
        gamma = [0 if k % 2 else fp.MAX for k in range(width)]
    result = {fact["x"]: {"shape": [rows, width], "words": x},
              fact["gamma"]: {"shape": [width], "words": gamma}}
    if fact["beta"] not in result:
        result[fact["beta"]] = {"shape": [width], "words": beta}
    return result


def validate_variant(facts, legacy, variant):
    """Check declared test-suite diversity against actual native facts, not labels."""
    require(variant in ("default", "unit", "wide", "epsilon2", "subnormal", "mixed", "shared-affine"),
            "unknown source variant")
    shape = {"unit": (1, 1), "wide": (7, 65)}.get(variant, (3, 17))
    epsilon = {"epsilon2": "3a83126f", "subnormal": "00000001"}.get(variant, "3727c5ac")
    require(len(facts) == (3 if variant == "mixed" else 1) and len(legacy) == (1 if variant == "mixed" else 0),
            "source variant contract census mismatch")
    for index, fact in enumerate(facts):
        require((int(fact["rows"]), int(fact["width"])) == shape and
                fact["epsilon_bits"] == ("3f800000" if variant == "mixed" and index == 2 else epsilon),
                "source variant shape/epsilon mismatch")
        require((fact["gamma"] == fact["beta"]) == (variant == "shared-affine"), "source affine identity mismatch")


def invocations(facts):
    """Bind every admitted node and regime; never silently omit a native node."""
    return {"p%s_n%s_%s" % (fact["pu"], fact["node"], regime): {
                "source": {k: v for k, v in fact.items() if k != "phase"},
                "regime": regime, "tensors": tensor_inputs(fact, regime)}
            for fact in facts for regime in REGIMES}


def row_bindings(cases):
    """Slice complete tensors into ordered rows without changing affine broadcasts."""
    rows, origins = {}, {}
    for name, case in cases.items():
        fact, tensors = case["source"], case["tensors"]
        count, width = int(fact["rows"]), int(fact["width"])
        for r in range(count):
            row_id = name + "_r" + str(r)
            rows[row_id] = {"x": tensors[fact["x"]]["words"][r*width:(r+1)*width],
                            "gamma": tensors[fact["gamma"]]["words"],
                            "beta": tensors[fact["beta"]]["words"],
                            "epsilon": int(fact["epsilon_bits"], 16)}
            origins[row_id] = dict(invocation=name, row=r, pu=fact["pu"], node=fact["node"],
                                   result=fact["result"], source_line=fact["source_line"])
    return rows, origins


def audit_binding(source, target, architecture, cases, numerical, producer):
    """Reconstruct both the semantic and numerical join; trust no stored verdict."""
    facts, legacy, queries = join_contracts(source, target, architecture)
    require(json.dumps(cases, sort_keys=True, allow_nan=False) ==
            json.dumps(invocations(facts), sort_keys=True, allow_nan=False),
            "source/epsilon/domain/operand/input census drift")
    rows, origins = row_bindings(cases)
    states = expected_states(rows)
    census = audit_numbers(numerical, {}, states, {}, source_lines(producer))
    return dict(source_contracts=len(facts), v1_refusals=len(legacy), target_queries=len(queries),
                invocations=len(cases), rows=len(rows), state_comparisons=census["state_comparisons"],
                architecture=architecture, origins=origins)


def bound_requests(cases):
    """Canonicalize request order across JSON reloads without changing row order."""
    rows, _ = row_bindings(cases)
    return requests({}, dict(sorted(rows.items())), {})


def product_hashes(case):
    """Bind source images, mapped dumps, controls, raw traces and exact inputs."""
    names = ["ln_ref.B", "ln_ref.B.preserved.B", "ascii/ln_ref.T", "ascii/preserved.T", "target.t",
             "trace/preopt-on.t", "trace/default-trace.t", "trace/trace-off.t", "trace/preopt-off.t",
             "numerical/requests.txt", "numerical/arithmetic.t"]
    return {name: sha256(case / name) for name in names}


def audit_case(case, producer, host):
    """Verify retained bytes/controls and recompute the complete joined report."""
    manifest = json.loads((case / "binding.json").read_text())
    require(manifest.keys() == {"schema", "architecture", "invocations", "products", "host_sha256", "host_source_sha256"} and
            manifest["schema"] == "LN1-e-binding-v1", "unknown binding manifest/schema")
    require(manifest["products"] == product_hashes(case), "retained product digest mismatch")
    require(manifest["host_source_sha256"] == sha256(producer), "foreign numerical producer")
    require(manifest["host_sha256"] == sha256(host), "foreign numerical executable")
    require((case / "numerical/requests.txt").read_text() == bound_requests(manifest["invocations"]),
            "numerical request/input binding mismatch")
    require((case / "ln_ref.B").read_bytes().startswith(b"\x7fELF") and
            (case / "ln_ref.B").read_bytes() == (case / "ln_ref.B.preserved.B").read_bytes(), "native query mutated image")
    require((case / "ascii/ln_ref.T").read_bytes() == (case / "ascii/preserved.T").read_bytes(), "native mapped preservation failed")
    source = (case / "trace/preopt-on.t").read_text()
    require(audit_source(source) == audit_source((case / "trace/default-trace.t").read_text()), "default/on source drift")
    for name in ("trace-off", "preopt-off"):
        require("AI_NORM" not in (case / ("trace/" + name + ".t")).read_text(), "trace/PREOPT control ignored")
    return audit_binding(source, (case / "target.t").read_text(), manifest["architecture"],
                         manifest["invocations"], (case / "numerical/arithmetic.t").read_text(), producer)


def write_reports(case, output, producer, host):
    """Publish compact joined reports only after all stored evidence reaudits."""
    census = audit_case(case, producer, host)
    output.mkdir(parents=True, exist_ok=False)
    rows = ["# What it summarizes: exact native source/target joins and full-tensor numerical witness provenance.",
            "JOIN columns=invocation row pu node result architecture source_line"]
    for row in census["origins"].values():
        rows.append("JOIN " + " ".join(str(row[k]) for k in ("invocation", "row", "pu", "node", "result")) +
                    " " + census["architecture"] + " " + row["source_line"])
    detail = "\n".join(rows) + "\n"
    detail += "source=v2_ln_ref_1 target=conditional_isa_carrier_binding numerical=host_C0_C1_C2_MPFR_exact_states\n"
    detail += "input_shapes_affine_identity_epsilon=from_native_source v1=unbound_refused source_python_equivalence=not_claimed\n"
    (output / "binding.log").write_text(detail)
    numerical = ("# What it summarizes: same-contract numerical census, exact non-NaN bits and NaN-class acceptance.\n" +
                 "NUMERICAL " + " ".join(k + "=" + str(census[k]) for k in ("invocations", "rows", "state_comparisons")) + "\n" +
                 "policy=rne_gradual_ordered_no_fma_sqrt_then_div inputs=all_fp32_classes GPU=not_run image_execution=not_run\n")
    (output / "numerical.log").write_text(numerical)
    summary = ("# What it summarizes: integrated source/target/numerical binding and explicit remaining optimization gates.\n" +
               "BINDING " + " ".join(k + "=" + str(census[k]) for k in
                   ("source_contracts", "v1_refusals", "target_queries", "invocations", "rows", "state_comparisons")) + "\n" +
               "LN1-e=bounded_evidence_complete source=v2 target=conditional numerical=host_witnesses v1_migration=none\n" +
               "COST none DECISION none TRANSFORM none LN2_storage_events=open LN3_profitability=open LN4_lowering=open GPU=not_run\n")
    (output / "optimization-summary.log").write_text(summary)
    collection = "# What it summarizes: complete successful native source, target and joined numerical reports.\n"
    for name in ("source-reports/semantic.log", "target-reports/target-binding.log"):
        collection += (case / name).read_text()
    (output / "optimization.log").write_text(collection + detail + numerical + summary)
    return census
