#!/usr/bin/env python3
"""LN1-d exact/MPFR/native witnesses; never a compiler or GPU execution claim.

Design: doc/AI-COMPILER-LAYERNORM-ARITHMETIC-ORACLE-v0.1.md.
Only new output directories are accepted. Strict positional traces are checked
against a separately constructed exact specification census, not against their
own reported counts or outputs. No reduction implementation is imported.
"""
import argparse
from fractions import Fraction
import hashlib
import json
from pathlib import Path
import random
import re
import subprocess

import normalization_oracle as fp

ENGINES = ("C0", "C1", "C2", "MPFR")
HEADERS = (
    "AI_NORM_PRIM 1 HEADER case operation left right third host mpfr precision source_line",
    "AI_NORM_NUM 1 HEADER case engine stage index bits source_line",
)
HEX = re.compile(r"[0-9a-f]{8}\Z")


def sha256(path):
    """Record exact retained inputs and tools, not names or modification times."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def fixtures():
    """Build deterministic edge/random witnesses without host floating arithmetic."""
    rng = random.Random(0x4c4e3144)
    edge = [0, fp.SIGN, 1, fp.SIGN | 1, 2, 0x007fffff, 0x00800000,
            0x00800001, 0x33800000, 0x3f000000, 0x3f800000, 0x3f800001,
            0xbf800000, 0x40c00000, 0x4b800000, fp.MAX, fp.SIGN | fp.MAX,
            fp.INF, fp.SIGN | fp.INF, fp.QNAN, 0x7f800001, 0xffc01234]
    pairs = [(a, b) for a in edge for b in edge]
    pairs += [(rng.getrandbits(32), rng.getrandbits(32)) for _ in range(2048)]
    ops = {}
    arithmetic = (fp.add, fp.sub, fp.mul, fp.div, lambda a, b: fp.sqrt(a))
    for index, (a, b) in enumerate(pairs):
        for operation, function in enumerate(arithmetic, 1):
            ops[f"p{index}_{operation}"] = [operation, a, b, 0, function(a, b)]
    a, c = 0x3f800001, 0x33800000
    ops["fma_square"] = [6, a, a, c, fp.round_fraction(fp.finite(a)**2 + fp.finite(c))]
    ops["fma_affine"] = [6, a, a, 0xbf800002,
                         fp.round_fraction(fp.finite(a)**2 + fp.finite(0xbf800002))]
    # An exactly halfway product plus min-subnormal must refine the MPFR
    # interval beyond 64/128 bits, rather than accepting its midpoint.
    a = 0x3f800800
    ops["fma_refine"] = [6, a, a, 1, fp.round_fraction(fp.finite(a)**2 + fp.finite(1))]
    reciprocal = fp.div(0x3f800000, 0x40400000)
    ops["divide5by3"] = [4, 0x40a00000, 0x40400000, 0, fp.div(0x40a00000, 0x40400000)]
    ops["reciprocal3"] = [4, 0x3f800000, 0x40400000, 0, reciprocal]
    ops["reciprocal_product5"] = [3, 0x40a00000, reciprocal, 0, fp.mul(0x40a00000, reciprocal)]
    rows = {}

    def row(name, values, epsilon=0x3727c5ac, gamma=None, beta=None):
        """Give every row explicit affine inputs and an exact positive epsilon."""
        n = len(values)
        rows[name] = {"x": values, "gamma": gamma if gamma is not None else [0x3f800000]*n,
                      "beta": beta if beta is not None else [0]*n, "epsilon": epsilon}

    row("negative_zero", [fp.SIGN])
    row("subnormal_tie", [1, 0], 1)
    row("cancel", [0x4b800000, 0x3f800000, 0xcb800000])
    row("cancel_reorder", [0x4b800000, 0xcb800000, 0x3f800000])
    row("overflow_mean", [fp.MAX, fp.MAX])
    row("zero_subnormal_epsilon", [0, 0], 1)
    row("t6", [0], 0x40c00000)
    row("t1", [0], 0x3f800000)
    row("t4", [0], 0x40800000)
    row("nan", [fp.QNAN, 0x3f800000])
    row("signaling_nan", [0x7f800001])
    row("infinite", [fp.INF])
    row("opposed_infinite", [fp.INF, fp.SIGN | fp.INF])
    row("zero_gamma_invalid", [fp.INF], gamma=[0], beta=[0x3f800000])
    row("affine_overflow", [0xbf800000, 0x3f800000], gamma=[fp.MAX]*2, beta=[fp.SIGN | fp.MAX, fp.MAX])
    row("signed_affine", [fp.SIGN, 0], gamma=[fp.SIGN, fp.SIGN], beta=[fp.SIGN, fp.SIGN])
    for n in (1, 2, 3, 17, 65, 127):
        # Bounded finite values keep random rows informative instead of all NaN.
        values = [(rng.getrandbits(1) << 31) | (rng.randrange(110, 134) << 23) | rng.getrandbits(23) for _ in range(n)]
        row(f"finite_{n}", values)
        row(f"bits_{n}", [rng.getrandbits(32) for _ in range(n)],
            gamma=[rng.getrandbits(32) for _ in range(n)], beta=[rng.getrandbits(32) for _ in range(n)])
    return ops, rows


def expected_states(rows):
    """Compute a complete state census independently of host trace generation."""
    return {name: fp.reference_row(r["x"], r["gamma"], r["beta"], r["epsilon"]) for name, r in rows.items()}


def source_lines(producer):
    """Bind last-column coordinates to the retained test producer, not DSL code."""
    source = Path(producer).read_text().splitlines()
    patterns = {"num": "std::setfill('0') << bits", "env": "mpfr_get_version() <<",
                "prim": "witness << ' ' << std::setw(8) << reference"}
    result = {}
    for key, pattern in patterns.items():
        matches = [i for i, line in enumerate(source, 1) if pattern in line and "__LINE__" in line]
        if len(matches) != 1:
            raise ValueError("ambiguous producer line: " + key)
        result[key] = matches[0]
    return result


def audit(trace, ops, states, counts, lines):
    """Reject missing/duplicate/unknown rows, schema drift and non-NaN bit drift."""
    records = trace.splitlines()
    if len(records) < 3 or tuple(records[1:3]) != HEADERS:
        raise ValueError("missing exact numerical trace headers")
    env = records[0].split()
    if len(env) != 10 or env[:8] != "AI_NORM_ENV 1 HOST rne 0 0 masked class_only".split() or env[-1] != str(lines["env"]) or not re.fullmatch(r"\d+\.\d+\.\d+", env[8]):
        raise ValueError("unverified arithmetic environment")
    expected = {(name, engine, stage, str(index)): value
                for name, state in states.items() for engine in ENGINES
                for stage, values in state.items() for index, value in enumerate(values)}
    expected.update({(name, "COUNT", "count", "0"): fp.count_bits(n) for name, n in counts.items()})
    seen_ops, seen_states, precisions = set(), set(), set()
    for record in records[3:]:
        fields = record.split()
        if fields[:3] == ["AI_NORM_PRIM", "1", "VALUE"]:
            if len(fields) != 12 or fields[3] not in ops or fields[3] in seen_ops:
                raise ValueError("unknown, duplicate or malformed primitive")
            operation, a, b, c, value = ops[fields[3]]
            if fields[4:8] != [str(operation), f"{a:08x}", f"{b:08x}", f"{c:08x}"] or fields[-1] != str(lines["prim"]) or fields[-2] not in {"64", "128", "256", "512", "1024"}:
                raise ValueError("primitive input/provenance drift")
            for bits in fields[8:10]:
                if not HEX.fullmatch(bits) or not fp.equivalent(int(bits, 16), value):
                    raise ValueError("primitive bit/class mismatch: " + fields[3])
            # The schema has two results, precision and last-column source only.
            seen_ops.add(fields[3]); precisions.add(int(fields[-2]))
        elif fields[:3] == ["AI_NORM_NUM", "1", "VALUE"]:
            key = tuple(fields[3:7])
            if len(fields) != 9 or key not in expected or key in seen_states or fields[-1] != str(lines["num"]):
                raise ValueError("unknown, duplicate or malformed state")
            if not HEX.fullmatch(fields[7]) or not fp.equivalent(int(fields[7], 16), expected[key]):
                raise ValueError("state bit/class mismatch: " + "/".join(key))
            seen_states.add(key)
        else:
            raise ValueError("unknown numerical trace record")
    if seen_ops != set(ops) or seen_states != set(expected):
        raise ValueError("incomplete primitive/state census")
    return {"primitive_cases": len(ops), "state_comparisons": len(expected),
            "row_cases": len(states), "host_realizations": 3, "mpfr_versions": [env[8]],
            "mpfr_precisions": sorted(precisions), "count_cases": len(counts)}


def requests(ops, rows, counts):
    """Keep the exact input transcript replayable without fixture regeneration."""
    output = [f"OP {name} {op} {a:08x} {b:08x} {c:08x}" for name, (op, a, b, c, _) in ops.items()]
    for name, row in rows.items():
        words = " ".join(f"{x:08x}" for key in ("x", "gamma", "beta") for x in row[key])
        for engine in ENGINES:
            output.append(f"ROW {name} {engine} 2 {len(row['x'])} {row['epsilon']:08x} {words}")
    output += [f"COUNT {name} {n}" for name, n in counts.items()]
    return "\n".join(output) + "\n"


def counterexamples(states):
    """Document exact disagreements, not blanket numerical-accuracy claims."""
    a, c = 0x3f800001, 0x33800000
    separate = fp.add(fp.mul(a, a), c)
    fused = fp.round_fraction(fp.finite(a)**2 + fp.finite(c))
    t = fp.finite(0x40c00000)
    once = fp.round_sqrt_fraction(1 / t)
    twice = states["t6"]["q"][0]
    if separate == fused or once == twice:
        raise ValueError("required illegal-recipe counterexample missing")
    return {"square_add": {"separate": f"{separate:08x}", "fused": f"{fused:08x}"},
            "t6": {"sqrt": f"{states['t6']['h'][0]:08x}", "two_round_q": f"{twice:08x}", "one_round_rsqrt": f"{once:08x}"},
            "first_add": {"required": f"{states['negative_zero']['sum_step'][0]:08x}", "identity_copy": "80000000"},
            "reorder_mu": {"ordered": f"{states['cancel']['mu'][0]:08x}", "reordered": f"{states['cancel_reorder']['mu'][0]:08x}"},
            "division": {"direct5by3": f"{fp.div(0x40a00000, 0x40400000):08x}",
                         "rounded_reciprocal_product": f"{fp.mul(0x40a00000, fp.div(0x3f800000, 0x40400000)):08x}"},
            "mean_scaling": {"fold_then_divide": f"{states['overflow_mean']['mu'][0]:08x}",
                             "divide_each_then_fold": f"{fp.add(fp.div(fp.MAX, 0x40000000), fp.div(fp.MAX, 0x40000000)):08x}"}}


def review(host, output, producer):
    """Produce fresh evidence; failure cannot overwrite or certify an old run."""
    output.mkdir(parents=True, exist_ok=False)
    ops, rows = fixtures()
    states = expected_states(rows)
    counts = {f"n{n}": n for n in (1, 2, 3, 17, 2**24-1, 2**24)}
    transcript = requests(ops, rows, counts)
    (output / "requests.txt").write_text(transcript)
    process = subprocess.run([str(host.resolve())], input=transcript, text=True, capture_output=True, check=False)
    (output / "arithmetic.t").write_text(process.stdout)
    (output / "host.stderr").write_text(process.stderr)
    if process.returncode:
        raise ValueError("host witness failed: " + process.stderr)
    census = audit(process.stdout, ops, states, counts, source_lines(producer))
    rejections = {}
    for name, request in {
        "v1": "ROW bad C0 1 1 3f800000 00000000 3f800000 00000000",
        "count_zero": "COUNT bad 0", "count_large": "COUNT bad 16777217",
        "unknown_engine": "ROW bad UNKNOWN 2 1 3f800000",
        "epsilon_zero": "ROW bad C0 2 1 00000000", "epsilon_negative": "ROW bad C0 2 1 bf800000",
        "epsilon_infinite": "ROW bad C0 2 1 7f800000", "epsilon_nan": "ROW bad C0 2 1 7fc00000",
        "truncated_row": "ROW bad C0 2 1 3f800000 00000000",
        "bad_word": "ROW bad C0 2 1 3f800000 100000000",
    }.items():
        rejected = subprocess.run([str(host.resolve())], input=request + "\n", text=True, capture_output=True, check=False)
        (output / f"reject-{name}.t").write_text(rejected.stdout)
        (output / f"reject-{name}.stderr").write_text(rejected.stderr)
        if rejected.returncode != 2 or " VALUE " in rejected.stdout:
            raise ValueError("invalid input was admitted: " + name)
        rejections[name] = rejected.returncode
    manifest = {"schema": "LN1-d-oracle-v1", "semantic_version": 2,
                "scope": "exact specification and standalone host witnesses; no compiled LayerNorm or GPU execution",
                "seed": "0x4c4e3144", "host_sha256": sha256(host), "producer_sha256": sha256(producer),
                "oracle_sha256": sha256(Path(__file__).with_name("normalization_oracle.py")),
                "reviewer_sha256": sha256(__file__), "census": census, "rejections": rejections,
                "counterexamples": counterexamples(states), "rows": rows, "expected_states": states,
                "counts": counts, "source_lines": source_lines(producer), "trace_sha256": sha256(output / "arithmetic.t")}
    (output / "manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
    summary = ("# What it summarizes: LN-REF-1 v2 exact-bit arithmetic, independent MPFR intervals and host C0/C1/C2 witnesses.\n"
               + f"NUMERICAL pass primitives={len(ops)} rows={len(rows)} states={census['state_comparisons']} rejected={len(rejections)}\n"
               + "CONTRACT rne gradual no_fma ordered count_exact sqrt_then_div non_nan_bits_exact nan_class_only\n"
               + "COUNTEREXAMPLE " + json.dumps(manifest["counterexamples"], separators=(",", ":")) + "\n"
               + "V1 preserved unresolved; no automatic promotion; production/reduction code unchanged\n"
               + "DECISION none COST none TRANSFORM none GPU unverified; LN1-e integrated evidence remains open\n")
    for name in ("numerical.log", "optimization.log", "optimization-summary.log"):
        (output / name).write_text(summary)
    return manifest


def main():
    """Use explicit host executable and fresh review directory for reproducibility."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    manifest = review(args.host, args.output, Path(__file__).with_name("normalization_oracle_host.cxx"))
    print(json.dumps(manifest["census"], sort_keys=True))


if __name__ == "__main__":
    main()
