"""R1 CPU numerical witnesses and read-only semantic reports, not optimization.

Authority: BUILDING-BLOCK-INGESTION-PLAN.md. Test-only alternatives illustrate
why permission and measured sample acceptance are independent requirements.
No sample result grants a compiler permission or a universal error guarantee.
"""

import argparse
import hashlib
import inspect
import json
import math
import os
from pathlib import Path

import torch
from torch.fx import symbolic_trace
from torch.fx.passes.shape_prop import ShapeProp

from open64_dsc.row_characterization import RowNumericalPermissions, characterize_row_graph
from models.building_blocks import make_fixture
from models.building_block_reference import reference

KINDS = ("reduction", "softmax", "layernorm")


def capture_rows(model, inputs):
    """Capture real source dataflow and current tensor descriptors without mutation."""
    graph = symbolic_trace(model)
    with torch.no_grad():
        ShapeProp(graph).propagate(*inputs)
    return graph


def _partition_sum(row, chunk):
    """Test-only FP32 partial sums followed by an explicit merge reduction."""
    return torch.stack([part.sum() for part in row.split(chunk)]).sum()


def _online_softmax(row, chunk):
    """Test-only FP32 max/sum state merge, with an explicit empty-state guard."""
    maximum = total = None
    for part in row.split(chunk):
        part_max = part.max()
        part_sum = torch.exp(part - part_max).sum()
        if maximum is None:
            maximum, total = part_max, part_sum
        else:
            new_max = torch.maximum(maximum, part_max)
            total = torch.exp(maximum - new_max) * total + torch.exp(part_max - new_max) * part_sum
            maximum = new_max
    return torch.exp(row - maximum) / total


def _welford(row, chunk):
    """Test-only FP32 Welford partials and pairwise state merge; not a kernel."""
    state = None
    for part in row.split(chunk):
        mean, m2 = torch.zeros((), dtype=row.dtype), torch.zeros((), dtype=row.dtype)
        for count, value in enumerate(part, 1):
            delta = value - mean
            mean = mean + delta / count
            m2 = m2 + delta * (value - mean)
        current = (part.numel(), mean, m2)
        if state is None:
            state = current
        else:
            n1, mu1, sum1 = state
            n2, mu2, sum2 = current
            delta = mu2 - mu1
            state = (n1 + n2, mu1 + delta * n2 / (n1 + n2),
                     sum1 + sum2 + delta * delta * n1 * n2 / (n1 + n2))
    return state[1], state[2] / state[0]


def execute_alternative(kind, strategy, model, inputs):
    """Execute a named CPU test witness, never install it into captured source."""
    if strategy in ("source_sum", "retain_exp", "centered_population_variance"):
        return model(*inputs)
    value = inputs[0]
    if strategy == "partition_and_merge":
        return torch.stack([_partition_sum(row, 4) for row in value])
    if strategy == "recompute_exp_same_reductions":
        maximum = value.amax(dim=-1, keepdim=True)
        denominator = torch.exp(value - maximum).sum(dim=-1, keepdim=True)
        return torch.exp(value - maximum) / denominator
    if strategy == "online_max_sum_state":
        return torch.stack([_online_softmax(row, 4) for row in value])
    if strategy == "welford_partition_merge":
        results = []
        for row in value:
            mean, variance = _welford(row, 4)
            results.append((row - mean) * torch.rsqrt(variance + 1e-5) * inputs[1] + inputs[2])
        return torch.stack(results)
    raise ValueError("unknown test strategy: " + strategy)


def acceptance(actual, expected, rtol=2e-5, atol=2e-6):
    """Check elementwise and aggregate error without relative-only zero hazards."""
    if not (math.isfinite(rtol) and math.isfinite(atol) and rtol >= 0 and atol > 0):
        raise ValueError("invalid numerical thresholds")
    if actual.shape != expected.shape or actual.numel() == 0:
        return {"accepted": False, "reason": "shape_or_empty_output"}
    if not torch.isfinite(actual).all().item() or not torch.isfinite(expected).all().item():
        return {"accepted": False, "reason": "nonfinite_result"}
    expected = expected.double()
    error = (actual.double() - expected.double()).abs()
    allowance = atol + rtol * expected.abs()
    error_norm = torch.linalg.vector_norm(error).item()
    ref_norm = torch.linalg.vector_norm(expected).item()
    accepted = bool((error <= allowance).all().item() and
                    error_norm <= atol * math.sqrt(error.numel()) + rtol * ref_norm)
    return {"accepted": accepted, "reason": "within_sample_tolerance" if accepted else "sample_error_limit",
            "max_abs_error": error.max().item(), "max_scaled_error": (error / allowance).max().item(),
            "error_l2": error_norm, "reference_l2": ref_norm,
            "relative_l2_diagnostic": error_norm / max(ref_norm, atol)}


def numerical_cases(kind):
    """Return deterministic ordinary, adversarial and out-of-contract samples."""
    model, base = make_fixture(kind)
    cases = [("fixture", base)]
    if kind == "reduction":
        cases.extend([
            ("cancellation", (torch.tensor([[1e8, 1., -1e8, 1.]], dtype=torch.float32),)),
            ("zero", (torch.zeros(2, 17, dtype=torch.float32),)),
            ("overflow", (torch.tensor([[3e38, 3e38]], dtype=torch.float32),)),
        ])
    elif kind == "softmax":
        cases.extend([
            ("constant", (torch.full((2, 17), 10000., dtype=torch.float32),)),
            ("large_range", (torch.tensor([[-10000., 0., 10000.]], dtype=torch.float32),)),
            ("all_negative_infinity", (torch.full((1, 3), -math.inf, dtype=torch.float32),)),
        ])
    elif kind == "layernorm":
        near = torch.tensor([[10000. + (i - 7.5) / 16 for i in range(16)]], dtype=torch.float32)
        cases.extend([
            ("constant", (torch.full_like(base[0], 7.), *base[1:])),
            ("large_offset_small_variance", (near, torch.ones(16, dtype=torch.float32), torch.zeros(16, dtype=torch.float32))),
            ("squared_overflow", (torch.tensor([[1e20, -1e20]], dtype=torch.float32),
                                  torch.ones(2, dtype=torch.float32), torch.zeros(2, dtype=torch.float32))),
        ])
    else:
        raise ValueError("unsupported row kind")
    return model, cases


def assess_case(kind, model, inputs, permissions):
    """Gate value-domain/permission first; preserve positive and negative evidence."""
    graph = capture_rows(model, inputs)
    characterization = characterize_row_graph(graph, permissions)
    result = {"characterization": characterization, "alternatives": [],
              "source": {"file": Path(inspect.getsourcefile(model.forward)).name,
                         "function_line": inspect.getsourcelines(model.forward)[1]}}
    if not all(torch.isfinite(value).all().item() for value in inputs):
        result["value_domain"] = "rejected_nonfinite_inputs"
        return result
    result["value_domain"] = "finite_inputs; intermediate_domain_not_assumed"
    expected = torch.tensor(reference(kind, [value.tolist() for value in inputs]), dtype=torch.float64)
    for alternative in characterization["alternatives"]:
        strategy = alternative["id"]
        if alternative["missing_permissions"]:
            record = {"id": strategy, "accepted": False, "reason": "permission_required"}
        else:
            with torch.no_grad():
                actual = execute_alternative(kind, strategy, model, inputs)
            record = {"id": strategy, **acceptance(actual, expected)}
        result["alternatives"].append(record)
    return result


def build_report():
    """Collect semantic derivation and strict/permissive sample acceptance ledgers."""
    report = {"schema": 1, "torch": torch.__version__, "device": "cpu",
              "native_whirl": "not_run", "transformation": "none",
              "thresholds": {"rtol": 2e-5, "atol": 2e-6}, "cases": {}}
    for kind in KINDS:
        model, cases = numerical_cases(kind)
        for name, inputs in cases:
            key = kind + "/" + name
            strict = assess_case(kind, model, inputs, RowNumericalPermissions())
            permitted = assess_case(kind, model, inputs, RowNumericalPermissions(True, True))
            report["cases"][key] = {"strict": strict, "permitted": permitted}
    base = Path(__file__).resolve().parent
    sources = [Path(__file__).resolve(), base / "models/building_blocks.py",
               base / "models/building_block_reference.py",
               Path(inspect.getsourcefile(characterize_row_graph)),
               base / "test_row_semantic_capture_optional.py"]
    report["producer_sources"] = {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in sources}
    return report


def write_report(destination):
    """Retain fresh reports, exact typed inputs, graphs and replayable source."""
    root = Path(destination)
    root.mkdir(parents=True, exist_ok=False)
    report = build_report()
    lines = ["# What it summarizes: R1 source-derived row semantics, numerical permissions and CPU sample acceptance/rejection; no native WHIRL transformation or GPU certificate."]
    lines.append("# columns: sample mode alternative accepted reason source_file source_function_line")
    for kind in KINDS:
        model, cases = numerical_cases(kind)
        for name, inputs in cases:
            case = root / kind / name
            case.mkdir(parents=True)
            graph = capture_rows(model, inputs)
            (case / "graph.txt").write_text(str(graph.graph) + "\n")
            # Invalid-domain witnesses retain a readable nonfinite token, not invalid JSON.
            payload = [{"shape": list(value.shape), "dtype": str(value.dtype),
                        "values": _json_values(value.tolist())} for value in inputs]
            (case / "inputs.json").write_text(json.dumps(payload, indent=2, allow_nan=False) + "\n")
            witnesses = {}
            if all(torch.isfinite(value).all().item() for value in inputs):
                witnesses["reference_fp64"] = reference(kind, [value.tolist() for value in inputs])
                witnesses["output_fp32"] = {}
                alternatives = report["cases"][kind + "/" + name]["permitted"]["alternatives"]
                for alternative in alternatives:
                    with torch.no_grad():
                        value = execute_alternative(kind, alternative["id"], model, inputs)
                    witnesses["output_fp32"][alternative["id"]] = _json_values(value.tolist())
            else:
                witnesses["reason"] = "rejected_nonfinite_inputs"
            (case / "numerical-values.json").write_text(json.dumps(witnesses, indent=2, allow_nan=False) + "\n")
            for mode in ("strict", "permitted"):
                item = report["cases"][kind + "/" + name][mode]
                for alt in item["alternatives"]:
                    lines.append("%s %s %s %s %s %s %s" %
                                 (kind + "/" + name, mode, alt["id"], alt["accepted"], alt["reason"],
                                  item["source"]["file"], item["source"]["function_line"]))
                if not item["alternatives"]:
                    lines.append("%s %s - False %s %s %s" %
                                 (kind + "/" + name, mode, item["value_domain"],
                                  item["source"]["file"], item["source"]["function_line"]))
    (root / "report.json").write_text(json.dumps(report, indent=2, sort_keys=True, allow_nan=False) + "\n")
    (root / "semantic-summary.log").write_text("\n".join(lines) + "\n")
    source = root / "source"
    for relative in ("row_semantic_report.py", "models/__init__.py", "models/building_blocks.py",
                     "models/building_block_reference.py", "test_row_semantic_capture_optional.py"):
        target = source / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((Path(__file__).resolve().parent / relative).read_bytes())
    target = source / "open64_dsc/row_characterization.py"
    target.parent.mkdir()
    target.write_bytes(Path(inspect.getsourcefile(characterize_row_graph)).read_bytes())
    (root / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file()))
    return report


def _json_values(value):
    """Encode retained nonfinite input witnesses as explicit strings."""
    if isinstance(value, list):
        return [_json_values(item) for item in value]
    return value if math.isfinite(value) else str(value)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir")
    args = parser.parse_args()
    root = args.artifact_dir
    if root is None and os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR"):
        root = str(Path(os.environ["OPEN64_DSL_TEST_ARTIFACT_DIR"]) / "row-semantics")
    if root is None:
        parser.error("provide --artifact-dir or OPEN64_DSL_TEST_ARTIFACT_DIR")
    result = write_report(root)
    print("R1: %d cases; source characterization/numerical witnesses only" % len(result["cases"]))
