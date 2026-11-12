"""Run the standalone absorbed MLA source/capture case; see MLA-INGESTION-PLAN.md."""

import argparse
from collections import Counter
import hashlib
import inspect
import json
import os
from pathlib import Path
import shutil
import sys

import torch
from torch.fx import symbolic_trace
from torch.fx.passes.shape_prop import ShapeProp

from models.absorbed_mla import CONTEXT_LENGTHS, INPUT_NAMES, make_fixture, validate_inputs
from models.mla_reference import expanded_reference


RTOL = 2e-5
ATOL = 2e-6


def capture(model, inputs):
    """Capture genuine tensor operations; record requirements without inventing IR."""
    validate_inputs(inputs)
    graph = symbolic_trace(model)
    with torch.no_grad():
        ShapeProp(graph).propagate(*inputs)
    nodes = []
    for node in graph.graph.nodes:
        target = str(node.target)
        if node.op == "call_function":
            target = node.target.__module__ + "." + node.target.__name__
        meta = node.meta.get("tensor_meta")
        nodes.append({
            "name": node.name, "op": node.op, "target": target,
            "arguments": str(node.args), "keyword_arguments": str(node.kwargs),
            "shape": list(meta.shape) if hasattr(meta, "shape") else None,
            "dtype": str(meta.dtype) if hasattr(meta, "dtype") else None,
            "classification": ("region_contract" if node.op in ("placeholder", "output")
                               else "common_tensor_or_numerical_requirement"),
        })
    counts = Counter(node["op"] + ":" + node["target"] for node in nodes)
    return graph, {"nodes": nodes, "operators": dict(sorted(counts.items()))}


def probe_ingestion(model, inputs):
    """Exercise the existing mapper only; no native image or fabricated opcode."""
    try:
        from open64_dsc import export_to_whirl
        from open64_dsc.options import WhirlExportOptions
    except ModuleNotFoundError as error:
        if error.name != "open64_dsc":
            raise
        return {"status": "not_available", "diagnostic": "open64_dsc is not on PYTHONPATH"}
    try:
        module = export_to_whirl(
            model, inputs, WhirlExportOptions(backend="mock", model_name="mla_absorbed"))
    except NotImplementedError as error:
        return {"status": "blocked", "diagnostic": str(error)}
    return {"status": "mock_mapping_only", "operators": module.operators,
            "diagnostic": "Mock mapping is not native WHIRL or executable validation"}


def check_case(context, length=None):
    """Check eager and captured code against independently expanded scalar FP64."""
    model, inputs = make_fixture(context, length)
    originals = tuple(value.clone() for value in inputs)
    graph, structure = capture(model, inputs)
    expected = tuple(torch.tensor(value, dtype=torch.float64)
                     for value in expanded_reference(
                         *[value.tolist() for value in inputs], model.alpha))
    with torch.no_grad():
        actual = model(*inputs)
        captured = graph(*inputs)
        repeated = model(*inputs)
    for value, reference, fx_value, replay in zip(actual, expected, captured, repeated):
        torch.testing.assert_close(value.double(), reference, rtol=RTOL, atol=ATOL)
        torch.testing.assert_close(fx_value, value, rtol=0, atol=0)
        torch.testing.assert_close(replay, value, rtol=0, atol=0)
    for original, value in zip(originals, inputs):
        torch.testing.assert_close(value, original, rtol=0, atol=0)
    finite_lse = torch.isfinite(expected[1])
    lse_error = ((actual[1].double()[finite_lse] - expected[1][finite_lse]).abs().max().item()
                 if finite_lse.any().item() else 0.0)
    report = {
        "context": context, "length": inputs[2].shape[1],
        "input_shapes": {name: list(value.shape) for name, value in zip(INPUT_NAMES, inputs)},
        "scale": model.alpha, "rtol": RTOL, "atol": ATOL,
        "max_output_abs_error": (actual[0].double() - expected[0]).abs().max().item(),
        "max_finite_lse_abs_error": lse_error,
        "correctness": "PASS", "fx_replay": "PASS", "no_input_mutation": "PASS",
        "ingestion": probe_ingestion(model, inputs),
    }
    return report, inputs, expected, actual, graph, structure


def write_artifacts(destination, cases=("short", "long"), lengths=None):
    """Retain replayable inputs and source evidence in a fresh, never-replaced root."""
    if not cases or any(context not in CONTEXT_LENGTHS for context in cases):
        raise ValueError("cases must contain short and/or long")
    root = Path(destination)
    root.mkdir(parents=True, exist_ok=False)
    report = {
        "schema": 1, "fixture": "absorbed_mla_decode_v0.1", "torch": torch.__version__,
        "python": sys.version, "device": "cpu", "dtype": "float32",
        "native_whirl": "not_run", "gpu_validation": "not_run", "cases": {},
        "reference": "Independent scalar FP64 expanded keys and values",
    }
    summary = ["# What it summarizes: absorbed MLA CPU FP32 source and real FX capture versus scalar expanded FP64; no native WHIRL or GPU certification."]
    for context in cases:
        result, inputs, expected, actual, graph, structure = check_case(
            context, (lengths or {}).get(context))
        case = root / context
        case.mkdir()
        torch.save(dict(zip(INPUT_NAMES, inputs)), case / "inputs.pt")
        torch.save({"output": expected[0], "lse": expected[1]}, case / "reference.pt")
        torch.save({"output": actual[0], "lse": actual[1]}, case / "actual.pt")
        (case / "graph.txt").write_text(str(graph.graph) + "\n")
        (case / "forward.py").write_text(graph.code)
        (case / "census.json").write_text(json.dumps(structure, indent=2) + "\n")
        report["cases"][context] = result
        summary.append("%s PASS L=%d output_error=%.9g lse_error=%.9g ingestion=%s" %
                       (context, result["length"], result["max_output_abs_error"],
                        result["max_finite_lse_abs_error"], result["ingestion"]["status"]))
    source = root / "source"
    (source / "models").mkdir(parents=True)
    tests = Path(__file__).parent
    for name in ("mla_standalone.py", "test_mla_reference.py", "test_mla_capture_optional.py"):
        shutil.copyfile(tests / name, source / name)
    for module in (inspect.getmodule(make_fixture), inspect.getmodule(expanded_reference)):
        path = Path(module.__file__)
        shutil.copyfile(path, source / "models" / path.name)
    (source / "models" / "__init__.py").write_text("")
    plan = tests.parents[1] / "MLA-INGESTION-PLAN.md"
    if plan.is_file():
        shutil.copyfile(plan, source / plan.name)
    (root / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    (root / "capture-summary.log").write_text("\n".join(summary) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file()))
    return report


def main():
    """Default to both contexts; optionally save source/capture evidence for replay."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", choices=("short", "long", "all"), default="all")
    parser.add_argument("--short-length", type=int)
    parser.add_argument("--long-length", type=int)
    parser.add_argument("--artifact-dir", type=Path)
    args = parser.parse_args()
    cases = tuple(CONTEXT_LENGTHS) if args.case == "all" else (args.case,)
    lengths = {name: value for name, value in
               (("short", args.short_length), ("long", args.long_length)) if value is not None}
    destination = args.artifact_dir
    if destination is None and os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR"):
        destination = Path(os.environ["OPEN64_DSL_TEST_ARTIFACT_DIR"]) / "mla-absorbed"
    if destination:
        result = write_artifacts(destination, cases, lengths)
    else:
        result = {"cases": {name: check_case(name, lengths.get(name))[0] for name in cases},
                  "native_whirl": "not_run", "gpu_validation": "not_run"}
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
