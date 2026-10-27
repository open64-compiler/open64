"""Reviewable FX evidence for BUILDING-BLOCK-INGESTION-PLAN.md.

Run from the tests directory (or put it on PYTHONPATH). The output is a source
capture receipt, never a substitute for native .B/.T certification.
"""

import argparse
from collections import Counter
import hashlib
import inspect
import json
import os
from pathlib import Path

import torch
from torch.fx import symbolic_trace
from torch.fx.passes.shape_prop import ShapeProp

from models.building_blocks import NAMES, make_fixture
from models.building_block_reference import reference


def _target(node):
    """Use semantic target names rather than version-dependent repr addresses."""
    if node.op == "call_function":
        return node.target.__module__ + "." + node.target.__name__
    return str(node.target)


def capture(name):
    """Capture real nodes and shapes without claiming downstream support."""
    model, inputs = make_fixture(name)
    graph = symbolic_trace(model)
    with torch.no_grad():
        ShapeProp(graph).propagate(*inputs)
    nodes = []
    for node in graph.graph.nodes:
        meta = node.meta.get("tensor_meta")
        nodes.append({
            "name": node.name, "op": node.op, "target": _target(node),
            "arguments": str(node.args), "keyword_arguments": str(node.kwargs),
            "shape": list(meta.shape) if hasattr(meta, "shape") else None,
            "dtype": str(meta.dtype) if hasattr(meta, "dtype") else None,
        })
    counts = Counter(node["op"] + ":" + node["target"] for node in nodes)
    return graph, {"nodes": nodes, "operators": dict(sorted(counts.items()))}


def census():
    """Produce the six-case structural drift baseline from actual FX capture."""
    return {name: capture(name)[1] for name in NAMES}


def write_artifacts(destination):
    """Write fresh per-case inputs, reference results, graph and census evidence."""
    root = Path(destination)
    root.mkdir(parents=True, exist_ok=False)
    summary = ["# What it summarizes: six CPU FP32 source fixtures, scalar FP64 reference errors and real FX capture; no native WHIRL or GPU certification."]
    report = {"schema": 1, "torch": torch.__version__, "device": "cpu",
              "native_whirl": "not_run", "cases": {}}
    for name in NAMES:
        model, inputs = make_fixture(name)
        graph, structure = capture(name)
        with torch.no_grad():
            output = model(*inputs)
            captured = graph(*inputs)
        expected_list = reference(name, [value.tolist() for value in inputs])
        expected = torch.tensor(expected_list, dtype=torch.float64)
        torch.testing.assert_close(output.double(), expected, rtol=2e-5, atol=2e-6)
        torch.testing.assert_close(captured, output, rtol=0, atol=0)
        error = (output.double() - expected).abs().max().item()
        case = root / name
        case.mkdir()
        payload = {"inputs": [{"dtype": str(value.dtype), "shape": list(value.shape),
                               "values": value.tolist()} for value in inputs],
                   "reference_fp64": expected_list, "output_fp32": output.tolist()}
        (case / "values.json").write_text(json.dumps(payload, indent=2) + "\n")
        (case / "census.json").write_text(json.dumps(structure, indent=2) + "\n")
        (case / "graph.txt").write_text(str(graph.graph) + "\n")
        (case / "forward.py").write_text(graph.code)
        report["cases"][name] = {"max_abs_error": error, "node_count": len(structure["nodes"])}
        summary.append("%s PASS nodes=%d max_abs_error=%.9g" %
                       (name, len(structure["nodes"]), error))
    source = root / "source"
    source.mkdir()
    (source / "models").mkdir()
    for module in (inspect.getmodule(make_fixture), inspect.getmodule(reference)):
        path = Path(module.__file__)
        (source / "models" / path.name).write_bytes(path.read_bytes())
    package = Path(inspect.getmodule(make_fixture).__file__).with_name("__init__.py")
    (source / "models" / package.name).write_bytes(package.read_bytes())
    (source / Path(__file__).name).write_bytes(Path(__file__).read_bytes())
    for name in ("test_building_blocks_capture_optional.py", "test_building_block_reference.py"):
        test = Path(__file__).with_name(name)
        (source / test.name).write_bytes(test.read_bytes())
    (source / "golden").mkdir()
    golden = Path(__file__).parent / "golden" / "building_blocks_fx.json"
    (source / "golden" / golden.name).write_bytes(golden.read_bytes())
    (root / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    (root / "capture-summary.log").write_text("\n".join(summary) + "\n")
    files = sorted(path for path in root.rglob("*") if path.is_file())
    (root / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in files))
    return report


def main():
    """Capture into an explicit fresh directory, honoring the test artifact root."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir")
    parser.add_argument("--write-census", type=Path)
    args = parser.parse_args()
    if args.write_census:
        args.write_census.write_text(json.dumps(census(), indent=2, sort_keys=True) + "\n")
    destination = args.artifact_dir
    if destination is None and os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR"):
        destination = str(Path(os.environ["OPEN64_DSL_TEST_ARTIFACT_DIR"]) / "building-blocks")
    if destination:
        print(json.dumps(write_artifacts(destination), indent=2))
    elif not args.write_census:
        parser.error("provide --artifact-dir or OPEN64_DSL_TEST_ARTIFACT_DIR")


if __name__ == "__main__":
    main()
