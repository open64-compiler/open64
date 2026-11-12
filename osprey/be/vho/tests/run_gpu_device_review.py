"""Retain H1a production registry, real backend admission and generic parity.

Requires a configured Linux build and its native Python frontend binding on
PYTHONPATH. This exercises existing generic kernels; it claims no H20 resource
calibration or GPU execution. Design:
doc/AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]


def run(command, directory, expected=0):
    """Retain each real tool command/output; expected=None permits a refusal."""
    result = subprocess.run([str(x) for x in command], cwd=directory,
                            text=True, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, timeout=240)
    with (directory / "commands.log").open("a") as log:
        log.write(json.dumps(list(map(str, command))) + "\n")
    with (directory / "validation.log").open("a") as log:
        log.write(result.stdout)
    if expected is not None and result.returncode != expected:
        raise RuntimeError(f"exit {result.returncode}: {directory}/validation.log")
    return result


def main():
    """Run bounded option/ISA cases and preserve actual mapped parity evidence."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build, root = args.build_dir.resolve(), args.output_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    access = REPO / "osprey/common/targ_info/access"
    run([os.environ.get("CXX", "c++"), "-std=c++11", "-O2", "-I",
         REPO / "osprey/linux/include", "-I", REPO / "osprey/common/com",
         "-I", access, access / "tests/dsl_gpu_device_contract.cxx",
         access / "dsl_memory_hierarchy.cxx", "-o", root / "contract"], root)
    contract = run([root / "contract"], root).stdout
    if "72 checks" not in contract:
        raise RuntimeError("incomplete registry assertions")
    fixture = REPO / "osprey/be/vho/tests/ai_matmul_cuda/edge_matmul_fixture.py"
    run([sys.executable, fixture, root / "input.B", "3", "5", "7"], root)
    driver = build / "osprey/targdir/be/be"
    ir_b2a = build / "osprey/targdir/ir_tools/ir_b2a"
    base = [driver, "-fB,input.B", "-fo,result.o", "-ft,phase.t",
            "-PHASE:w=on:c=off:wpath=" + str(build / "osprey/targdir/wopt")]
    results = []
    generic = {}
    cases = [("generic", None, None), ("explicit-sm90", None, "sm_90"),
             ("generic-sm90a", None, "sm_90a")]
    for device in ("h20_96gb", "h20_141gb"):
        cases += [(device + "-implicit", device, None),
                  (device + "-explicit", device, "sm_90")]
    for label, device, arch in cases:
        case = root / label
        case.mkdir()
        shutil.copyfile(root / "input.B", case / "input.B")
        options = ([f"-DSL:gpu_device={device}"] if device else [])
        if arch:
            options.append(f"-DSL:gpu_arch={arch}")
        command = base + ["-O3", "-DSL:cuda_legacy_emit=OFF",
                          "-DSL:gemm_plan_search=joint",
                          "-DSL:cuda_whirl_output=selected.B"] + options + [fixture]
        output = run(command, case).stdout
        identity = [x for x in output.splitlines() if x.startswith("DSL_GPU_DEVICE 1 ")]
        if device:
            if len(identity) != 1 or f"device={device} " not in identity[0] or any(
                    x not in identity[0] for x in ("arch=sm_90 ",
                    "planning=generic-architecture-fallback", "device_resources=unbound",
                    "device_calibration=unbound")):
                raise RuntimeError("missing or misleading device identity")
        elif identity:
            raise RuntimeError("omitted device changed generic reporting")
        run([ir_b2a, "-st", "-src", "selected.B", "selected.T"], case)
        binary, trace = (case / "selected.B").read_bytes(), (case / "selected.T").read_bytes()
        if label == "generic":
            generic = dict(binary=binary, trace=trace)
        elif arch != "sm_90a" and (binary != generic["binary"] or trace != generic["trace"]):
            raise RuntimeError("identity-only selection changed generic mapped kernel")
        results.append(dict(case=label, accepted=True, parity=(arch != "sm_90a")))
    # Ordinary semantic lowering must also retain its accepted fallback path.
    fallback = []
    fallback_binary = None
    for device in (None, "h20_96gb", "h20_141gb"):
        case = root / ("o0-" + (device or "generic"))
        case.mkdir()
        shutil.copyfile(root / "input.B", case / "input.B")
        options = [f"-DSL:gpu_device={device}"] if device else []
        result = run(base + ["-O0"] + options + [fixture], case)
        shutil.copyfile(case / "result.O", case / "fallback.B")
        run([ir_b2a, "-st", "-src", "fallback.B", "fallback.T"], case)
        current = (case / "fallback.B").read_bytes()
        if fallback_binary is None:
            fallback_binary = current
        elif current != fallback_binary:
            raise RuntimeError("device identity changed O0 fallback WHIRL")
        fallback.append(dict(device=device, returncode=result.returncode))
    invalid = [("unknown", "h200_141gb", "sm_90", "001"),
               ("case", "H20_96GB", "sm_90", "001"),
               ("suffix", "h20_96gb_extra", "sm_90", "001"),
               ("empty", "", "sm_90", "001")]
    for device in ("h20_96gb", "h20_141gb"):
        invalid += [(device + "-" + arch, device, arch, code) for arch, code in (
            ("sm_100", "003"), ("sm_100a", "003"), ("sm_90a", "003"),
            ("m90", "002"), ("sm_90typo", "002"))]
    refusals = []
    for label, device, arch, code in invalid:
        case = root / ("reject-" + label)
        case.mkdir()
        prior = (root / "input.B").read_bytes() if label == "unknown" else None
        if prior is not None:
            (case / "forbidden.B").write_bytes(prior)
        command = base + ["-O3", f"-DSL:gpu_device={device}",
                          f"-DSL:gpu_arch={arch}",
                          "-DSL:cuda_whirl_output=forbidden.B", "missing-source.c"]
        result = run(command, case, expected=None)
        checkpoint = case / "forbidden.B"
        changed_output = (checkpoint.exists() if prior is None else
                          not checkpoint.exists() or checkpoint.read_bytes() != prior)
        if (result.returncode == 0 or f"DSL_GPU_DEVICE-{code}:" not in result.stdout
                or changed_output or "No such file" in result.stdout):
            raise RuntimeError(f"device refusal not early/atomic: {case}")
        refusals.append(label)
    report = dict(schema="open64.h1a-gpu-device-review.v1", status="PASS",
                  registry_checks=72, accepted=results, o0_fallback=fallback,
                  early_refusals=refusals, existing_checkpoint_preserved=True,
                  o0_fallback_byte_identical=True, two_phase_search_changed=False,
                  device_resources_bound=False, calibration=False, GPU=False)
    (root / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        f"{hashlib.sha256(p.read_bytes()).hexdigest()}  {p.relative_to(root)}\n"
        for p in sorted(root.rglob("*")) if p.is_file() and p.name != "SHA256SUMS"))
    print(f"H1a production option/registry and generic parity review passed: {root}")


if __name__ == "__main__":
    main()
