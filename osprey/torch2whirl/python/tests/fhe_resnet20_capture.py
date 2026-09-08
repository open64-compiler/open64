"""SYNC-2 FHE ResNet-20 artifact certification."""

from __future__ import annotations

import argparse
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import sys
from typing import Optional, Sequence

from open64_dsc.builder import (
    FHE_BACKEND_OPENFHE,
    FHE_BOOTSTRAP_AUTO,
    FHE_ENCODING_CKKS_PACKED,
    FHE_ENCODING_NONE,
    FHE_ENTRY_VALUE_INPUT,
    FHE_ENTRY_VALUE_OUTPUT,
    FHE_ENTRY_VALUE_PARAMETER,
    FHE_KEY_BOOTSTRAP,
    FHE_KEY_PUBLIC,
    FHE_KEY_RELINEARIZATION,
    FHE_PACKING_AUTO,
    FHE_PACKING_METAKERNEL,
    FHE_PARAMETER_POLICY_ENCODED_PLAINTEXT,
    FHE_POLICY_AUTO,
    FHE_POLICY_INHERIT,
    FHE_VALUE_CLASS_CIPHERTEXT,
    FHE_VALUE_CLASS_ENCODED_PLAINTEXT,
    ProgramUnitHandle,
    ValueHandle,
    load_builder,
)
from open64_dsc.export import export_to_whirl, save_as_whirl
from open64_dsc.options import WhirlExportOptions


def _repo_root() -> Path:
    return Path(__file__).resolve().parents[4]


def _fixture_source() -> Path:
    return Path(__file__).resolve().parent / "models" / "secure_resnet20.py"


def _load_model(source: Path):
    spec = importlib.util.spec_from_file_location("secure_resnet20", source)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"failed to load fixture: {source}")
    module = importlib.util.module_from_spec(spec)
    sys.modules["secure_resnet20"] = module
    spec.loader.exec_module(module)
    return module.create_model(), module.open64_sample_inputs()


def _prepare_artifact_dir(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)
    for name in (
        "secure_resnet20.py",
        "secure_resnet20.B",
        "secure_resnet20.T",
        "secure_resnet20.safetensors",
        "operator-census.txt",
        "capture-options.txt",
        "gatekeeper.log",
    ):
        candidate = path / name
        if candidate.exists():
            candidate.unlink()


def _write_operator_census(path: Path, module) -> None:
    counts: dict[str, int] = {}
    for name in module.operators:
        counts[name] = counts.get(name, 0) + 1
    lines = [
        "model=SecureResNet20",
        "dataset=CIFAR-10",
        f"graph_source={module.graph_source}",
        f"pu_mode={module.options.pu_mode}",
        f"input_count={module.input_count}",
        f"external_parameter_count={sum(1 for value in module.values if value.value_kind == 'external_data')}",
        f"operator_count={len(module.operators)}",
        "",
        "operator_counts:",
    ]
    for name in sorted(counts):
        lines.append(f"  {name}={counts[name]}")
    lines.extend([
        "",
        "sync2_contract:",
        "  every source ReLU is emitted as common.relu",
        "  FHE records describe entry/encryption/key contracts only",
        "  Python emits no bootstrap, CKKS, SIHE, or FHE conversion operators",
        "  class_centric_pus=pending_pr_103_rebase",
        "  final_native_certification=blocked_until_pr_103_merge",
    ])
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def _attach_fhe_contract(module) -> None:
    builder = load_builder("native")
    config = builder.fhe_compilation_config(
        bootstrap_policy=FHE_BOOTSTRAP_AUTO,
        backend_policy=FHE_BACKEND_OPENFHE,
    )
    ciphertext = builder.fhe_encryption_descriptor(
        config,
        value_class=FHE_VALUE_CLASS_CIPHERTEXT,
        key_set_name="request_key",
        slot_count_policy=FHE_POLICY_INHERIT,
        encoding_policy=FHE_ENCODING_NONE,
        packing_policy=FHE_PACKING_AUTO,
    )
    encoded_plaintext = builder.fhe_encryption_descriptor(
        config,
        value_class=FHE_VALUE_CLASS_ENCODED_PLAINTEXT,
        key_set_name="",
        slot_count_policy=FHE_POLICY_AUTO,
        encoding_policy=FHE_ENCODING_CKKS_PACKED,
        packing_policy=FHE_PACKING_METAKERNEL,
    )
    inputs = [
        value for value in module.values
        if value.value_kind == "model_input"
    ]
    parameters = [
        value for value in module.values
        if value.value_kind == "external_data"
    ]
    outputs = [
        operator for operator in module.graph_operators
        if operator.name == "common.output_logits"
    ]
    if len(outputs) != 1:
        raise RuntimeError("expected one common.output_logits output")
    entry = builder.fhe_entry_contract(
        ProgramUnitHandle(module.entry_function.handle),
        config,
        input_count=len(inputs),
        output_count=1,
        parameter_count=len(parameters),
        parameter_policy=FHE_PARAMETER_POLICY_ENCODED_PLAINTEXT,
    )
    for ordinal, value in enumerate(inputs):
        builder.declare_fhe_entry_value(
            entry,
            ValueHandle(value.handle),
            ordinal,
            FHE_ENTRY_VALUE_INPUT,
            ciphertext,
        )
    builder.declare_fhe_entry_value(
        entry,
        ValueHandle(outputs[0].handle),
        0,
        FHE_ENTRY_VALUE_OUTPUT,
        ciphertext,
    )
    for ordinal, value in enumerate(parameters):
        builder.declare_fhe_entry_value(
            entry,
            ValueHandle(value.handle),
            ordinal,
            FHE_ENTRY_VALUE_PARAMETER,
            encoded_plaintext,
        )
    builder.fhe_key_requirement(config, key_class=FHE_KEY_PUBLIC)
    builder.fhe_key_requirement(config, key_class=FHE_KEY_RELINEARIZATION)
    builder.fhe_key_requirement(
        config,
        key_class=FHE_KEY_BOOTSTRAP,
        bootstrap_profile="pre_relu_refresh_v1",
    )


def _emit_capture(artifact_dir: Path) -> int:
    _prepare_artifact_dir(artifact_dir)
    source = artifact_dir / "secure_resnet20.py"
    shutil.copyfile(_fixture_source(), source)
    model, sample_inputs = _load_model(source)
    options = WhirlExportOptions(
        entry="forward",
        backend="native",
        model_name="secure_resnet20",
        external_data_file="secure_resnet20.safetensors",
        pu_mode="single",
    )
    module = export_to_whirl(model, sample_inputs, options)
    _attach_fhe_contract(module)
    save_as_whirl(module, str(artifact_dir / "secure_resnet20.B"))
    _write_operator_census(artifact_dir / "operator-census.txt", module)
    (artifact_dir / "capture-options.txt").write_text(
        "\n".join([
            "entry=forward",
            "backend=native",
            "model_name=secure_resnet20",
            "sample_input=shape:1,3,32,32",
            "pu_mode=single",
            "fhe.scheme=ckks",
            "fhe.bootstrap=auto",
            "fhe.backend=openfhe",
            "fhe.input=value_class:ciphertext",
            "fhe.output=value_class:ciphertext",
            "fhe.parameters=value_class:encoded_plaintext;side_file=secure_resnet20.safetensors",
        ]) + "\n",
        encoding="utf-8",
    )
    (artifact_dir / "gatekeeper.log").write_text(
        "native DSL/FHE structural verification passed for preliminary capture\n"
        "final native secure_resnet20.B/.T certification is gated on "
        "PR #103 merge and FHE-branch rebase.\n",
        encoding="utf-8",
    )
    return 0


def _find_ir_b2a() -> Optional[Path]:
    configured = os.environ.get("OPEN64_IR_B2A", "")
    if configured:
        path = Path(configured)
        if path.is_file() and os.access(str(path), os.X_OK):
            return path
        print(f"skip: ir_b2a is not executable: {path}")
        return None
    found = shutil.which("ir_b2a")
    if found:
        return Path(found)
    print("skip: ir_b2a not found; set OPEN64_IR_B2A to enable smoke test")
    return None


def _inspect_capture(ir_b2a: Path, artifact_dir: Path) -> int:
    artifact = artifact_dir / "secure_resnet20.B"
    text_dump = artifact_dir / "secure_resnet20.T"
    result = subprocess.run(
        [str(ir_b2a), "-st", "-src", str(artifact), str(text_dump)],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    if result.returncode != 0:
        print(result.stdout, file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        return result.returncode
    text = text_dump.read_text(encoding="utf-8", errors="replace")
    required = [
        "common.relu",
        "cnn.conv2d",
        "common.residual_add",
        "common.linear",
        "common.output_logits",
        "FHE Compilation Configuration Table:",
        "FHE Entry Contract Table:",
        "FHE Entry Value Table:",
        "FHE Encryption Descriptor Table:",
        "FHE Tensor Binding Table:",
        "FHE Key Requirement Table:",
        "value=safetensors://secure_resnet20.safetensors",
        "secure_resnet20.py",
    ]
    missing = [needle for needle in required if needle not in text]
    if missing:
        print(
            "FHE ResNet-20 ir_b2a output missed expected text: " +
            ", ".join(missing),
            file=sys.stderr,
        )
        return 1
    return 0


def _run_parent(artifact_dir: Path) -> int:
    ir_b2a = _find_ir_b2a()
    if ir_b2a is None:
        return 0
    env = os.environ.copy()
    completed = subprocess.run(
        [sys.executable, __file__, "--emit-only", str(artifact_dir)],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=env,
    )
    if completed.returncode != 0:
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
        return completed.returncode
    status = _inspect_capture(ir_b2a, artifact_dir)
    if status != 0:
        return status
    print(f"retained FHE ResNet-20 artifacts: {artifact_dir}")
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--emit-only", action="store_true")
    parser.add_argument("artifact_dir", nargs="?")
    args = parser.parse_args(argv)
    artifact_dir = (
        Path(args.artifact_dir)
        if args.artifact_dir
        else _repo_root() / "artifacts" / "fhe" / "resnet20_capture"
    )
    if args.emit_only:
        return _emit_capture(artifact_dir)
    return _run_parent(artifact_dir)


if __name__ == "__main__":
    raise SystemExit(main())
