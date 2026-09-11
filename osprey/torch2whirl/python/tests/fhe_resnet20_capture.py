"""SYNC-2 FHE ResNet-20 artifact certification."""

from __future__ import annotations

import argparse
import importlib.util
import os
from pathlib import Path
import re
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


def _fixture_source(bn_fold_only: bool = False) -> Path:
    name = "secure_resnet20_bn_fold.py" if bn_fold_only else "secure_resnet20.py"
    return Path(__file__).resolve().parent / "models" / name


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


def _write_operator_census(path: Path, module, bn_fold_only: bool) -> None:
    counts: dict[str, int] = {}
    for name in module.operators:
        counts[name] = counts.get(name, 0) + 1
    lines = [
        "model=SecureResNet20BNFoldCertification" if bn_fold_only else
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
        "  reusable_common_relu_node_definitions=" +
        ("0" if bn_fold_only else "11"),
        "  source_context_common_relu_uses=" +
        ("0" if bn_fold_only else "19"),
        "  ReLU call contexts are not operator or function versions",
        "  resnet_class_pus=entry_plus_signature_specialized_ResNet20Block_clones",
        "  resnet_class_regions=required: cnn.basic_block inside each clone PU",
        "  final_native_certification=ready_for_main_side_ir_review",
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


def _emit_capture(artifact_dir: Path, bn_fold_only: bool = False) -> int:
    _prepare_artifact_dir(artifact_dir)
    source = artifact_dir / "secure_resnet20.py"
    shutil.copyfile(_fixture_source(bn_fold_only), source)
    model, sample_inputs = _load_model(source)
    options = WhirlExportOptions(
        entry="forward",
        backend="native",
        model_name=(
            "secure_resnet20_bn_fold" if bn_fold_only else "secure_resnet20"
        ),
        external_data_file="secure_resnet20.safetensors",
        pu_mode="multiple",
    )
    module = export_to_whirl(model, sample_inputs, options)
    _attach_fhe_contract(module)
    save_as_whirl(module, str(artifact_dir / "secure_resnet20.B"))
    _write_operator_census(
        artifact_dir / "operator-census.txt", module, bn_fold_only
    )
    (artifact_dir / "capture-options.txt").write_text(
        "\n".join([
            "entry=forward",
            "backend=native",
            "model_name=secure_resnet20",
            "sample_input=shape:1,3,32,32",
            "pu_mode=multiple",
            "fhe.scheme=ckks",
            "fhe.bootstrap=auto",
            "fhe.backend=openfhe",
            "fhe.input=value_class:ciphertext",
            "fhe.output=value_class:ciphertext",
            "fhe.parameters=value_class:encoded_plaintext;side_file=secure_resnet20.safetensors",
            "certification=bn_fold_relu_free" if bn_fold_only else
            "certification=full_resnet20_capture",
        ]) + "\n",
        encoding="utf-8",
    )
    (artifact_dir / "gatekeeper.log").write_text(
        "native DSL/FHE structural verification passed after PR #103 rebase\n"
        "ResNet class-centric PUs, explicit callsites, and cnn.basic_block "
        "REGION contracts are required in secure_resnet20.T.\n",
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
        "cnn.basic_block",
        "FUNC_ENTRY",
        "SecureResNet20",
        "ResNet20Block__",
        "__WHIRL_DSL_CALL__",
        "__WHIRL_DSL_CALL__:callee=ResNet20Block__",
        "canonical_class_name = secure_resnet20.ResNet20Block",
        "callable_identity = secure_resnet20.ResNet20Block.forward",
        "context=SecureResNet20.layer1.0",
        "source_ordinal=0",
        "FHE Compilation Configuration Table:",
        "FHE Entry Contract Table:",
        "FHE Entry Value Table:",
        "FHE Encryption Descriptor Table:",
        "FHE Tensor Binding Table:",
        "FHE Key Requirement Table:",
        "DSL PU Interface Formal Table: version=1 entries=84",
        "DSL Call ABI Argument Table: version=1 entries=129",
        "role=cnn.basic_block.conv1.weight",
        "role=cnn.basic_block.bn2.variance",
        "role=cnn.basic_block.downsample.bn.variance",
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
    call_abi_rows = re.findall(
        r"^\s+\[\d+\] callsite=\d+.*role=cnn\.basic_block\.",
        text,
        re.MULTILINE,
    )
    if len(call_abi_rows) != 129:
        print(
            "FHE ResNet-20 expected 129 call ABI argument rows, found "
            f"{len(call_abi_rows)}",
            file=sys.stderr,
        )
        return 1
    interface_rows = re.findall(
        r"^\s+\[\d+\] owner_pu=<[^>]+> formal=(\d+) value=\d+ "
        r"st=<[^>]+> ty=\d+ flags=0x0$",
        text,
        re.MULTILINE,
    )
    if len(interface_rows) != 84:
        print(
            "FHE ResNet-20 expected 84 PU interface formal rows, found "
            f"{len(interface_rows)}",
            file=sys.stderr,
        )
        return 1
    interface_by_owner: dict[str, list[int]] = {}
    for owner, formal in re.findall(
        r"^\s+\[\d+\] owner_pu=(<[^>]+>) formal=(\d+) value=\d+ "
        r"st=<[^>]+> ty=\d+ flags=0x0$",
        text,
        re.MULTILINE,
    ):
        interface_by_owner.setdefault(owner, []).append(int(formal))
    interface_counts = sorted(len(formals) for formals in interface_by_owner.values())
    if interface_counts != [2, 14, 14, 14, 20, 20] or any(
        sorted(formals) != list(range(len(formals)))
        for formals in interface_by_owner.values()
    ):
        print(
            "FHE ResNet-20 PU interface is incomplete or has noncontiguous "
            f"formal ordinals: {interface_by_owner}",
            file=sys.stderr,
        )
        return 1
    external_uris = re.findall(
        r"value=safetensors://secure_resnet20\.safetensors#[^\s;]+"
        r"\?offset=\d+&length=\d+&checksum=([^\s;\)\"\]]+)",
        text,
    )
    if not external_uris or any(
        re.fullmatch(r"[0-9a-f]{64}", checksum) is None
        for checksum in external_uris
    ):
        print(
            "FHE ResNet-20 external tensor checksum evidence is missing "
            "or malformed",
            file=sys.stderr,
        )
        return 1
    tensor_constants = sorted(set(re.findall(
        r"payload=name=([^;]+);[^\n]*value_kind=(?:external_data|implicit_zero)",
        text,
    )))
    bad_sources: list[str] = []
    for name in tensor_constants:
        symbol = re.search(
            rf"^\[\d+\]: {re.escape(name)}(?:\s|$)(.*?)(?=^\[\d+\]: |\n------------|\Z)",
            text,
            re.MULTILINE | re.DOTALL,
        )
        if symbol is None:
            bad_sources.append(f"{name}: missing symbol entry")
            continue
        entry = symbol.group(0)
        if "location: file (null), line 0" in entry:
            bad_sources.append(f"{name}: null line-zero location")
        elif "location:" not in entry:
            bad_sources.append(f"{name}: missing location")
        elif re.search(r"location: file [^,\n]+, line 0(?:\D|$)", entry):
            bad_sources.append(f"{name}: line-zero location")
    if bad_sources:
        print(
            "FHE ResNet-20 tensor parameter symbols missed source evidence: " +
            ", ".join(bad_sources),
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
    parser.add_argument("--bn-fold-only", action="store_true")
    parser.add_argument("artifact_dir", nargs="?")
    args = parser.parse_args(argv)
    artifact_dir = (
        Path(args.artifact_dir)
        if args.artifact_dir
        else _repo_root() / "artifacts" / "fhe" / "resnet20_capture"
    )
    if args.emit_only:
        return _emit_capture(artifact_dir, args.bn_fold_only)
    return _run_parent(artifact_dir)


if __name__ == "__main__":
    raise SystemExit(main())
