"""Native process-boundary certification for tiny Llama 2 decode."""

from __future__ import annotations

import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
from textwrap import dedent
from typing import Optional


def _require_torch() -> None:
    try:
        import torch  # noqa: F401
    except ImportError as exc:
        raise RuntimeError(
            "torch is required for llama2_decode_native_ir_tools_smoke.py"
        ) from exc


def _find_driver() -> Path:
    configured = os.environ.get("TORCH2WHIRL_DRIVER", "")
    if configured:
        path = Path(configured)
        if path.is_file() and os.access(str(path), os.X_OK):
            return path
        raise FileNotFoundError(f"torch2whirl driver is not executable: {path}")

    found = shutil.which("torch2whirl")
    if found:
        return Path(found)

    raise FileNotFoundError("torch2whirl driver not found")


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


def _test_root() -> Path:
    return Path(__file__).resolve().parent


def _pythonpath_env() -> dict[str, str]:
    env = os.environ.copy()
    pieces = [
        str(_test_root()),
        str(_test_root().parent),
    ]
    current = env.get("PYTHONPATH", "")
    if current:
        pieces.append(current)
    env["PYTHONPATH"] = os.pathsep.join(pieces)
    return env


def _run_driver(
    driver: Path,
    model_path: Path,
    artifact: Path,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            str(driver),
            str(model_path),
            "--entry",
            "forward",
            "--backend",
            "native",
            "--sample-input",
            "shape:1,3,224,224",
            "--output",
            str(artifact),
        ],
        check=False,
        env=_pythonpath_env(),
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )


def _write_bad_gqa_model(path: Path) -> None:
    path.write_text(
        dedent(
            """
            from models.llama2_decode_model import TinyLlama2DecodeConfig
            from models.llama2_decode_model import create_tiny_llama2_decode
            from models.llama2_decode_model import open64_sample_inputs


            def create_model():
                return create_tiny_llama2_decode(
                    TinyLlama2DecodeConfig(num_kv_heads=2)
                )
            """
        ).lstrip(),
        encoding="utf-8",
    )


def _check_failed_run_cleans_partial_artifact(
    driver: Path,
    work_dir: Path,
) -> int:
    bad_model = work_dir / "llama2_decode_bad_gqa.py"
    bad_artifact = work_dir / "llama2_decode_bad_gqa.B"
    bad_log = work_dir / "llama2_decode_bad_gqa.log"
    _write_bad_gqa_model(bad_model)
    if bad_artifact.exists():
        bad_artifact.unlink()

    completed = _run_driver(driver, bad_model, bad_artifact)
    bad_log.write_text(
        completed.stdout + completed.stderr,
        encoding="utf-8",
    )
    if completed.returncode == 0:
        print("driver accepted unsupported decode GQA profile", file=sys.stderr)
        return 1
    if bad_artifact.exists():
        print(
            "unsupported decode run left a partial .B artifact",
            file=sys.stderr,
        )
        return 1
    if "grouped-query attention" not in bad_log.read_text(encoding="utf-8"):
        print("unsupported decode diagnostic lost GQA context", file=sys.stderr)
        return 1
    return 0


def _inspect_artifact(ir_b2a: Path, artifact: Path, text_dump: Path) -> int:
    result = subprocess.run(
        [str(ir_b2a), "-st", "-src", str(artifact), str(text_dump)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if result.returncode != 0:
        print(result.stdout, file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        return result.returncode

    text = text_dump.read_text(encoding="utf-8", errors="replace")
    required = [
        'source files:\t1\t"',
        "llama2_decode_model.py",
        "FUNC_ENTRY",
        "contract=transformer.decode.v1",
        "contract=transformer.decoder_layer.v2",
        "OPR_DSLROTARYEMBEDDING # OPR_DSLROTARYEMBEDDING version=2",
        "OPR_DSLATTENTION # OPR_DSLATTENTION version=2",
        "kid3=input1",
        "attr.position_mode=explicit_operand",
        "attr.execution_mode=single_token_decode",
        "attr.mask_mode=implicit_prefix_causal",
        "attr.cache_mode=functional_append",
        "attr.cache_sequence_axis=2",
        "DSL Abstract State Table: version=1 states=4 effects=4",
        "name=layer0.key_cache kind=mutable_buffer",
        "name=layer0.value_cache kind=mutable_buffer",
        "name=layer1.key_cache kind=mutable_buffer",
        "name=layer1.value_cache kind=mutable_buffer",
        "kind=modify",
        "roles=0x4 flags=0x1b",
        "{line: 1/",
    ]
    missing = [needle for needle in required if needle not in text]
    if missing:
        print(
            "decode ir_b2a -st -src output missed expected text: " +
            ", ".join(missing),
            file=sys.stderr,
        )
        print(text, file=sys.stderr)
        return 1

    if re.search(
        r"FUNC_ENTRY <[^>]*TinyLlama2DecodeForCausalLM>",
        text,
    ) is None:
        print("decode trace retained a generic FUNC_ENTRY name", file=sys.stderr)
        return 1

    count_expectations = {
        "contract=transformer.decoder_layer.v2": 2,
        "OPR_DSLROTARYEMBEDDING # OPR_DSLROTARYEMBEDDING version=2": 4,
        "OPR_DSLATTENTION # OPR_DSLATTENTION version=2": 2,
        "roles=0x4 flags=0x1b": 4,
        " kind=modify ": 4,
    }
    for needle, expected in count_expectations.items():
        actual = text.count(needle)
        if actual != expected:
            print(
                f"decode trace expected {expected} occurrences of "
                f"{needle!r}, found {actual}",
                file=sys.stderr,
            )
            return 1

    state_names = re.findall(r"STATE \[\d+\] name=([^ ]+)", text)
    if state_names != [
        "layer0.key_cache",
        "layer0.value_cache",
        "layer1.key_cache",
        "layer1.value_cache",
    ]:
        print(f"unexpected decode state order: {state_names}", file=sys.stderr)
        return 1

    effects = re.findall(
        r"EFFECT \[(\d+)\] node=(\d+) state=(\d+) ordinal=(\d+) "
        r"kind=modify",
        text,
    )
    if effects != [
        ("1", "27", "1", "0"),
        ("2", "27", "2", "1"),
        ("3", "61", "3", "0"),
        ("4", "61", "4", "1"),
    ]:
        print(f"unexpected decode effect order: {effects}", file=sys.stderr)
        return 1

    layer0_region = re.search(
        r"REGION id=2 parent=1 depth=2 kind=0 "
        r"contract=transformer\.decoder_layer\.v2(?P<body>.*?)"
        r"REGION id=3 parent=1 depth=2 kind=0 "
        r"contract=transformer\.decoder_layer\.v2",
        text,
        re.DOTALL,
    )
    layer1_region = re.search(
        r"REGION id=3 parent=1 depth=2 kind=0 "
        r"contract=transformer\.decoder_layer\.v2(?P<body>.*?)\n\n",
        text,
        re.DOTALL,
    )
    if layer0_region is None or layer1_region is None:
        print("decode trace did not expose both layer-owned regions",
              file=sys.stderr)
        return 1
    if "layer_ordinal:0" not in layer0_region.group("body"):
        print("layer 0 region metadata missing", file=sys.stderr)
        return 1
    if "layer_ordinal:1" not in layer1_region.group("body"):
        print("layer 1 region metadata missing", file=sys.stderr)
        return 1

    forbidden = ["OPR_DSL ", "MDSL ", "OPC_MDSL"]
    exposed = [needle for needle in forbidden if needle in text]
    if exposed:
        print(
            "decode ir_b2a exposed physical DSL storage text: " +
            ", ".join(exposed),
            file=sys.stderr,
        )
        print(text, file=sys.stderr)
        return 1

    return 0


def _run_smoke(ir_b2a: Path, driver: Path, work_dir: Path) -> int:
    work_dir.mkdir(parents=True, exist_ok=True)
    model_path = _test_root() / "models" / "llama2_decode_model.py"
    artifact = work_dir / "llama2_decode.B"
    text_dump = work_dir / "llama2_decode.T"
    side_file = work_dir / "llama2_decode.safetensors"
    driver_log = work_dir / "llama2_decode_driver.log"
    for path in (
        artifact,
        text_dump,
        side_file,
        driver_log,
        work_dir / "llama2_decode_bad_gqa.B",
        work_dir / "llama2_decode_bad_gqa.log",
        work_dir / "llama2_decode_bad_gqa.py",
    ):
        if path.exists():
            path.unlink()

    completed = _run_driver(driver, model_path, artifact)
    driver_log.write_text(
        completed.stdout + completed.stderr,
        encoding="utf-8",
    )
    if completed.returncode != 0:
        print(completed.stdout, file=sys.stderr)
        print(completed.stderr, file=sys.stderr)
        return completed.returncode
    if not artifact.exists() or artifact.stat().st_size == 0:
        print("decode WHIRL artifact was not created", file=sys.stderr)
        return 1
    if not side_file.exists() or side_file.stat().st_size == 0:
        print("decode safetensors side file was not created", file=sys.stderr)
        return 1

    inspect_status = _inspect_artifact(ir_b2a, artifact, text_dump)
    if inspect_status != 0:
        return inspect_status

    failure_status = _check_failed_run_cleans_partial_artifact(
        driver,
        work_dir,
    )
    if failure_status != 0:
        return failure_status

    print(f"retained Llama decode artifacts: {work_dir}")
    return 0


def main() -> int:
    ir_b2a = _find_ir_b2a()
    if ir_b2a is None:
        return 0
    _require_torch()
    driver = _find_driver()

    artifact_dir = os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR", "")
    if artifact_dir:
        status = _run_smoke(ir_b2a, Path(driver), Path(artifact_dir))
    else:
        with tempfile.TemporaryDirectory(
            prefix="torch2whirl-llama2-decode-native-"
        ) as tmp:
            status = _run_smoke(ir_b2a, Path(driver), Path(tmp))
    if status != 0:
        return status

    print("Llama decode native ir_b2a smoke passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
