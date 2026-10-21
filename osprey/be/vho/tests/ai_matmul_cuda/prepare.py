"""Create a self-contained, non-overwriting CUDA validation artifact."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import tempfile


HERE = Path(__file__).resolve().parent
FAMILIES = {
    "cuda_64": (64, 64, 16, 8, 4),
    "cuda_128": (128, 128, 32, 8, 8),
}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def prepare(output: Path, family: str) -> Path:
    output = output.resolve()
    if output.exists():
        raise FileExistsError(f"artifact already exists: {output}")
    output.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=".ai-matmul-cuda-", dir=output.parent))
    try:
        source = stage / "source"
        source.mkdir()
        shape = FAMILIES[family]
        generated = HERE.joinpath("matmul_template.cu").read_text(encoding="utf-8")
        for key, value in zip(("BM", "BN", "BK", "TM", "TN"), shape):
            generated = generated.replace(f"@{key}@", str(value))
        cuda_file = source / "matmul.generated.cu"
        cuda_file.write_text(generated, encoding="utf-8")
        shutil.copy2(HERE / "run_gpu.py", stage / "run_gpu.py")
        plan = {
            "schema": "aio9-cuda-validation-plan-v1",
            "family": family,
            "cta_m": shape[0], "cta_n": shape[1], "cta_k": shape[2],
            "thread_m": shape[3], "thread_n": shape[4],
            "threads_per_cta": shape[0] // shape[3] * (shape[1] // shape[4]),
            "shared_ab_bytes_per_stage": (shape[0] * shape[2] + shape[2] * shape[1]) * 4,
            "buffer_stages": 1,
            "instruction_family": "scalar_fma",
            "movement": "cooperative_global_to_shared_synchronous",
            "plan_provenance": "explicit AIO-9-shaped family seed; not a selected compiler plan",
            "lowering_provenance": "standalone source template; not emitted from WHIRL",
        }
        (stage / "plan.json").write_text(json.dumps(plan, indent=2) + "\n", encoding="utf-8")
        (stage / "source.sha256").write_text(
            f"{sha256(cuda_file)}  source/matmul.generated.cu\n", encoding="utf-8"
        )
        stage.rename(output)
    except Exception:
        shutil.rmtree(stage)
        raise
    return output


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--family", choices=FAMILIES, default="cuda_64")
    args = parser.parse_args()
    print(prepare(args.output_dir, args.family))
