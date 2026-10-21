"""Upload a prepared CUDA artifact, run it over SSH, and retrieve evidence."""

from __future__ import annotations

import argparse
import hashlib
from pathlib import Path
import re
import subprocess


HOST = re.compile(r"[A-Za-z0-9_.-]+(?:@[A-Za-z0-9_.-]+)?\Z")
PATH = re.compile(r"/[A-Za-z0-9_./-]+\Z")


def execute(command: list[str]) -> None:
    subprocess.run(command, check=True)


def verify(root: Path) -> None:
    lines = (root / "SHA256SUMS").read_text(encoding="utf-8").splitlines()
    if not lines:
        raise RuntimeError("remote artifact returned no hashes")
    for line in lines:
        expected, relative = line.split("  ", 1)
        path = root / relative
        if not path.resolve().is_relative_to(root.resolve()) or not path.is_file():
            raise RuntimeError(f"missing or unsafe returned file: {relative}")
        if hashlib.sha256(path.read_bytes()).hexdigest() != expected:
            raise RuntimeError(f"returned file hash mismatch: {relative}")
    if not (root / "source/matmul.generated.cu").is_file():
        raise RuntimeError("remote artifact did not preserve the .cu source")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir", type=Path, required=True)
    parser.add_argument("--host", required=True, help="SSH host or user@host")
    parser.add_argument("--remote-dir", required=True, help="new absolute remote artifact path")
    parser.add_argument("--arch", default="sm_90", choices=("sm_90", "sm_100", "sm_120"))
    args = parser.parse_args()
    if not HOST.fullmatch(args.host) or not PATH.fullmatch(args.remote_dir) or \
            ".." in Path(args.remote_dir).parts:
        parser.error("host or remote path contains unsupported characters")
    root = args.artifact_dir.resolve()
    if not (root / "source.sha256").is_file() or (root / "receipt.json").exists():
        parser.error("artifact must be prepared and not already measured")
    # Only a new remote directory may be used; never replace prior evidence.
    execute(["ssh", args.host, f"test ! -e {args.remote_dir} && mkdir -p {args.remote_dir}"])
    execute(["rsync", "-a", f"{root}/", f"{args.host}:{args.remote_dir}/"])
    try:
        execute(["ssh", args.host,
                 f"python3 {args.remote_dir}/run_gpu.py --artifact-dir {args.remote_dir} "
                 f"--arch {args.arch}"])
    finally:
        # Keep failed build/run diagnostics and the original .cu locally.
        execute(["rsync", "-a", f"{args.host}:{args.remote_dir}/", f"{root}/"])
    verify(root)
    print(f"retrieved and verified artifact: {root}")


if __name__ == "__main__":
    main()
