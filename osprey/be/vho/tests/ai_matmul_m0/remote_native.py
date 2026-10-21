"""Run a prepared compiled m0 artifact on an SSH-accessible native host."""

from __future__ import annotations

import argparse
import hashlib
from pathlib import Path
import re
import subprocess


HOST = re.compile(r"[A-Za-z0-9_.-]+(?:@[A-Za-z0-9_.-]+)?\Z")
REMOTE_PATH = re.compile(r"/[A-Za-z0-9_./-]+\Z")


def execute(command: list[str]) -> None:
    subprocess.run(command, check=True)


def verify(root: Path) -> None:
    lines = (root / "SHA256SUMS").read_text(encoding="utf-8").splitlines()
    if not lines:
        raise RuntimeError("native artifact returned no checksum manifest")
    for line in lines:
        expected, relative = line.split("  ", 1)
        path = (root / relative).resolve()
        try:
            path.relative_to(root)
        except ValueError:
            raise RuntimeError(f"unsafe returned path: {relative}")
        if not path.is_file() or hashlib.sha256(path.read_bytes()).hexdigest() != expected:
            raise RuntimeError(f"returned file hash mismatch: {relative}")
    for relative in ("binary/matmul-m0", "binary/matmul.B",
                     "traces/matmul.T", "measurements/raw.csv", "receipt.json"):
        if not (root / relative).is_file():
            raise RuntimeError(f"native artifact omitted {relative}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir", type=Path, required=True)
    parser.add_argument("--host", required=True)
    parser.add_argument("--remote-dir", required=True,
                        help="new absolute path on the native host")
    args = parser.parse_args()
    if (not HOST.fullmatch(args.host) or
            not REMOTE_PATH.fullmatch(args.remote_dir) or
            ".." in Path(args.remote_dir).parts):
        parser.error("host or remote path contains unsupported characters")
    root = args.artifact_dir.resolve()
    if not (root / "native.json").is_file() or (root / "receipt.json").exists():
        parser.error("artifact must be prepared and not already measured")
    execute(["ssh", args.host,
             f"test ! -e {args.remote_dir} && mkdir -p {args.remote_dir}"])
    execute(["rsync", "-a", f"{root}/", f"{args.host}:{args.remote_dir}/"])
    try:
        execute(["ssh", args.host,
                 f"python3 {args.remote_dir}/run_native.py "
                 f"--artifact-dir {args.remote_dir}"])
    finally:
        execute(["rsync", "-a", f"{args.host}:{args.remote_dir}/", f"{root}/"])
    verify(root)
    print(f"retrieved native m0 evidence: {root}")


if __name__ == "__main__":
    main()
