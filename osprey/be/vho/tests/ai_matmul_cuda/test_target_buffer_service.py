"""Compile the target-neutral buffer service and retain host-mock evidence."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    output = args.output_dir.resolve()
    output.mkdir(parents=True, exist_ok=False)
    binary = output / "target-buffer-test"
    command = [os.environ.get("CC", "cc"), "-std=c11", "-Wall", "-Wextra",
               "-Werror", "-I", str(REPO / "osprey/include"),
               str(REPO / "osprey/libopen64rt/target_buffer_service.c"),
               str(HERE / "test_target_buffer_service.c"),
               "-o", str(binary)]
    build = subprocess.run(command, text=True, stdout=subprocess.PIPE,
                           stderr=subprocess.STDOUT, check=False)
    (output / "build.log").write_text(build.stdout, encoding="utf-8")
    if build.returncode:
        raise RuntimeError("target-buffer host build failed")
    run = subprocess.run([str(binary)], text=True, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT, check=False)
    log = ("# What it summarizes: Invocation-level buffer binding, transfer, "
           "borrowed-device zero-copy, and failure cleanup.\n" + run.stdout)
    (output / "buffer-binding.log").write_text(log, encoding="utf-8")
    expected = ("host_binding events=AHAHADRRR allocations=3 transfers=3 "
                "releases=3", "borrowed_device events=III allocations=0 "
                "transfers=0 releases=0", "transfer_failure events=AHR "
                "allocations=1 transfers=1 releases=1",
                "allocation_failure events=AR allocations=1 transfers=0 "
                "releases=1", "release_retry events=ARR allocations=1 "
                "transfers=0 releases=2")
    if run.returncode or any(line not in log for line in expected):
        raise RuntimeError("target-buffer host contract failed")
    receipt = {
        "schema": "open64-target-buffer-host-v1",
        "source_sha256": sha256(REPO / "osprey/libopen64rt/target_buffer_service.c"),
        "header_sha256": sha256(REPO / "osprey/include/open64_target_buffer_service.h"),
        "host_binding_verified": True,
        "borrowed_device_no_copy_verified": True,
        "failure_cleanup_verified": True,
        "native_cuda_compiled": False,
        "gpu_execution_verified": False,
    }
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n",
                                         encoding="utf-8")
    files = sorted(path for path in output.iterdir() if path.is_file())
    (output / "SHA256SUMS").write_text("".join(
        f"{sha256(path)}  {path.name}\n" for path in files), encoding="utf-8")
    print(output)


if __name__ == "__main__":
    main()
