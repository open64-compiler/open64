"""Trace-disabled MMA2 controls: exact SIMT images, separate mapped reopen."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil

from test_executable_pipeline import run


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--current", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    records = []
    for label in ("direct", "staged", "large-tail"):
        source = args.current.resolve() / label
        target = root / label
        target.mkdir()
        shutil.copy2(source / "gemm.B", target / "gemm.B")
        commands = [json.loads(line) for line in (source / "commands.log").read_text().splitlines()]
        command = next(row for row in commands if Path(row[0]).name == "opencc")
        command.insert(-1, "-Wb,-DSL:ai_opt_trace=OFF")
        run(command, target, dict(os.environ), "compiler.log")
        image = target / "gemm.device.B"
        if image.read_bytes() != (source / image.name).read_bytes():
            raise ValueError("tracing changed the SIMT image")
        trace = target / "gemm.t"
        if trace.exists() and "AI_MMA_SHADOW" in trace.read_text():
            raise ValueError("trace-disabled shadow emitted rows")
        run([str(args.build_dir.resolve() / "osprey/targdir/ir_tools/ir_b2a"),
             "-st", "-src", image.name, "gemm.device.T"], target, dict(os.environ), "dump.log")
        records.append(dict(label=label, image_sha256=hashlib.sha256(image.read_bytes()).hexdigest(), shadow_rows=0))
    (root / "receipt.json").write_text(json.dumps(records, indent=2) + "\n")
    print("Trace off parity passed: 3 images unchanged, no shadow rows; independent -st -src reopen")


if __name__ == "__main__":
    main()
