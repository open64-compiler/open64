"""Replay authenticated compiler artifacts through the read-only bubble model.

Design: doc/AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md. This downstream experiment
does not rebuild or invalidate the compiler producer and never modifies its
immutable receipt. Preserve source, commands, inputs and timing evidence.
"""

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]


def digest(path):
    """Fingerprint retained evidence without relying on file names or timestamps."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    """Execute sequential replay/audit cases into a fresh checksummed directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--producer", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--profiles", type=Path, default=HERE / "bubble_profiles.json")
    args = parser.parse_args()
    producer, root = args.producer.resolve(), args.output.resolve()
    expected = {}
    for line in (producer / "SHA256SUMS").read_text().splitlines():
        value, name = line.split("  ", 1)
        expected[name] = value
    root.mkdir(parents=True, exist_ok=False)
    sources = ("gemm_bubble.py", "audit_bubble.py", "test_gemm_bubble.py",
               "run_bubble_experiment.py", "ai_opt_report.py")
    (root / "source").mkdir()
    for name in sources:
        shutil.copy2(HERE / name, root / "source" / name)
    shutil.copy2(args.profiles, root / "source/profiles.json")
    commands = []

    def run(command, log):
        """Record exact argv and combined output, failing the run on any error."""
        commands.append(command)
        result = subprocess.run(command, cwd=REPO, text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        log.write_text(result.stdout)
        (root / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if result.returncode:
            raise RuntimeError("command failed: " + str(command) + "\n" + result.stdout)
        return result.stdout

    run([sys.executable, "-m", "unittest", "discover", "-s", str(HERE), "-p", "test_*.py", "-v"],
        root / "unit-tests.log")
    summaries = []
    for case in ("direct", "direct-long-k", "tails", "multi-cta", "staged"):
        source = producer / "comparison" / case
        print("Replaying " + case, flush=True)
        retained = root / "producer" / case
        retained.mkdir(parents=True)
        names = ("gemm.t", "gemm.before.B", "gemm.before.T", "gemm.after.B", "gemm.after.T",
                 "gemm.before-after.diff", "gemm.whirl.cu", "commands.log", "host-sim.log")
        for name in names:
            path = source / name
            if expected.get(str(path.relative_to(producer))) != digest(path):
                raise ValueError("producer hash mismatch: " + str(path))
            shutil.copy2(path, retained / name)
        result = run([sys.executable, str(root / "source/gemm_bubble.py"),
            "--trace", str(retained / "gemm.t"), "--profiles", str(root / "source/profiles.json"),
            "--output", str(root / case), "--retain-events"], root / (case + ".log"))
        print(result, end="", flush=True)
        run([sys.executable, str(root / "source/audit_bubble.py"), str(root / case / "events")],
            root / (case + "-audit.log"))
        for name in names:
            if digest(source / name) != digest(retained / name):
                raise ValueError("producer changed during replay")
        data = json.loads((root / case / "comparison.json").read_text())
        summaries.extend(dict(case=case, **row) for row in data["comparisons"])
    receipt = dict(schema="open64-bubble-replay-v1", model="source_warp_v1",
        producer=str(producer), producer_manifest_sha256=digest(producer / "SHA256SUMS"),
        replay_sources={name: digest(root / "source" / name) for name in sources},
        profiles_sha256=digest(root / "source/profiles.json"), python=sys.version,
        comparisons=summaries, compiler_rebuilt=False, compiler_selection_changed=False,
        native_gpu_verified=False, limitations="synthetic source-warp timing; no SASS, cache or native occupancy calibration")
    (root / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        digest(path) + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print("PASS: five authenticated compiler cases; independent event audits; producer unchanged", flush=True)


if __name__ == "__main__":
    main()
