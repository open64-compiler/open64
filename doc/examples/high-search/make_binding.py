"""Documentation adapter for the experimental LNO High-search user guide.

Reuse the existing reference binding adapter with a real producer manifest and
native snapshot; never invent source/fact identities or select a schedule.
The compiler retains complete schema, ownership and numerical admission.
Design: doc/AI-COMPILER-HIGH-SEARCH-USER-GUIDE.md.
"""
import argparse
import json
from pathlib import Path
import sys


def unique_members(pairs):
    """Reject duplicate JSON names before a documentation input can lose them."""
    result = {}
    for name, value in pairs:
        if name in result:
            raise ValueError("duplicate JSON member: " + name)
        result[name] = value
    return result


def read_json(path):
    """Load standard JSON without collapsing duplicate keys in producer policy."""
    return json.loads(path.read_text(), object_pairs_hook=unique_members)


def main():
    """Write a complete v2 binding to a new path from caller-selected inputs.

    This reference tool copies snapshot/producer evidence and controls. It
    neither certifies their compatibility nor repairs rejected input; native
    compiler admission remains required. Existing output is never overwritten.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("manifest", "snapshot", "gpu", "model", "controls", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    args = parser.parse_args()
    source_root = Path(__file__).resolve().parents[3]
    sys.path.insert(0, str(source_root / "osprey/be/lno/tests"))
    from run_high_observer_review import binding

    manifest = read_json(args.manifest)
    gpu = read_json(args.gpu)
    model = read_json(args.model)
    controls = read_json(args.controls)
    result = binding(manifest, args.snapshot.read_text(), gpu, model)
    result["schema"] = "open64.lno-high-search.v2"
    result["search_controls"] = controls
    text = json.dumps(result, indent=2) + "\n"
    with args.output.open("x") as output:
        output.write(text)
    print(str(args.output.resolve()))


if __name__ == "__main__":
    main()
