#!/usr/bin/env python3
"""Select conservative FHE test lanes from the actual changed-file set.

The selector implements doc/IMPACTFUL-TEST-STRATEGY.md. It does not execute
tests or infer semantic safety from path names alone; it turns the reviewed
manifest into a visible test plan and escalates unknown production files.
"""

from __future__ import annotations

import argparse
import fnmatch
import json
import subprocess
import sys
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[4]
DEFAULT_MANIFEST = Path(__file__).with_name(
    "fhe_impactful_test_manifest.json"
)


def git_lines(arguments: list[str]) -> list[str]:
    """Return nonempty lines from one read-only Git query."""

    result = subprocess.run(
        ["git", *arguments],
        cwd=REPO_ROOT,
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    return [line for line in result.stdout.splitlines() if line]


def changed_files(base: str, head: str) -> list[str]:
    """Collect committed, working-tree, and untracked paths without duplicates."""

    paths = set(git_lines([
        "diff", "--name-only", "--diff-filter=ACMR", f"{base}...{head}"
    ]))
    paths.update(git_lines([
        "diff", "--name-only", "--diff-filter=ACMR"
    ]))
    paths.update(git_lines([
        "ls-files", "--others", "--exclude-standard"
    ]))
    return sorted(paths)


def load_manifest(path: Path) -> dict[str, Any]:
    """Load and minimally validate the dependency-free JSON policy."""

    manifest = json.loads(path.read_text(encoding="utf-8"))
    if manifest.get("schema_version") != 1:
        raise ValueError("unsupported impactful-test manifest schema")
    if not isinstance(manifest.get("lanes"), dict):
        raise ValueError("impactful-test manifest has no lanes")
    if not isinstance(manifest.get("rules"), list):
        raise ValueError("impactful-test manifest has no rules")
    return manifest


def pattern_matches(path: str, pattern: str) -> bool:
    """Match repository-relative POSIX paths using manifest glob syntax."""

    return fnmatch.fnmatchcase(path, pattern)


def select_lanes(files: list[str], manifest: dict[str, Any]) -> dict[str, Any]:
    """Return the union of matched lanes and conservative unknown escalation."""

    selected: set[str] = set()
    matched_rules: dict[str, list[str]] = {}
    unknown: list[str] = []
    for path in sorted(set(files)):
        matches: list[str] = []
        for rule in manifest["rules"]:
            if any(pattern_matches(path, pattern)
                   for pattern in rule["patterns"]):
                matches.append(rule["name"])
                selected.update(rule["lanes"])
        if matches:
            matched_rules[path] = matches
        else:
            unknown.append(path)
            selected.update(manifest["unknown_file_lanes"])
    lanes = []
    for name in sorted(selected):
        if name not in manifest["lanes"]:
            raise ValueError(f"rule selects unknown lane: {name}")
        lane = {"name": name, **manifest["lanes"][name]}
        lanes.append(lane)
    unavailable = [
        lane["name"] for lane in lanes if lane["status"] == "planned"
    ]
    return {
        "changed_files": sorted(set(files)),
        "matched_rules": matched_rules,
        "unknown_files": unknown,
        "lanes": lanes,
        "unavailable_required_lanes": unavailable,
    }


def print_text(plan: dict[str, Any]) -> None:
    """Print one review-friendly selection report."""

    print("Impactful FHE test selection")
    print(f"changed_files={len(plan['changed_files'])}")
    for path in plan["changed_files"]:
        rules = plan["matched_rules"].get(path, ["UNKNOWN"])
        print(f"  {path}: {','.join(rules)}")
    print("lanes:")
    for lane in plan["lanes"]:
        print(f"  [{lane['status']}] {lane['name']}: {lane['command']}")
    if plan["unknown_files"]:
        print("unknown files escalated to full certification:")
        for path in plan["unknown_files"]:
            print(f"  {path}")
    if plan["unavailable_required_lanes"]:
        print("required infrastructure gaps:")
        for lane in plan["unavailable_required_lanes"]:
            print(f"  {lane}")


def main() -> int:
    """Parse CLI arguments and emit the selected test plan."""

    parser = argparse.ArgumentParser()
    parser.add_argument("--base", default="origin/develop")
    parser.add_argument("--head", default="HEAD")
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--changed-file", action="append", default=[])
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--check-ready", action="store_true")
    arguments = parser.parse_args()

    try:
        manifest = load_manifest(arguments.manifest)
        files = arguments.changed_file or changed_files(
            arguments.base, arguments.head
        )
        plan = select_lanes(files, manifest)
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"impactful-test selector: {error}", file=sys.stderr)
        return 1
    if arguments.json:
        print(json.dumps(plan, indent=2, sort_keys=True))
    else:
        print_text(plan)
    if arguments.check_ready and plan["unavailable_required_lanes"]:
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
