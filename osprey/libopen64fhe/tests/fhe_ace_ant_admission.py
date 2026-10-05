#!/usr/bin/env python3
"""Audit the pinned ACE source before SYNC-6 provider implementation.

Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md and
doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md (S6-1). This is a source admission
probe, not an executable capability or security certification.
"""

import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys


PIN = "fb76131171b9f82aa6387f84dd73684fba5277e8"
RTLIB = "fhe-cmplr/rtlib"
EVIDENCE = (
    "include/rt_ant/rt_api.h",
    "include/rt_ant/ant_api.h",
    "ant/context/src/ckks_context.c",
    "ant/ckks/src/rtlib.c",
    "ant/include/ckks/key_gen.h",
    "ant/include/ckks/cipher.h",
)


def git(root, *args):
    """Read committed ACE objects, never silently audit dirty working files."""
    return subprocess.check_output(("git", "-C", str(root), *args))


def source_digest(root):
    """Hash a deterministic path/content inventory of the pinned rtlib tree."""
    names = git(root, "ls-tree", "-r", "--name-only", "HEAD", RTLIB).decode().splitlines()
    digest = hashlib.sha256()
    for name in names:
        blob = git(root, "show", "HEAD:" + name)
        digest.update(name.encode("utf-8") + b"\0")
        digest.update(hashlib.sha256(blob).digest())
    return digest.hexdigest(), len(names)


def audit(root):
    """Report exact-source evidence and fail closed on unproved import paths."""
    revision = git(root, "rev-parse", "HEAD").decode().strip()
    if revision != PIN:
        raise ValueError("ACE_REVISION_MISMATCH: expected " + PIN + ", got " + revision)
    dirty = git(root, "status", "--porcelain", "--", RTLIB).decode().strip()
    if dirty:
        raise ValueError("ACE_SOURCE_DIRTY: pinned rtlib worktree differs from commit")

    files = {}
    for relative in EVIDENCE:
        name = RTLIB + "/" + relative
        data = git(root, "show", "HEAD:" + name)
        if (root / name).read_bytes() != data:
            raise ValueError("ACE_SOURCE_MISMATCH: " + name)
        files[name] = hashlib.sha256(data).hexdigest()

    context = git(root, "show", "HEAD:" + RTLIB + "/ant/context/src/ckks_context.c")
    data_path = git(root, "show", "HEAD:" + RTLIB + "/ant/ckks/src/rtlib.c")
    required_anchors = {
        "context_generates_keypair": b"Alloc_ckks_key_generator(" in context,
        "context_constructs_decryptor": b"Alloc_ckks_decryptor(" in context,
        "dataset_input_encrypts": b"Encrypt_msg(ciph," in data_path,
        "dataset_output_decrypts": b"Decrypt(plain," in data_path,
    }
    if not all(required_anchors.values()):
        raise ValueError("ACE_AUDIT_ANCHOR_CHANGED: source requires fresh review")

    digest, count = source_digest(root)
    return {
        "schema": "open64.fhe.sync6.ace-source-admission.v1",
        "ace_revision": revision,
        "rtlib_source_sha256": digest,
        "rtlib_tracked_file_count": count,
        "evidence_file_sha256": files,
        "source_observations": required_anchors,
        "capabilities": {
            "evaluation_only_context_import": "not_demonstrated",
            "non_secret_keyset_import": "not_demonstrated",
            "ciphertext_import": "not_demonstrated",
            "ciphertext_export": "not_demonstrated",
            "arithmetic_and_bootstrap": "declared_not_executed_by_this_probe",
        },
        "admitted": False,
        "diagnostic": "CAPABILITY_MISSING: pinned ACE context generates a secret key and decryptor; evaluation-only import and ciphertext transport require a reviewed ACE patch/new pin",
    }


def main():
    """Write review evidence; optionally return a failing admission status."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ace-root", required=True, type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--require-admitted", action="store_true")
    args = parser.parse_args()
    try:
        report = audit(args.ace_root.resolve())
    except (OSError, subprocess.CalledProcessError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 2
    encoded = json.dumps(report, sort_keys=True, indent=2) + "\n"
    if args.output:
        args.output.write_text(encoded, encoding="utf-8")
    else:
        sys.stdout.write(encoded)
    return 3 if args.require_admitted and not report["admitted"] else 0


if __name__ == "__main__":
    sys.exit(main())
