"""Validation for the provider-independent SYNC-4 ReLU capability subset."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any, Dict, Iterable, List


SCHEMA = "open64.fhe.sync4.relu-provider-capability.v1"
STATUS = "approved_static_subset_evidence"
PROFILE = "ace.chebyshev.sign.7x15x13.depth11.v2"
ACE_REVISION = "fb76131171b9f82aa6387f84dd73684fba5277e8"
REQUIRED_PRIMITIVES = frozenset(
    {
        "bootstrap_to_target_level",
        "ciphertext_add",
        "ciphertext_multiply",
        "plaintext_multiply",
        "scalar_multiply",
        "rescale",
        "relinearize",
        "constant_encode",
        "ace_bsgs_chebyshev_addition_chain",
    }
)


class CapabilityManifestError(ValueError):
    """Raised when a provider manifest cannot admit the SYNC-4 subset."""


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise CapabilityManifestError(message)


def _lower_hex_digest(value: Any, field: str) -> str:
    _require(isinstance(value, str), f"{field} must be a string")
    _require(len(value) == 64, f"{field} must contain 64 hexadecimal digits")
    _require(value == value.lower(), f"{field} must be lowercase")
    _require(all(ch in "0123456789abcdef" for ch in value),
             f"{field} must be hexadecimal")
    return value


def _unique_strings(values: Any, field: str) -> List[str]:
    _require(isinstance(values, list) and values, f"{field} must be nonempty")
    _require(all(isinstance(value, str) and value for value in values),
             f"{field} must contain nonempty strings")
    _require(len(values) == len(set(values)), f"{field} contains duplicates")
    return values


def validate_manifest(manifest: Dict[str, Any]) -> None:
    _require(isinstance(manifest, dict), "manifest must be an object")
    _require(manifest.get("schema") == SCHEMA, "unsupported capability schema")
    _require(manifest.get("status") == STATUS, "capability evidence is unapproved")

    provider = manifest.get("provider")
    _require(isinstance(provider, dict), "missing provider")
    _require(provider.get("name") == "ace-ant", "unsupported provider")
    _require(provider.get("runtime") == "FHErt_ant", "unsupported runtime")
    _require(provider.get("revision") == ACE_REVISION,
             "provider revision does not match the pinned ACE revision")

    profile = manifest.get("profile")
    _require(isinstance(profile, dict), "missing profile")
    _require(profile.get("name") == PROFILE, "unsupported ReLU profile")
    _require(profile.get("stage_degrees") == [7, 15, 13],
             "composite stage order is invalid")
    _require(profile.get("total_multiplicative_depth") == 11,
             "composite depth is invalid")
    _require(profile.get("evaluation_scheme") ==
             "ace_bsgs_addition_chain",
             "composite evaluation scheme is not the pinned ACE algorithm")
    _lower_hex_digest(profile.get("coefficient_manifest_sha256"),
                      "coefficient_manifest_sha256")
    _lower_hex_digest(profile.get("coefficient_bundle_sha256"),
                      "coefficient_bundle_sha256")

    config = manifest.get("logical_configuration")
    _require(isinstance(config, dict), "missing logical configuration")
    _require(config.get("scheme") == "CKKS", "unsupported scheme")
    _require(config.get("ring_dimension") == 65536,
             "unsupported ring dimension")
    _require(config.get("slot_count", 0) >= 32768, "insufficient slot count")
    _require(config.get("multiplicative_depth", 0) >= 11,
             "insufficient multiplicative depth")
    _require(config.get("first_modulus_bits") == 60,
             "logical first modulus must be 60 bits")
    _require(config.get("scaling_modulus_bits") == 56,
             "logical scale must be 56 bits")
    _require(config.get("post_refresh_levels") == [15, 17, 18],
             "unsupported post-refresh levels")

    internal = manifest.get("provider_internal_configuration")
    _require(isinstance(internal, dict), "missing provider-internal configuration")
    _require(internal.get("first_modulus_bits") == 51 and
             internal.get("scaling_modulus_bits") == 46,
             "pinned generated provider parameters changed")
    _require(internal.get("meaning") ==
             "provider-internal emitted parameters; not the Open64 logical scale/state contract",
             "provider-internal parameters are not distinguished from logical state")

    primitives = set(_unique_strings(manifest.get("required_primitives"),
                                     "required_primitives"))
    missing = sorted(REQUIRED_PRIMITIVES - primitives)
    _require(not missing, "missing required primitives: " + ", ".join(missing))

    bootstrap = manifest.get("bootstrap_evidence")
    _require(isinstance(bootstrap, dict), "missing bootstrap evidence")
    _require(bootstrap.get("context_count") == 19,
             "bootstrap context count must be 19")
    _require(bootstrap.get("target_level_counts") ==
             {"15": 16, "17": 1, "18": 2},
             "bootstrap level distribution is invalid")

    evidence = manifest.get("source_evidence")
    _require(isinstance(evidence, list) and evidence,
             "source evidence must be nonempty")
    paths = []
    for index, record in enumerate(evidence):
        _require(isinstance(record, dict), f"source_evidence[{index}] is invalid")
        path = record.get("path")
        _require(isinstance(path, str) and path and not Path(path).is_absolute(),
                 f"source_evidence[{index}].path is invalid")
        paths.append(path)
        _lower_hex_digest(record.get("sha256"),
                          f"source_evidence[{index}].sha256")
    _require(len(paths) == len(set(paths)), "source evidence paths are duplicated")

    security = manifest.get("security_boundary")
    _require(isinstance(security, dict), "missing security boundary")
    _require(security.get("logical_materialization_requires_secret_key") is False,
             "logical materialization must not require a secret key")
    _require(security.get("runtime_server_secret_key_exclusion_certified") is False,
             "SYNC-4 must not claim the SYNC-6 server boundary")


def load_and_validate(path: Path) -> Dict[str, Any]:
    manifest = json.loads(path.read_text(encoding="utf-8"))
    validate_manifest(manifest)
    return manifest


def verify_source_evidence(manifest: Dict[str, Any], ace_root: Path) -> None:
    validate_manifest(manifest)
    for record in manifest["source_evidence"]:
        source = ace_root / record["path"]
        _require(source.is_file(), f"missing ACE evidence: {record['path']}")
        digest = hashlib.sha256(source.read_bytes()).hexdigest()
        _require(digest == record["sha256"],
                 f"ACE evidence hash mismatch: {record['path']}")
