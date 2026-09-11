from __future__ import annotations

import base64
import copy
import hashlib
import json
import struct
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[4]
MANIFEST_DIR = ROOT / "doc" / "fhe-policy" / "sync3-relu"


class ManifestError(ValueError):
    pass


def load_manifest(name: str) -> dict:
    with (MANIFEST_DIR / name).open("r", encoding="ascii") as stream:
        return json.load(stream)


def require_sha256(value: object, field: str) -> None:
    if not isinstance(value, str) or len(value) != 64:
        raise ManifestError(f"{field} must be a SHA-256 string")
    if any(ch not in "0123456789abcdef" for ch in value):
        raise ManifestError(f"{field} must use lowercase hexadecimal")


def validate_coefficients(manifest: dict, require_approval: bool = False) -> None:
    if manifest.get("schema") != "open64.fhe.relu.coefficients.v1":
        raise ManifestError("unknown coefficient schema")
    profile = manifest.get("profile", {})
    if profile.get("stage_degrees") != [7, 15, 13]:
        raise ManifestError("coefficient stage order must be 7,15,13")
    if profile.get("basis") != "chebyshev":
        raise ManifestError("coefficient basis must be Chebyshev")
    if profile.get("c0_convention") != (
        "direct_sum_c_i_times_T_i_no_half_weight"
    ):
        raise ManifestError("unsupported c0 convention")

    stages = manifest.get("stages")
    if not isinstance(stages, list) or len(stages) != 3:
        raise ManifestError("exactly three coefficient stages are required")
    bundle = bytearray()
    for ordinal, (stage, degree) in enumerate(zip(stages, [7, 15, 13])):
        if stage.get("ordinal") != ordinal or stage.get("degree") != degree:
            raise ManifestError("coefficient stage identity mismatch")
        decimals = stage.get("source_decimals")
        binary_hex = stage.get("binary64_hex")
        if not isinstance(decimals, list) or len(decimals) != degree + 1:
            raise ManifestError("coefficient cardinality mismatch")
        if not isinstance(binary_hex, list) or len(binary_hex) != degree + 1:
            raise ManifestError("binary64 cardinality mismatch")
        encoded = bytearray()
        for coefficient_ordinal, (decimal, expected_hex) in enumerate(
            zip(decimals, binary_hex)
        ):
            value = float(decimal)
            bits = struct.unpack("<Q", struct.pack("<d", value))[0]
            if f"{bits:016x}" != expected_hex:
                raise ManifestError("decimal and binary64 coefficient disagree")
            if coefficient_ordinal % 2 == 0 and bits != 0:
                raise ManifestError("candidate sign polynomial is not odd")
            encoded.extend(struct.pack("<d", value))
        if base64.b64encode(encoded).decode("ascii") != stage.get(
            "little_endian_bytes_base64"
        ):
            raise ManifestError("coefficient byte encoding mismatch")
        digest = hashlib.sha256(encoded).hexdigest()
        if digest != stage.get("coefficient_bytes_sha256"):
            raise ManifestError("coefficient stage checksum mismatch")
        bundle.extend(encoded)
    if hashlib.sha256(bundle).hexdigest() != profile.get(
        "coefficient_bundle_sha256"
    ):
        raise ManifestError("coefficient bundle checksum mismatch")

    provenance = manifest.get("provenance", {})
    require_sha256(provenance.get("source_sha256"), "source_sha256")
    if provenance.get("license_expression") != (
        "Apache-2.0 WITH LLVM-exception"
    ):
        raise ManifestError("coefficient license evidence is missing")
    if require_approval:
        approval = manifest.get("approval", {})
        if manifest.get("status") != "approved_empirical_ace":
            raise ManifestError("coefficient status is not approved")
        if not approval.get("coefficient_provenance_reviewed"):
            raise ManifestError("coefficient provenance is not approved")
        if not approval.get("license_provenance_reviewed"):
            raise ManifestError("coefficient license provenance is not approved")
        if not approval.get("native_profile_enablement_allowed"):
            raise ManifestError("native coefficient enablement is not approved")


def validate_ranges(manifest: dict, require_ready: bool = False) -> None:
    if manifest.get("schema") != "open64.fhe.relu.context-ranges.v1":
        raise ManifestError("unknown range schema")
    contexts = manifest.get("contexts")
    if not isinstance(contexts, list) or len(contexts) != 19:
        raise ManifestError("exactly 19 ReLU contexts are required")
    keys = set()
    root_count = 0
    for context in contexts:
        key = (
            context.get("source_relu_value_id"),
            context.get("context_pu_identity_id"),
            context.get("context_callsite_id"),
        )
        if any(not isinstance(value, int) for value in key) or key in keys:
            raise ManifestError("invalid or duplicate ReLU context identity")
        keys.add(key)
        if context["context_callsite_id"] == 0:
            root_count += 1
    if root_count != 1:
        raise ManifestError("one root and eighteen called contexts are required")
    if require_ready:
        if manifest.get("status") != "approved":
            raise ManifestError("range evidence is not approved")
        authority = manifest.get("calibration_authority", {})
        required = (
            "dataset_sha256",
            "sample_selection",
            "preprocessing_sha256",
            "trained_checkpoint_sha256",
            "bound_rule",
        )
        if any(not authority.get(field) for field in required):
            raise ManifestError("range calibration authority is incomplete")
        for context in contexts:
            if any(
                context.get(field) is None
                for field in ("observed_min", "observed_max", "bound_b")
            ):
                raise ManifestError("a ReLU context lacks measured range evidence")
            if context["sample_count"] <= 0 or context["bound_b"] <= 0:
                raise ManifestError("a ReLU context has an invalid range")


def validate_accuracy(manifest: dict, require_ready: bool = False) -> None:
    if manifest.get("schema") != "open64.fhe.relu.accuracy.v1":
        raise ManifestError("unknown accuracy schema")
    if require_ready:
        if manifest.get("status") != "approved":
            raise ManifestError("accuracy evidence is not approved")
        inputs = manifest.get("inputs", {})
        metrics = manifest.get("predeclared_metrics", {})
        for field in (
            "dataset_sha256",
            "preprocessing_sha256",
            "trained_checkpoint_sha256",
            "coefficient_manifest_sha256",
            "range_manifest_sha256",
            "software_lock_sha256",
        ):
            require_sha256(inputs.get(field), field)
        if any(value is None for value in metrics.values()):
            raise ManifestError("accuracy thresholds were not predeclared")
        if not isinstance(manifest.get("results"), dict):
            raise ManifestError("accuracy results are absent")


def validate_ckks(manifest: dict, require_ready: bool = False) -> None:
    if manifest.get("schema") != "open64.fhe.relu.ckks-schedule.v1":
        raise ManifestError("unknown CKKS schedule schema")
    symbolic = manifest.get("symbolic_contract", {})
    if symbolic.get("stage_degrees") != [7, 15, 13]:
        raise ManifestError("CKKS schedule stage order mismatch")
    schedule = manifest.get("schedule")
    if not isinstance(schedule, list) or len(schedule) != 5:
        raise ManifestError("CKKS schedule must cover five ordered steps")
    if require_ready:
        if manifest.get("status") != "approved":
            raise ManifestError("CKKS state proof is not approved")
        config = manifest.get("selected_fhe_config", {})
        if any(value is None for value in config.values()):
            raise ManifestError("concrete OpenFHE configuration is incomplete")
        fields = (
            "input_level",
            "output_level",
            "input_scale_bits",
            "output_scale_bits",
            "component_count",
            "minimum_precision_bits",
            "evaluation_algorithm",
            "level_consumption",
        )
        if any(step.get(field) is None for step in schedule for field in fields):
            raise ManifestError("CKKS schedule transition is incomplete")
        if sum(step["level_consumption"] for step in schedule) != 11:
            raise ManifestError("CKKS schedule does not prove depth 11")
        if manifest.get("depth_sum") != 11:
            raise ManifestError("CKKS depth sum is inconsistent")
        if not manifest.get("symbolic_proof"):
            raise ManifestError("symbolic CKKS proof is absent")
        if not manifest.get("executed_openfhe_evidence"):
            raise ManifestError("executed OpenFHE evidence is absent")


def validate_package_index(manifest: dict) -> None:
    if manifest.get("schema") != "open64.fhe.relu.policy-package.v1":
        raise ManifestError("unknown policy package schema")
    artifacts = manifest.get("artifacts")
    if not isinstance(artifacts, list) or len(artifacts) != 4:
        raise ManifestError("policy package must contain all four gates")
    for artifact in artifacts:
        path = ROOT / artifact.get("path", "")
        if not path.is_file():
            raise ManifestError("policy package artifact is missing")
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        if digest != artifact.get("sha256"):
            raise ManifestError("policy package artifact checksum mismatch")
    enablement = manifest.get("enablement", {})
    if enablement.get("allowed") is not False:
        raise ManifestError("incomplete policy package must fail closed")
    if enablement.get("diagnostic") != "CFHECNN-RELU-003":
        raise ManifestError("policy package has the wrong stable diagnostic")


class FHEReluPolicyManifestTest(unittest.TestCase):
    def setUp(self) -> None:
        self.coefficients = load_manifest("coefficient-manifest.json")
        self.ranges = load_manifest("range-manifest.json")
        self.accuracy = load_manifest("accuracy-manifest.json")
        self.ckks = load_manifest("ckks-schedule-manifest.json")
        self.package = load_manifest("package-index.json")

    def test_review_package_is_structurally_valid(self) -> None:
        validate_coefficients(self.coefficients)
        validate_ranges(self.ranges)
        validate_accuracy(self.accuracy)
        validate_ckks(self.ckks)
        validate_package_index(self.package)

    def test_only_coefficient_profile_gate_is_approved(self) -> None:
        validate_coefficients(self.coefficients, True)
        for validator, manifest in (
            (validate_ranges, self.ranges),
            (validate_accuracy, self.accuracy),
            (validate_ckks, self.ckks),
        ):
            with self.assertRaises(ManifestError):
                validator(manifest, True)

    def test_incomplete_coefficient_stage_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.coefficients)
        malformed["stages"].pop()
        with self.assertRaises(ManifestError):
            validate_coefficients(malformed)

    def test_mismatched_coefficient_checksum_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.coefficients)
        malformed["stages"][0]["coefficient_bytes_sha256"] = "0" * 64
        with self.assertRaises(ManifestError):
            validate_coefficients(malformed)

    def test_duplicate_context_identity_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.ranges)
        malformed["contexts"][1] = copy.deepcopy(malformed["contexts"][0])
        with self.assertRaises(ManifestError):
            validate_ranges(malformed)

    def test_ready_range_manifest_without_measurements_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.ranges)
        malformed["status"] = "approved"
        with self.assertRaises(ManifestError):
            validate_ranges(malformed, require_ready=True)

    def test_ready_accuracy_manifest_without_thresholds_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.accuracy)
        malformed["status"] = "approved"
        with self.assertRaises(ManifestError):
            validate_accuracy(malformed, require_ready=True)

    def test_ready_ckks_manifest_without_transitions_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.ckks)
        malformed["status"] = "approved"
        with self.assertRaises(ManifestError):
            validate_ckks(malformed, require_ready=True)

    def test_mismatched_package_artifact_hash_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.package)
        malformed["artifacts"][0]["sha256"] = "0" * 64
        with self.assertRaises(ManifestError):
            validate_package_index(malformed)


if __name__ == "__main__":
    unittest.main()
