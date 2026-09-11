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
            context.get("owner_pu_st"),
            context.get("source_relu_value_id"),
            context.get("context_pu_identity_id"),
            context.get("context_callsite_id"),
        )
        if (
            not isinstance(key[0], str)
            or any(not isinstance(value, int) for value in key[1:])
            or key in keys
        ):
            raise ManifestError("invalid or duplicate ReLU context identity")
        keys.add(key)
        if key[3] == 0:
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
        for field in (
            "dataset_sha256",
            "preprocessing_sha256",
            "trained_checkpoint_sha256",
        ):
            require_sha256(authority.get(field), field)
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
        results = manifest.get("results")
        if not isinstance(results, dict):
            raise ManifestError("accuracy results are absent")
        if manifest.get("failures") != []:
            raise ManifestError("accuracy run recorded policy failures")
        if not isinstance(results.get("sample_count"), int) or \
                results["sample_count"] <= 0:
            raise ManifestError("accuracy sample count is invalid")
        checks = (
            results.get("clear_baseline_top1_percent", -1.0) >=
                metrics["minimum_clear_top1_percent"],
            abs(
                results.get("clear_baseline_top1_percent", -1.0) -
                metrics["published_ace_clear_reference_percent"]
            ) <= metrics["maximum_clear_reference_difference_points"],
            results.get("polynomial_top1_drop_points", float("inf")) <=
                metrics["maximum_polynomial_top1_drop_points"],
            results.get("prediction_agreement_percent", -1.0) >=
                metrics["minimum_prediction_agreement_percent"],
            results.get("out_of_range_value_count", -1) <=
                metrics["maximum_out_of_range_values"],
        )
        if not all(checks):
            raise ManifestError("accuracy result violates a predeclared gate")


def validate_ckks(manifest: dict, require_ready: bool = False) -> None:
    if manifest.get("schema") != "open64.fhe.relu.ckks-schedule.v1":
        raise ManifestError("unknown CKKS schedule schema")
    schedule = manifest.get("profile_schedule")
    if not isinstance(schedule, list) or len(schedule) != 5:
        raise ManifestError("CKKS schedule must cover five ordered steps")
    if [step.get("ordinal") for step in schedule] != list(range(5)):
        raise ManifestError("CKKS schedule ordinals are not dense")
    if sum(step.get("level_consumption", -1) for step in schedule) != 11:
        raise ManifestError("CKKS schedule does not prove depth 11")
    contexts = manifest.get("contexts")
    if not isinstance(contexts, list) or len(contexts) != 19:
        raise ManifestError("CKKS schedule must cover 19 ReLU contexts")
    names = [context.get("instance_path") for context in contexts]
    if len(set(names)) != 19:
        raise ManifestError("CKKS context schedule contains duplicates")
    levels = [context.get("post_refresh_level") for context in contexts]
    if set(levels) != {15, 17, 18}:
        raise ManifestError("CKKS context levels do not match ACE")
    if any(
        context.get("final_level") != context.get("post_refresh_level") - 11
        for context in contexts
    ):
        raise ManifestError("CKKS context depth transition is inconsistent")
    if require_ready:
        if manifest.get("status") != "approved_static_compiler_schedule":
            raise ManifestError("CKKS state proof is not approved")
        config = manifest.get("selected_fhe_config", {})
        expected_config = {
            "scheme": "CKKS",
            "security_level": "128_classic",
            "ring_dimension": 65536,
            "slot_count": 32768,
            "multiplicative_depth": 33,
            "first_modulus_bits": 60,
            "scaling_modulus_bits": 56,
            "bootstrap_policy": "auto_or_on",
        }
        if any(config.get(key) != value for key, value in expected_config.items()):
            raise ManifestError("concrete OpenFHE configuration is incomplete")
        fields = ("input_level_offset", "output_level_offset", "scale_bits",
                  "component_count", "minimum_precision_bits",
                  "evaluation_algorithm", "level_consumption")
        if any(step.get(field) is None for step in schedule for field in fields):
            raise ManifestError("CKKS schedule transition is incomplete")
        contract = manifest.get("context_state_contract", {})
        if contract.get("state_role") != "POST_REFRESH" or \
                contract.get("state_version") != 1 or \
                contract.get("scale_bits") != 56 or \
                contract.get("component_count") != 2 or \
                contract.get("minimum_precision_bits") != 30 or \
                contract.get("pending_actions") != ["BOOTSTRAP"] or \
                contract.get("pending_bootstrap_reason") != \
                    "PRE_RELU_REFRESH":
            raise ManifestError("CKKS context-state contract is incomplete")


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

    def test_all_commit19_policy_evidence_gates_are_approved(self) -> None:
        validate_coefficients(self.coefficients, True)
        validate_ranges(self.ranges, True)
        validate_accuracy(self.accuracy, True)
        validate_ckks(self.ckks, True)

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

    def test_approved_range_manifest_without_measurements_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.ranges)
        malformed["contexts"][0]["bound_b"] = None
        with self.assertRaises(ManifestError):
            validate_ranges(malformed, require_ready=True)

    def test_approved_accuracy_manifest_with_threshold_failure_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.accuracy)
        malformed["results"]["prediction_agreement_percent"] = 97.9
        with self.assertRaises(ManifestError):
            validate_accuracy(malformed, require_ready=True)

    def test_ckks_context_level_mismatch_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.ckks)
        malformed["contexts"][0]["post_refresh_level"] = 16
        with self.assertRaises(ManifestError):
            validate_ckks(malformed, require_ready=True)

    def test_ckks_depth_mismatch_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.ckks)
        malformed["profile_schedule"][4]["level_consumption"] = 2
        with self.assertRaises(ManifestError):
            validate_ckks(malformed, require_ready=True)

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
