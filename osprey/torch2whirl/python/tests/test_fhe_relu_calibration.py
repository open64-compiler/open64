from __future__ import annotations

import copy
import importlib.util
import json
import os
from pathlib import Path
import unittest

import torch

from open64_dsc.fhe_relu_calibration import (
    CalibrationManifestError,
    CalibrationSample,
    IdentityBoundReluCalibrationCollector,
    canonical_json_bytes,
    finalize_manifest,
    validate_calibration_manifest,
    validate_context_template,
)


ROOT = Path(__file__).resolve().parents[4]
POLICY_DIR = ROOT / "doc" / "fhe-policy" / "sync3-relu"
MODEL_SOURCE = Path(__file__).resolve().parent / "models" / "secure_resnet20.py"
COEFFICIENT_SHA256 = (
    "75132d449852303ec3e44e86c8a5b5ffc196c0643cf7fadff453d797c2266931"
)


def _load_model():
    spec = importlib.util.spec_from_file_location(
        "secure_resnet20_calibration_fixture", MODEL_SOURCE
    )
    if spec is None or spec.loader is None:
        raise RuntimeError("failed to load SecureResNet calibration fixture")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.create_model()


def _context_template() -> list[dict]:
    with (POLICY_DIR / "range-manifest.json").open("r", encoding="ascii") as stream:
        return json.load(stream)["contexts"]


def _samples() -> list[CalibrationSample]:
    first = torch.linspace(-1.0, 1.0, 3 * 32 * 32, dtype=torch.float32)
    second = torch.linspace(0.75, -0.5, 3 * 32 * 32, dtype=torch.float32)
    return [
        CalibrationSample("fixture-sample-000", (first.reshape(1, 3, 32, 32),)),
        CalibrationSample("fixture-sample-001", (second.reshape(1, 3, 32, 32),)),
    ]


def _authority() -> dict:
    return {
        "dataset_name": "open64-deterministic-range-machinery-fixture",
        "dataset_version": "v1",
        "dataset_split": "fixture-only-not-calibration",
        "dataset_files": ["generated:linspace-v1"],
        "dataset_sha256": "1" * 64,
        "sample_selection": "two declared synthetic tensors",
        "sample_seed": 0,
        "preprocessing": "identity float32 NCHW",
        "preprocessing_sha256": "2" * 64,
        "trained_checkpoint_identity": "none:deterministic-source-fixture",
        "trained_checkpoint_sha256": "3" * 64,
        "acceptance_data_role": "disjoint_held_out",
        "out_of_range_policy": "reject",
        "approval_identity": "focused-test-only",
    }


def _source_artifact() -> dict:
    return {
        "binary_whirl_sha256": (
            "7d6505f9f351f1983567c990ae4fad6779dd8441b79c22aae7b1d0bda6054d68"
        ),
        "model_source_sha256": (
            "1362bd7fe87a983f3842136ffb1d4cf3b59ec51309b96b49b17e1dd81c4c8e5e"
        ),
        "parameter_payload_sha256": (
            "b60445bd8373fab1c4d002603de0de6528bceeaa43b0d6ab4b38c8b2aa10032e"
        ),
    }


def _collect() -> dict:
    collector = IdentityBoundReluCalibrationCollector(
        _load_model(), _context_template(), safety_factor=1.125
    )
    return collector.collect(
        _samples(),
        profile_name="ace.chebyshev.sign.7x15x13.depth11.v1",
        coefficient_manifest_sha256=COEFFICIENT_SHA256,
        calibration_authority=_authority(),
        source_artifact=_source_artifact(),
    )


class FHEReluCalibrationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.template = _context_template()
        cls.manifest = _collect()

    def test_collects_exactly_nineteen_identity_bound_contexts(self) -> None:
        validate_calibration_manifest(
            self.manifest, expected_contexts=self.template
        )
        self.assertEqual(len(self.manifest["contexts"]), 19)
        self.assertEqual(self.manifest["collection"]["sample_count"], 2)
        self.assertEqual(
            sum(item["context_callsite_id"] == 0
                for item in self.manifest["contexts"]),
            1,
        )
        for context in self.manifest["contexts"]:
            self.assertEqual(context["sample_count"], 2)
            self.assertGreater(context["element_count"], 0)
            self.assertGreater(context["bound_b"], 0.0)
            self.assertGreaterEqual(
                context["bound_b"], context["observed_abs_max"]
            )
            self.assertEqual(context["outlier_count"], 0)

    def test_collection_is_byte_deterministic(self) -> None:
        repeated = _collect()
        self.assertEqual(
            canonical_json_bytes(self.manifest), canonical_json_bytes(repeated)
        )

    def test_duplicate_and_missing_contexts_are_rejected(self) -> None:
        duplicate = copy.deepcopy(self.template)
        duplicate[1] = copy.deepcopy(duplicate[0])
        with self.assertRaises(CalibrationManifestError):
            validate_context_template(duplicate)
        with self.assertRaises(CalibrationManifestError):
            validate_context_template(self.template[:-1])

    def test_identity_mismatch_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.manifest)
        malformed["contexts"][0]["source_relu_value_id"] += 1
        malformed = finalize_manifest(malformed)
        with self.assertRaises(CalibrationManifestError):
            validate_calibration_manifest(
                malformed, expected_contexts=self.template
            )

        malformed = copy.deepcopy(self.manifest)
        malformed["contexts"][0]["module_path"] = "layer1.0.relu"
        malformed = finalize_manifest(malformed)
        with self.assertRaises(CalibrationManifestError):
            validate_calibration_manifest(
                malformed, expected_contexts=self.template
            )

    def test_invalid_bounds_are_rejected(self) -> None:
        for value in (0.0, -1.0):
            malformed = copy.deepcopy(self.manifest)
            malformed["contexts"][0]["bound_b"] = value
            malformed = finalize_manifest(malformed)
            with self.assertRaises(CalibrationManifestError):
                validate_calibration_manifest(malformed)

    def test_manifest_hash_mismatch_is_rejected(self) -> None:
        malformed = copy.deepcopy(self.manifest)
        malformed["collection"]["sample_ids"][0] = "changed"
        with self.assertRaises(CalibrationManifestError):
            validate_calibration_manifest(malformed)

        malformed = copy.deepcopy(self.manifest)
        malformed["collection"]["sample_ids"].reverse()
        malformed = finalize_manifest(malformed)
        with self.assertRaises(CalibrationManifestError):
            validate_calibration_manifest(malformed)

    def test_unapproved_fixture_cannot_enable_binding(self) -> None:
        with self.assertRaises(CalibrationManifestError):
            validate_calibration_manifest(
                self.manifest, expected_contexts=self.template,
                require_approved=True
            )
        approved = copy.deepcopy(self.manifest)
        approved["status"] = "approved"
        approved = finalize_manifest(approved)
        with self.assertRaises(CalibrationManifestError):
            validate_calibration_manifest(
                approved, expected_contexts=self.template, require_approved=True
            )

        approved["calibration_authority"].update({
            "dataset_name": "CIFAR-10",
            "dataset_version": "reviewed-release",
            "dataset_split": "reviewed-calibration-split",
            "sample_selection": "immutable-reviewed-sample-list",
            "preprocessing": "reviewed-CIFAR-10-preprocessing",
            "trained_checkpoint_identity": "reviewed-resnet20-checkpoint",
            "approval_identity": "reviewer-record-1",
        })
        approved = finalize_manifest(approved)
        validate_calibration_manifest(
            approved, expected_contexts=self.template, require_approved=True
        )

    @classmethod
    def tearDownClass(cls) -> None:
        output = os.environ.get("FHE_RELU_CALIBRATION_OUTPUT", "")
        if output:
            path = Path(output)
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(canonical_json_bytes(cls.manifest))


if __name__ == "__main__":
    unittest.main()
