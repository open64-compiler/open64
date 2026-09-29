import copy
import hashlib
import json
import os
import unittest
from pathlib import Path

from fhe_sync4_capability_manifest import (
    CapabilityManifestError,
    load_and_validate,
    validate_manifest,
    verify_source_evidence,
)


REPO_ROOT = Path(__file__).resolve().parents[4]
MANIFEST_PATH = (
    REPO_ROOT / "doc/fhe-policy/sync4-relu/ace-ant-subset-capability.json"
)
PACKAGE_PATH = REPO_ROOT / "doc/fhe-policy/sync4-relu/package-index.json"


class FHESync4CapabilityManifestTest(unittest.TestCase):
    def setUp(self) -> None:
        self.manifest = load_and_validate(MANIFEST_PATH)

    def rejected(self, mutate) -> None:
        candidate = copy.deepcopy(self.manifest)
        mutate(candidate)
        with self.assertRaises(CapabilityManifestError):
            validate_manifest(candidate)

    def test_approved_manifest(self) -> None:
        validate_manifest(self.manifest)

    def test_package_binds_exact_manifest_bytes(self) -> None:
        package = json.loads(PACKAGE_PATH.read_text(encoding="utf-8"))
        self.assertEqual(package["status"], "s4_2_static_subset_ready")
        self.assertEqual(len(package["artifacts"]), 1)
        artifact = package["artifacts"][0]
        self.assertEqual(
            artifact["path"],
            "doc/fhe-policy/sync4-relu/ace-ant-subset-capability.json",
        )
        self.assertEqual(
            artifact["sha256"], hashlib.sha256(MANIFEST_PATH.read_bytes()).hexdigest()
        )

    def test_bad_revision_rejected(self) -> None:
        self.rejected(lambda value: value["provider"].update(revision="0" * 40))

    def test_bad_hash_rejected(self) -> None:
        self.rejected(
            lambda value: value["profile"].update(
                coefficient_manifest_sha256="A" * 64
            )
        )

    def test_missing_bootstrap_rejected(self) -> None:
        self.rejected(
            lambda value: value.update(
                required_primitives=[
                    item for item in value["required_primitives"]
                    if item != "bootstrap_to_target_level"
                ]
            )
        )

    def test_unsupported_level_rejected(self) -> None:
        self.rejected(
            lambda value: value["logical_configuration"].update(
                post_refresh_levels=[15, 17]
            )
        )

    def test_insufficient_slots_rejected(self) -> None:
        self.rejected(
            lambda value: value["logical_configuration"].update(slot_count=16384)
        )

    def test_insufficient_depth_rejected(self) -> None:
        self.rejected(
            lambda value: value["logical_configuration"].update(
                multiplicative_depth=10
            )
        )

    def test_logical_and_internal_parameters_cannot_be_collapsed(self) -> None:
        self.rejected(
            lambda value: value["provider_internal_configuration"].update(
                first_modulus_bits=60, scaling_modulus_bits=56
            )
        )

    @unittest.skipUnless(os.environ.get("ACE_COMPILER_ROOT"),
                         "ACE_COMPILER_ROOT is not set")
    def test_pinned_ace_source_hashes(self) -> None:
        verify_source_evidence(
            self.manifest, Path(os.environ["ACE_COMPILER_ROOT"])
        )


if __name__ == "__main__":
    unittest.main()
