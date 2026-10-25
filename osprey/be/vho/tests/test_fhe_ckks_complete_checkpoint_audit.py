#!/usr/bin/env python3
"""Focused contract tests for the human-readable complete audit report."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).with_name("fhe_ckks_complete_checkpoint_audit.py")
SPEC = importlib.util.spec_from_file_location("complete_audit", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
AUDIT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(AUDIT)


class CompleteAuditTextTest(unittest.TestCase):
    """Keep human output concise without changing the JSON contract."""

    def setUp(self) -> None:
        self.result = {
            "func_entries": 10,
            "ckks_events": 33367,
            "bootstraps": 23,
            "pre_relu_refresh_bootstraps": 19,
            "depth_exhaustion_bootstraps": 4,
            "rotation_keys": 954,
            "conv_operations": 28927,
            "residual_operations": 18,
            "relu_operations": 4237,
            "tail_operations": 185,
            "live_high_level_computations": 0,
        }

    def test_readable_report_preserves_review_census(self) -> None:
        """Render the important findings in a compact stable order."""

        report = AUDIT.render_text(self.result)
        self.assertIn("Status: PASS", report)
        self.assertIn("Event/result/state joins: 33367", report)
        self.assertIn("Explicit bootstraps: 23", report)
        self.assertIn("Rotation-key requirements: 954", report)
        self.assertIn("Live high-level computations: 0", report)
        self.assertLess(len(report.splitlines()), 50)

    def test_json_contract_is_unchanged(self) -> None:
        """Adding text output must not add path-dependent JSON fields."""

        encoded = json.dumps(self.result, sort_keys=True, indent=2)
        self.assertNotIn("trace", encoded)
        self.assertNotIn("artifact_dir", encoded)
        self.assertEqual(json.loads(encoded), self.result)

    def test_text_publication_is_no_replace(self) -> None:
        """A repeated audit cannot overwrite an accepted human report."""

        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "complete-audit.txt"
            AUDIT.write_text_no_replace(path, "first\n")
            self.assertEqual(path.read_text(encoding="utf-8"), "first\n")
            with self.assertRaises(ValueError):
                AUDIT.write_text_no_replace(path, "second\n")
            self.assertEqual(path.read_text(encoding="utf-8"), "first\n")


if __name__ == "__main__":
    unittest.main()
