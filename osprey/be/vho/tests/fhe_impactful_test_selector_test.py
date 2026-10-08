#!/usr/bin/env python3
"""Focused contract tests for the FHE impactful-test selector."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path


SCRIPT = Path(__file__).with_name("fhe_impactful_test_selector.py")
SPEC = importlib.util.spec_from_file_location("impactful_selector", SCRIPT)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("could not load impactful-test selector")
SELECTOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(SELECTOR)


class ImpactfulTestSelectorTest(unittest.TestCase):
    """Prove narrow selection and conservative escalation boundaries."""

    @classmethod
    def setUpClass(cls) -> None:
        """Load the same checked-in manifest used by the CLI."""

        cls.manifest = SELECTOR.load_manifest(SELECTOR.DEFAULT_MANIFEST)

    def lanes(self, *paths: str) -> set[str]:
        """Return selected lane names for explicit deterministic paths."""

        plan = SELECTOR.select_lanes(list(paths), self.manifest)
        return {lane["name"] for lane in plan["lanes"]}

    def test_documentation_does_not_select_full_model(self) -> None:
        """A prose-only change requires hygiene and no producer replay."""

        self.assertEqual(self.lanes("doc/example.md"), {"hygiene"})

    def test_auditor_reuses_retained_artifact(self) -> None:
        """An auditor change selects cached evidence, not materialization."""

        lanes = self.lanes("osprey/be/vho/tests/example_audit.py")
        self.assertEqual(lanes, {"artifact-audit", "hygiene"})

    def test_pure_tail_plan_stays_focused(self) -> None:
        """A pure tail-plan change runs its oracle without full ResNet."""

        lanes = self.lanes("osprey/be/vho/fhe_ckks_tail_plan.cxx")
        self.assertEqual(lanes, {"hygiene", "tail-plan"})

    def test_materializer_requires_miniature_and_final_certification(self) -> None:
        """Executable mutation selects focused, miniature, and final gates."""

        lanes = self.lanes("osprey/be/vho/fhe_ckks_tail_materialize.cxx")
        self.assertIn("tail-plan", lanes)
        self.assertIn("miniature-checkpoint", lanes)
        self.assertIn("full-s6-0c-certification", lanes)

    def test_common_transaction_is_conservatively_broad(self) -> None:
        """A generic rewrite change reaches mapped, miniature, and full gates."""

        lanes = self.lanes("osprey/common/com/dsl_ir_rewrite.cxx")
        self.assertIn("common-ckks-transaction", lanes)
        self.assertIn("view-retirement", lanes)
        self.assertIn("mapped-reopen", lanes)
        self.assertIn("full-s6-0c-certification", lanes)

    def test_unknown_production_file_escalates(self) -> None:
        """An unclassified source file can never silently skip certification."""

        plan = SELECTOR.select_lanes(["osprey/be/vho/new_pass.cxx"],
                                     self.manifest)
        lanes = {lane["name"] for lane in plan["lanes"]}
        self.assertEqual(plan["unknown_files"],
                         ["osprey/be/vho/new_pass.cxx"])
        self.assertIn("full-s6-0c-certification", lanes)


if __name__ == "__main__":
    unittest.main()
