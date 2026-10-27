"""Independent small arithmetic/proof checks for the research-only milestone.

Controlling design: doc/AI-COMPILER-COMMON-EXECUTION-PLANNING.md.
These tests do not call or certify Open64 backend transformations.
"""

import copy
import json
import math
from pathlib import Path
import unittest

import research_demo as demo


class ResearchChecks(unittest.TestCase):
    def setUp(self):
        self.profile = json.loads(Path(__file__).with_name("synthetic_profile.json").read_text())

    def test_one_cta_hand_oracle(self):
        phase = demo.Phase("unit", read=64, write=32, alu=256, exp=16,
                           div=4, rsqrt=4, shared_traffic=128, barriers=2)
        plan = demo.Plan("unit", "unit", (demo.Kernel("unit", 1, 32, 1, 4, (phase,)),))
        result = demo.evaluate(plan, self.profile)
        self.assertEqual(result["components"], dict(compute=4, memory=47, synchronization=6, launch=40))
        self.assertEqual(result["total"], 97)

    def test_shared_rates_not_multiplied_by_residency(self):
        phase = demo.Phase("unit", read=64, alu=256)
        plan = demo.Plan("unit", "unit", (demo.Kernel("unit", 5, 32, 1, 0, (phase,)),))
        result = demo.evaluate(plan, self.profile)
        self.assertEqual(result["components"], dict(compute=2, memory=25, synchronization=0, launch=40))
        self.assertEqual(result["kernels"][0]["rounds"][0]["q"], 2)

    def test_reservation_rounding(self):
        plan = demo.Plan("unit", "unit", (demo.Kernel("unit", 1, 33, 9, 129, ()),))
        result = demo.evaluate(plan, self.profile)["kernels"][0]
        self.assertEqual(result["registers"], 512)
        self.assertEqual(result["shared"], 256)

    def test_zero_residency(self):
        profile = dict(self.profile, shared_bytes_per_sm=64)
        self.assertEqual(demo.evaluate(demo.plans()[0], profile)["reason"], "zero_residency")

    def test_register_guard(self):
        profile = dict(self.profile, registers_per_thread_limit=18)
        selected = {p.name: demo.evaluate(p, profile) for p in demo.plans() if p.case == "softmax"}
        self.assertFalse(selected["retain_row"]["legal"])
        self.assertTrue(selected["reread_row"]["legal"])

    def test_permission_guard(self):
        for plan in demo.plans():
            result = demo.evaluate(plan, self.profile, allow_algebra=False)
            self.assertEqual(result["legal"], not plan.reassociates)

    def test_invalid_profile(self):
        with self.assertRaises(ValueError):
            demo.evaluate(demo.plans()[0], dict(self.profile, alu_ops_per_sm_tick=0))

    def test_no_profile_mutation(self):
        before = copy.deepcopy(self.profile)
        for plan in demo.plans():
            demo.evaluate(plan, self.profile)
        self.assertEqual(before, self.profile)

    def test_no_dependency_is_not_reuse(self):
        self.assertFalse(demo.reuse_allowed({"done": [], "next": []}, ["done"], "next"))

    def test_all_consumers_must_complete(self):
        edges = {"read0_done": ["next"], "read1_done": [], "next": []}
        self.assertFalse(demo.reuse_allowed(edges, ["read0_done", "read1_done"], "next"))
        edges["read1_done"] = ["next"]
        self.assertTrue(demo.reuse_allowed(edges, ["read0_done", "read1_done"], "next"))

    def test_issue_is_not_access_completion(self):
        edges = {"issue": ["next", "read_done"], "next": [], "read_done": []}
        self.assertFalse(demo.reuse_allowed(edges, ["read_done"], "next"))

    def test_cycle_rejected(self):
        with self.assertRaises(ValueError):
            demo.reuse_allowed({"a": ["b"], "b": ["a"]}, ["a"], "b")

    def test_unknown_event_rejected(self):
        self.assertFalse(demo.reuse_allowed({"a": ["b"]}, ["unknown"], "b"))
        self.assertFalse(demo.reuse_allowed({"a": ["b"]}, [], "b"))

    def test_generation_requires_prior_read_completion(self):
        edges = {"read_g0": ["release_g0"], "release_g0": ["issue_g1"], "issue_g1": []}
        self.assertTrue(demo.reuse_allowed(edges, ["read_g0"], "issue_g1"))
        self.assertFalse(demo.reuse_allowed(edges, ["issue_g1"], "read_g0"))

    def test_statistics_merge(self):
        merged = demo.merge_statistics((2, 2.0, 2.0), (2, 6.0, 2.0))
        self.assertEqual(merged, (4, 4.0, 20.0))
        self.assertEqual(demo.merge_statistics((0, 0.0, 0.0), merged), merged)

    def test_shift_invariant_softmax(self):
        row = [0.0, 1.0, -2.0, 3.0]
        for retain in (False, True):
            x = demo.stable_softmax(row, retain)
            y = demo.stable_softmax([v + 10000 for v in row], retain)
            self.assertLess(max(abs(a - b) for a, b in zip(x, y)), 1e-12)
            self.assertAlmostEqual(math.fsum(x), 1.0)

    def test_host_semantic_oracles(self):
        result = demo.numerical_checks()
        self.assertEqual(len(result), 9)
        self.assertLessEqual(max(result.values()), 1e-5)

    def test_census_and_all_components(self):
        result = demo.run(self.profile)
        self.assertEqual(len(result["candidates"]), 32)
        self.assertEqual(len(result["decisions"]), 16)
        for row in result["candidates"]:
            cost = row["result"]
            self.assertEqual(cost["total"], sum(cost["components"].values()))
            self.assertTrue(cost["legal"])

    def test_ordinary_reduction_counts_workspace_and_second_launch(self):
        plan = next(p for p in demo.plans() if p.name == "split16")
        result = demo.evaluate(plan, self.profile)
        self.assertEqual(result["workspace_bytes"], 64)
        self.assertEqual(result["components"]["launch"], 80)

    def test_sensitivity_changes_are_not_kernel_name_rules(self):
        result = demo.run(self.profile)
        winners = {(d["scenario"], d["case"]): d["winner"] for d in result["decisions"]}
        self.assertEqual(winners["base", "normalization"], "centered")
        self.assertEqual(winners["memory_slow", "normalization"], "welford")
        self.assertEqual(winners["division_slow", "normalization"], "centered")
        self.assertEqual(winners["base", "reduction"], "split16")
        self.assertEqual(winners["launch_heavy", "reduction"], "one_cta")

    def test_base_results_match_published_research_table(self):
        expected = {"tile16": 7344, "tile32": 6120, "one_cta": 2168,
                    "split16": 719, "retain_row": 242, "reread_row": 428,
                    "centered": 2233, "welford": 2281}
        for plan in demo.plans():
            self.assertEqual(demo.evaluate(plan, self.profile)["total"], expected[plan.name])


if __name__ == "__main__":
    unittest.main()
