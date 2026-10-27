"""Timing oracles and fail-closed tests for the read-only bubble experiment."""

from dataclasses import replace
import unittest
from unittest.mock import patch

from gemm_bubble import Graph, Profile, Recipe, Unknown, audit_timeline, compare, simulate


class BubbleTest(unittest.TestCase):
    def check(self, graph):
        result = simulate(graph)
        audit_timeline(graph, result)
        return result

    def test_load_use_slack(self):
        g = Graph()
        load = g.add("warp", "load", demands=(("lsu", 1, 1),), latency=10)
        independent = g.add("warp", "integer", latency=2)
        use = g.add("warp", "add", [(load, "done"), (independent, "done")],
                    (("f32:0", 1, 1),), 4, 0)
        r = self.check(g)
        self.assertEqual(r["timeline"], [(load, 0, 10), (independent, 1, 3), (use, 10, 14)])
        self.assertEqual(r["completion_cycles"], 14)
        self.assertEqual(r["wait_causes"]["load"], 8)

    def test_latency_is_not_initiation_interval(self):
        g = Graph()
        g.add("w0", "mul", demands=(("f32:0", 1, 1),), latency=10, sm=0)
        g.add("w1", "mul", demands=(("f32:0", 1, 1),), latency=10, sm=0)
        r = self.check(g)
        self.assertEqual([s for _, s, _ in r["timeline"]], [0, 1])
        self.assertEqual(r["completion_cycles"], 11)

    def test_other_warp_covers_wait(self):
        def graph(cover):
            g = Graph()
            load = g.add("w0", "load", latency=10, sm=0)
            g.add("w0", "add", [(load, "done")], (("f32:0", 1, 1),), 1, 0)
            if cover:
                for _ in range(10):
                    g.add("w1", "add", demands=(("f32:0", 1, 1),), latency=1, sm=0)
            return g
        blocked, covered = self.check(graph(False)), self.check(graph(True))
        self.assertEqual(blocked["completion_cycles"], covered["completion_cycles"])
        self.assertGreater(blocked["compute_issue_idle_cycles"], covered["compute_issue_idle_cycles"])
        self.assertEqual(covered["compute_issue_idle_cycles"], 0)

    def test_finite_bandwidth_and_queue(self):
        g = Graph()
        for actor in ("w0", "w1", "w2"):
            g.add(actor, "load", demands=(("memory", 4, 1), ("queue", 10, 2)), latency=10)
        r = self.check(g)
        self.assertEqual([s for _, s, _ in r["timeline"]], [0, 4, 10])
        self.assertEqual(r["completion_cycles"], 20)

    def test_accumulator_recurrence(self):
        g = Graph()
        first = g.add("w", "add", demands=(("f32:0", 1, 1),), latency=4, sm=0)
        g.add("w", "add", [(first, "done")], (("f32:0", 1, 1),), 4, 0)
        self.assertEqual(self.check(g)["completion_cycles"], 8)

    def test_reject_corrupt_timeline(self):
        g = Graph()
        load = g.add("w", "load", latency=10)
        use = g.add("w", "add", [(load, "done")], latency=4)
        r = self.check(g)
        r["timeline"][-1] = (use, 2, 6)
        with self.assertRaisesRegex(ValueError, "dependency"):
            audit_timeline(g, r)

    def test_reject_resource_overbooking(self):
        g = Graph()
        g.add("a", "mul", demands=(("f32:0", 1, 1),), latency=1, sm=0)
        g.add("b", "mul", demands=(("f32:0", 1, 1),), latency=1, sm=0)
        r = self.check(g)
        r["timeline"][1] = (1, 0, 1)
        with self.assertRaisesRegex(ValueError, "capacity"):
            audit_timeline(g, r)

    def test_reject_corrupt_wait_totals(self):
        g = Graph()
        g.add("a", "x", latency=5)
        r = self.check(g)
        r["actor_wait_cycles"] += 1
        with self.assertRaisesRegex(ValueError, "waits"):
            audit_timeline(g, r)

    def test_virtual_join_and_admission_are_not_actor_wait(self):
        g = Graph()
        load = g.add("warp0", "load", latency=20)
        join = g.add("virtual-join", "completion", [(load, "done")])
        g.add("next-cta-warp0", "entry", [(join, "done")], latency=1)
        result = self.check(g)
        self.assertEqual(result["actor_wait_cycles"], 0)
        self.assertEqual(result["completion_cycles"], 21)

    def test_bounds_and_dependencies(self):
        g = Graph(1)
        g.add("a", "x")
        with self.assertRaisesRegex(Unknown, "budget"):
            g.add("b", "x")
        with self.assertRaisesRegex(ValueError, "dependency"):
            Graph().add("a", "x", [(5, "done")])
        with self.assertRaisesRegex(ValueError, "latency"):
            Graph().add("a", "x", latency=-1)

    def test_profile_is_explicitly_synthetic(self):
        Profile().validate()
        for p in (replace(Profile(), provenance="measured"), replace(Profile(), memory_queue=0),
                  replace(Profile(), sms=True), replace(Profile(), base_residue_words=8)):
            with self.assertRaises(ValueError):
                p.validate()

    def fixture(self, kind="S1", k=8, smem="row_major", transpose=False):
        tile = dict(cta=(8, 32, 8), threads=32,
            grid=dict(problem=(8, 32, k), ctas=1, grid=(1, 1), k_iterations=(k + 7) // 8),
            layout=dict(thread_columns=32, a_layout="transposed_padded" if transpose else "row_major"),
            smem=dict(b_layout=smem))
        return tile, dict(kind=kind)

    def test_bank_layout_and_actual_work(self):
        tile, row = self.fixture()
        recipe = Recipe(tile, row, Profile(), 10000)
        self.assertEqual(recipe.shared_word(0, 1, 2), 10)
        graph = recipe.build()
        result = self.check(graph)
        self.assertEqual(sum(e.op == "mul" for e in graph.events), 64)
        self.assertEqual(sum(e.op == "add" for e in graph.events), 64)
        self.assertGreater(result["completion_cycles"], 0)

    def test_tma_reuse_waits_for_all_consumers(self):
        tile, row = self.fixture("T2", 32)
        graph = Recipe(tile, row, Profile(), 10000).build()
        self.check(graph)
        tma = [e for e in graph.events if e.op == "tma"]
        self.assertEqual(len(tma), 8)
        self.assertTrue(all(any(graph.events[d].op == "control" and phase == "done"
                               for d, phase in e.deps) for e in tma))
        waits = [e for e in graph.events if e.op == "wait"]
        self.assertEqual(len(waits), 4)
        self.assertTrue(all(sum(graph.events[d].op == "tma_fill" and phase == "done"
                                for d, phase in e.deps) == 2 for e in waits))
        self.assertEqual(sum(e.op == "tma_fill" for e in graph.events), 8)
        for transfer in tma[4:]:
            acquire = next(graph.events[d] for d, phase in transfer.deps
                           if graph.events[d].op == "control" and phase == "done")
            self.assertTrue(any(graph.events[d].op == "barrier" and phase == "done"
                                for d, phase in acquire.deps))

    def test_direct_has_no_smem_or_barrier(self):
        tile, row = self.fixture("D")
        graph = Recipe(tile, row, Profile(), 10000).build()
        self.check(graph)
        self.assertFalse(any(e.op in ("shared_load", "shared_store", "barrier", "tma") for e in graph.events))

    def test_determinism(self):
        tile, row = self.fixture()
        g = Recipe(tile, row, Profile(), 10000).build()
        self.assertEqual(self.check(g), self.check(g))

    def test_tail_masks_and_source_arithmetic_counts(self):
        tile, row = self.fixture("D")
        tile["grid"]["problem"] = (1, 1, 3)
        graph = Recipe(tile, row, Profile(), 10000).build()
        self.check(graph)
        self.assertEqual(sum(e.op == "mul" for e in graph.events), 3)
        self.assertEqual(sum(e.op == "add" for e in graph.events), 3)
        self.assertEqual(sum(e.op == "store" for e in graph.events), 1)

    def test_cta_admission_and_output_completion(self):
        tile, row = self.fixture("D")
        tile["grid"].update(problem=(16, 32, 8), ctas=2, grid=(2, 1))
        graph = Recipe(tile, row, replace(Profile(), sms=1), 10000).build()
        result = self.check(graph)
        ends = {i: end for i, _, end in result["timeline"]}
        starts = {i: start for i, start, _ in result["timeline"]}
        gate = next(i for i, e in enumerate(graph.events) if e.actor == "cta-end-0")
        first = next(i for i, e in enumerate(graph.events) if e.actor == "1:0")
        self.assertGreaterEqual(starts[first], ends[gate])
        self.assertEqual(result["completion_cycles"], max(ends.values()))

    def test_partial_comparison_cannot_name_winner(self):
        tile, row = self.fixture("D")
        tile["tile"] = 2
        rows = [dict(row, id=1, tile=2, source_line=7, total=1, legality="proven")]
        joint = dict(mode=dict(model=3), candidates=rows, selection=dict(selected=1), chosen=rows[0])
        with patch("gemm_bubble.parse", return_value=([tile], dict(joint=joint), {})):
            result = compare("fixture", [Profile()], budget=1)
        self.assertEqual(result["comparisons"][0]["outcome"], "incomplete")
        self.assertIsNone(result["comparisons"][0]["shadow"])
        self.assertEqual(result["comparisons"][0]["emitted"], 1)

    def test_duplicate_profiles_rejected(self):
        with patch("gemm_bubble.parse", return_value=([], dict(joint=dict(mode=dict(model=3))), {})):
            with self.assertRaisesRegex(ValueError, "duplicate"):
                compare("fixture", [Profile(), Profile()])


if __name__ == "__main__":
    unittest.main()
