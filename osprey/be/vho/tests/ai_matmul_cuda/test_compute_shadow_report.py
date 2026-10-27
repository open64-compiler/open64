"""MMA2 independent-reader failures and small graph sanity checks."""
import unittest

from compute_shadow_report import Graph, oracle_round, source_summary


def source_rows(arch=2, selected=7):
    return "\n".join(
        f"AI_MMA_SHADOW 1 SOURCE {profile} {arch} 1 "
        + ("numerical_contract_required" if arch == (2 if profile <= 2 else 4)
           else "exact_target_required")
        + f" unknown {selected} unknown {selected} none 27"
        for profile in range(1, 5))


class ComputeShadowReportTests(unittest.TestCase):
    def test_graph_precedence_and_service_not_added_twice(self):
        graph = Graph()
        operand = graph.event(10, 0)
        issue = graph.event(2, operand)
        service = graph.event(3, issue)
        latency = graph.event(20, operand)
        finish = graph.event(0, service, latency)
        self.assertEqual(graph.solve()[finish], 30)

    def test_one_atom_hand_schedule(self):
        geometry = [64, 64, 16, 64, 64, 16, 128, 4096, 27]
        model = [1, 1, 2, 2, 1, 2, 1, 1, 1, 1, 1, 0, 0, 1000, 27]
        rates = [4096, 4096, 131072, 1, 4096, 1, 1, 10, 2, 20, 3, 5, 0, 0, 0, 0]
        self.assertEqual(oracle_round(geometry, model, rates, 1), (59, 14))

    def test_unknown_preserves_source_and_selected_simt(self):
        for arch in (1, 2, 4):
            self.assertIn("timing=unknown", source_summary(source_rows(arch), 7))
        self.assertEqual(source_summary("historical AI_OPT trace", 7), "")

    def test_missing_duplicate_or_bad_source_rows_reject(self):
        rows = source_rows()
        for broken in (rows + "\n" + rows.splitlines()[0], "\n".join(rows.splitlines()[1:]),
                       rows.replace("none", "lowered"), rows.replace("unknown", "0"),
                       rows.replace("unknown 7 none", "unknown 8 none"),
                       rows.replace(" 1 numerical_contract", " 5 numerical_contract"),
                       rows.replace("numerical_contract_required", "exact_target_required")):
            with self.assertRaises(ValueError):
                source_summary(broken, 7)


if __name__ == "__main__":
    unittest.main()
