import copy
import json
import math
import struct
import tempfile
import unittest
from pathlib import Path

from fhe_sync4_relu_materialization import (
    MaterializationError,
    build_schedule,
    write_canonical,
)


REPO_ROOT = Path(__file__).resolve().parents[4]
POLICY3 = REPO_ROOT / "doc/fhe-policy/sync3-relu"
POLICY4 = REPO_ROOT / "doc/fhe-policy/sync4-relu"


def build(mode="auto", existing=None):
    return build_schedule(
        POLICY3 / "range-manifest.json",
        POLICY3 / "ckks-schedule-manifest.json",
        POLICY3 / "coefficient-manifest.json",
        POLICY4 / "ace-ant-subset-capability.json",
        mode,
        existing,
    )


class FHESync4ReluMaterializationTest(unittest.TestCase):
    @staticmethod
    def _coefficients():
        manifest = json.loads(
            (POLICY3 / "coefficient-manifest.json").read_text(encoding="utf-8")
        )
        return [
            [struct.unpack(">d", bytes.fromhex(value))[0]
             for value in stage["binary64_hex"]]
            for stage in manifest["stages"]
        ]

    @staticmethod
    def _direct_chebyshev(coefficients, value):
        terms = [1.0]
        if len(coefficients) > 1:
            terms.append(value)
        while len(terms) < len(coefficients):
            terms.append(2.0 * value * terms[-1] - terms[-2])
        return sum(coefficient * term
                   for coefficient, term in zip(coefficients, terms))

    @staticmethod
    def _clenshaw_chebyshev(coefficients, value):
        next_value = 0.0
        next_next = 0.0
        for coefficient in reversed(coefficients[1:]):
            current = coefficient + 2.0 * value * next_value - next_next
            next_next = next_value
            next_value = current
        return coefficients[0] + value * next_value - next_next

    def test_auto_materializes_all_contexts(self) -> None:
        schedule = build()
        self.assertEqual(schedule["context_count"], 19)
        self.assertEqual(schedule["operation_count"], 114)
        self.assertEqual(schedule["refresh_level_counts"],
                         {"15": 16, "17": 1, "18": 2})
        grouped = {}
        for operation in schedule["operations"]:
            grouped.setdefault(operation["instance_path"], []).append(operation)
        self.assertEqual(len(grouped), 19)
        for operations in grouped.values():
            self.assertEqual(
                [value["operation_ordinal"] for value in operations],
                list(range(6)),
            )
            self.assertEqual(
                [value["operation_kind"] for value in operations],
                [
                    "refresh",
                    "normalize",
                    "approx_stage",
                    "approx_stage",
                    "approx_stage",
                    "reconstruct_relu",
                ],
            )
            self.assertEqual(
                [value.get("degree") for value in operations[2:5]], [7, 15, 13]
            )
            self.assertEqual(
                [value["level_consumption"] for value in operations[2:]],
                [3, 4, 4, 0],
            )
            self.assertEqual(
                operations[4]["output_level"], operations[5]["output_level"]
            )

    def test_on_matches_auto_except_mode(self) -> None:
        auto = build("auto")
        on = build("on")
        for schedule in (auto, on):
            for operation in schedule["operations"]:
                operation.pop("bootstrap_mode", None)
            schedule.pop("bootstrap_mode")
        self.assertEqual(auto, on)

    def test_manual_validates_complete_existing_schedule_without_insertion(self) -> None:
        existing = build()
        existing["bootstrap_mode"] = "manual"
        existing["status"] = "persisted_complete_schedule"
        for operation in existing["operations"]:
            if operation["operation_kind"] == "refresh":
                operation["bootstrap_mode"] = "manual"
        snapshot = copy.deepcopy(existing)
        manual = build("manual", existing)
        self.assertEqual(manual["operation_count"], 114)
        self.assertEqual(manual, snapshot)

        incomplete = copy.deepcopy(existing)
        incomplete["operations"].pop()
        with self.assertRaisesRegex(MaterializationError, "CFHEMAT-RELU-004"):
            build("manual", incomplete)

    def test_manual_rejects_target_state_or_route_list_as_boundary_evidence(self) -> None:
        with self.assertRaisesRegex(MaterializationError, "CFHEMAT-RELU-004"):
            build("manual")
        with self.assertRaisesRegex(MaterializationError, "CFHEMAT-RELU-004"):
            build("manual", {"routes": ["stem.relu"]})

    def test_off_rejects_relu(self) -> None:
        with self.assertRaisesRegex(MaterializationError, "CFHEMAT-RELU-003"):
            build("off")

    def test_schedule_is_deterministic(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            first = Path(directory) / "first.json"
            second = Path(directory) / "second.json"
            write_canonical(build(), first)
            write_canonical(build(), second)
            self.assertEqual(first.read_bytes(), second.read_bytes())

    def test_shared_definitions_keep_distinct_context_policy(self) -> None:
        schedule = build()
        contexts = [
            operation for operation in schedule["operations"]
            if operation["operation_kind"] == "normalize" and
            operation["source_relu_value_id"] == 42
        ]
        self.assertEqual(len(contexts), 3)
        self.assertEqual(len({item["context_callsite_id"] for item in contexts}), 3)
        self.assertGreater(len({item["positive_bound_b"] for item in contexts}), 1)

    def test_composite_relu_matches_independent_clear_oracle(self) -> None:
        stages = self._coefficients()
        maximum_error = 0.0
        for index in range(20001):
            normalized = -1.0 + 2.0 * index / 20000.0
            direct = normalized
            clenshaw = normalized
            for coefficients in stages:
                direct = self._direct_chebyshev(coefficients, direct)
                clenshaw = self._clenshaw_chebyshev(coefficients, clenshaw)
            self.assertTrue(math.isfinite(clenshaw))
            self.assertAlmostEqual(direct, clenshaw, delta=2.0e-12)
            approximation = 0.5 * normalized * clenshaw + 0.5 * normalized
            maximum_error = max(
                maximum_error, abs(approximation - max(0.0, normalized))
            )
        self.assertLessEqual(maximum_error, 7.24e-4)


if __name__ == "__main__":
    unittest.main()
