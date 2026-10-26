"""Audit mandatory per-movement logical resource evidence in joint model 3."""

import unittest

from ai_opt_report import parse, verify_joint
import test_direct_staged_comparison as direct_tests


class MovementResourceTest(unittest.TestCase):
    def fixture(self):
        args = list(direct_tests.DirectComparisonTest().fixture())
        args[0]["model"] = 3
        resources = {
            row["id"]: dict(register_bytes=12, register_capacity=1020,
                shared_bytes=row["buffering"], shared_limit=49152,
                threads=256, thread_limit=1024, source_line=9)
            for row in args[1]
        }
        return args + [resources]

    def test_complete_resources_preserve_direct_winner(self):
        result = verify_joint(*self.fixture())
        self.assertEqual(result["chosen"]["kind"], "D")
        self.assertEqual(result["resources"][2]["shared_bytes"], 0)

    def test_missing_resource_row_is_rejected(self):
        args = self.fixture()
        del args[-1][1]
        with self.assertRaisesRegex(ValueError, "resources"):
            verify_joint(*args)

    def test_extra_resource_row_is_rejected(self):
        args = self.fixture()
        args[-1][99] = dict(args[-1][1])
        with self.assertRaisesRegex(ValueError, "coverage"):
            verify_joint(*args)

    def test_corrupt_resource_fields_are_rejected(self):
        for key in self.fixture()[-1][1]:
            with self.subTest(key=key):
                args = self.fixture()
                args[-1][1][key] += 1
                with self.assertRaisesRegex(ValueError, "resources"):
                    verify_joint(*args)

    def test_duplicate_resource_row_is_rejected(self):
        row = "AI_OPT 7 JOINT_RESOURCE 1 12 1020 1280 49152 256 1024 9\n"
        with self.assertRaisesRegex(ValueError, "duplicate"):
            parse(row + row)

    def test_legacy_trace_cannot_claim_new_resource_evidence(self):
        args = self.fixture()
        args[0]["model"] = 2
        with self.assertRaisesRegex(ValueError, "coverage"):
            verify_joint(*args)


if __name__ == "__main__":
    unittest.main()
