"""Exercise the production C++ analytical evaluator and independent report audit."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest

from readiness_report import (readiness_cost, legacy_performance, occupancy_record,
                              HW_RESOURCE_COLUMNS)
from ai_opt_report import verify_joint
import test_direct_staged_comparison

HERE = Path(__file__).resolve().parent


class ReadinessTest(unittest.TestCase):
    def test_production_header_against_literal_schedule(self):
        with tempfile.TemporaryDirectory(prefix="readiness-") as tmp:
            binary = str(Path(tmp) / "contract")
            subprocess.run([os.environ.get("CXX", "c++"), "-std=c++11", "-O2",
                            "-I", str(HERE.parents[3] / "common/com"),
                            "-I", str(HERE.parents[1]), str(HERE / "readiness_contract.cxx"),
                            "-o", binary], check=True, capture_output=True, text=True)
            result = subprocess.run([binary], check=True, capture_output=True, text=True, timeout=10)
            self.assertIn("2340 literal T2 oracles", result.stdout)

    def fixture(self):
        return dict(cta=(32, 32, 16), threads=128, source_line=7,
                    grid=dict(ctas=1, k_iterations=10, problem=(32, 32, 160), grid=(1, 1)),
                    gmem=dict(a=20480, b=20480, c=4096, a_issues=1280),
                    smem=dict(b_issues=1280, address=0), layout=dict(bank_work=0))

    def test_occupancy_production_against_literal_oracles(self):
        with tempfile.TemporaryDirectory(prefix="occupancy-") as tmp:
            binary = str(Path(tmp) / "contract")
            subprocess.run([os.environ.get("CXX", "c++"), "-std=c++11", "-O2",
                            "-I", str(HERE.parents[3] / "common/com"),
                            "-I", str(HERE.parents[1]), str(HERE / "occupancy_contract.cxx"),
                            "-o", binary], check=True, capture_output=True, text=True)
            result = subprocess.run([binary], check=True, capture_output=True, text=True, timeout=10)
            self.assertIn("5535 literal D/S1/T2 oracles", result.stdout)

    def test_longer_delivery_exposes_wait(self):
        low, _ = readiness_cost(self.fixture(), "T2", 20)
        high, _ = readiness_cost(self.fixture(), "T2", 1000)
        self.assertGreater(high["wait"], low["wait"])
        self.assertGreater(high["total"], low["total"])

    def test_drain_has_no_phantom_refill(self):
        tile = self.fixture()
        tile["grid"].update(k_iterations=1, problem=(32, 32, 16))
        record, terms = readiness_cost(tile, "T2", 100)
        self.assertEqual(terms[4], 2)
        self.assertEqual(record["window"], 0)
        self.assertEqual(terms[-1], sum(terms[:-1]))

    def test_more_stages_are_not_assumed_from_large_k(self):
        tile = self.fixture()
        tile["grid"].update(k_iterations=1000000000, problem=(32, 32, 16000000000))
        record, _ = readiness_cost(tile, "T2", 100)
        self.assertGreater(record["total"], 0)

    def audit_fixture(self):
        mode, rows, decision, tiles, direct = test_direct_staged_comparison.DirectComparisonTest().fixture()
        mode["model"] = 4
        resources, records = {}, {}
        tile = tiles[-1]
        for row in rows:
            ident = row["id"]
            record, terms = readiness_cost(tile, row["kind"], 100, direct.get(ident))
            records[ident] = record
            row.update(zip(("compute", "memory", "communication", "sync", "launch", "runtime", "total"), terms))
            resources[ident] = dict(register_bytes=12 if row["kind"] == "D" else 12,
                register_capacity=1020, shared_bytes=row["buffering"], shared_limit=49152,
                threads=256, thread_limit=1024, source_line=9)
        return mode, rows, decision, tiles, direct, resources, records, dict(name="assumed_v1", latency=100)

    def test_independent_ledger_audit(self):
        self.assertEqual(verify_joint(*self.audit_fixture())["chosen"]["kind"], "D")

    def test_missing_and_corrupt_readiness_rejected(self):
        for mutation in ("missing", "wait", "total", "cost", "profile"):
            args = self.audit_fixture()
            if mutation == "missing":
                args[6].pop(1)
            elif mutation == "profile":
                args[-1]["latency"] += 1
            elif mutation == "cost":
                args[1][0]["total"] += 1
            else:
                args[6][1][mutation] += 1
            with self.assertRaises(ValueError, msg=mutation):
                verify_joint(*args)

    def test_hardware_payload_drives_independent_recomputation(self):
        for name, value in (("memory_bytes_per_cycle", 7), ("scalar_ops_per_cycle", 16),
                            ("local_issues_per_cycle", 1), ("copy_elements_per_cycle", 3),
                            ("wait_cycles", 31), ("release_cycles", 17), ("store_latency", 19)):
            args = list(self.audit_fixture())
            args[0]["model"] = 5
            hw = legacy_performance(100)
            hw[name] = value
            args[-1] = dict(name="sm90_estimated_v1", architecture="sm_90", target=2,
                revision=1, unit="reference_cycles", provenance="estimated", override=0, performance=hw)
            for row in args[1]:
                record, terms = readiness_cost(args[3][-1], row["kind"], None,
                    args[4].get(row["id"]), hw)
                args[6][row["id"]] = record
                row.update(zip(("compute", "memory", "communication", "sync", "launch", "runtime", "total"), terms))
            args[2]["selected"] = min(args[1], key=lambda row: (row["total"], row["id"]))["id"]
            self.assertEqual(verify_joint(*args)["profile"]["performance"][name], value)

    def test_historical_model_cannot_impersonate_table_model(self):
        args = list(self.audit_fixture())
        args[0]["model"] = 5
        with self.assertRaisesRegex(ValueError, "description"):
            verify_joint(*args)

    def occupancy_fixture(self):
        args = list(self.audit_fixture())
        args[0]["model"] = 6
        hw = legacy_performance(100)
        resource = dict(zip(HW_RESOURCE_COLUMNS,
            (32, 64, 2048, 1024, 65536, 255, 256, 4, 233472, 65536, 128, 1024)))
        args[-1] = dict(name="sm90_estimated_v2", architecture="sm_90", target=2,
            revision=2, unit="reference_cycles", provenance="estimated", override=0,
            performance=hw, resources=resource)
        occupancy = {}
        for row in args[1]:
            ident = row["id"]
            legacy, _ = readiness_cost(args[3][-1], row["kind"], None, args[4].get(ident), hw)
            occ = occupancy_record(resource, hw, args[5][ident], legacy["ctas"], legacy["total"])
            occupancy[ident] = occ
            record, terms = readiness_cost(args[3][-1], row["kind"], None, args[4].get(ident), hw, occ)
            args[6][ident] = record
            row.update(zip(("compute", "memory", "communication", "sync", "launch", "runtime", "total"), terms))
        args[2]["selected"] = min(args[1], key=lambda row: (row["total"], row["id"]))["id"]
        args.append(occupancy)
        return args

    def test_independent_occupancy_audit(self):
        result = verify_joint(*self.occupancy_fixture())
        self.assertEqual(result["legacy_chosen"]["kind"], "D")
        self.assertGreater(result["occupancy"][1]["resident"], 0)
        self.assertIn("direct_roofline", result["derived"][2])

    def test_readiness_arithmetic_overflow_rejected(self):
        tile = self.fixture()
        tile["grid"].update(ctas=2 ** 63, k_iterations=2 ** 63)
        with self.assertRaisesRegex(ValueError, "overflow"):
            readiness_cost(tile, "T2", 100)

    def test_occupancy_fields_and_missing_rows_fail_closed(self):
        for field in ("registers", "shared", "block_limit", "warp_limit", "thread_limit",
                      "register_limit", "shared_limit", "resident", "warps", "limiting",
                      "full", "tail", "unused", "tail_sms", "legacy_total", "source_line", "missing"):
            args = self.occupancy_fixture()
            if field == "missing":
                args[-1].pop(1)
            else:
                args[-1][1][field] += 1
            with self.assertRaises(ValueError, msg=field):
                verify_joint(*args)

    def test_occupancy_table_required_and_historical_contract_isolated(self):
        for mutation in ("table", "revision", "historical"):
            args = self.occupancy_fixture()
            if mutation == "table":
                args[-2].pop("resources")
            elif mutation == "revision":
                args[-2]["revision"] = 1
            else:
                args[0]["model"] = 5
            with self.assertRaises(ValueError):
                verify_joint(*args)


if __name__ == "__main__":
    unittest.main()
