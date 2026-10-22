"""Host-only integrity tests for the applied M2 remote validation package."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import tempfile
import unittest

from remote_generated import (NATIVE_EVIDENCE, PREPARED_REQUIRED,
                              kernel_control_summary, native_attempt_exists,
                              verify_native,
                              verify_prepared)


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def put(root: Path, relative: str, content: str) -> None:
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def manifest(root: Path, name: str, relatives: list[str]) -> None:
    put(root, name, "".join(
        f"{digest(root / relative)}  {relative}\n" for relative in relatives))


class RemoteGeneratedTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.handoff = {
            "whirl_rewrite_applied": True,
            "selected_tile_plan_id": 3,
            "selected_optimization_plan_id": 3,
        }
        self.prepared = sorted(PREPARED_REQUIRED)
        for relative in self.prepared:
            put(self.root, relative, relative + "\n")
        put(self.root, "plans/m2-selected-tile.json", json.dumps(self.handoff))
        put(self.root, "receipt.json", json.dumps({
            "schema": "ai-matmul-m2-generated-gpu-package-v1",
            "status": "native_gpu_execution_pending",
            "selected_tile_plan_id": 3,
            "selected_optimization_plan_id": 3,
        }))
        manifest(self.root, "SHA256SUMS", self.prepared)

    def test_prepared_and_native_evidence(self) -> None:
        verify_prepared(self.root)
        self.assertFalse(native_attempt_exists(self.root))
        native = list(NATIVE_EVIDENCE)
        for relative in native:
            put(self.root, relative, relative + "\n")
        put(self.root, "measurements/kernel-control.csv",
            "sample,variant,elapsed_ms\n" + "".join(
                f"{sample},{variant},{1.0 if variant == 'untiled_control' else 0.5}\n"
                for sample in range(30)
                for variant in (("selected_tile", "untiled_control")
                                if sample % 2 else
                                ("untiled_control", "selected_tile"))))
        self.assertEqual(kernel_control_summary(
            self.root / "measurements/kernel-control.csv")[
                "control_ratio_direct_over_tiled"], 2.0)
        put(self.root, "native-receipt.json", json.dumps({
            "schema": "ai-matmul-m2-generated-gpu-run-v1",
            "status": "gpu_correctness_verified_timing_uncalibrated",
            "generated_path_required_each_sample": True,
            "fallback_samples": 0,
            "samples": 30,
            "selected_tile_plan_id": 3,
            "selected_optimization_plan_id": 3,
            "kernel_control_origin": "validation_only_untiled_cuda",
            "kernel_control_verified": True,
            "kernel_control": kernel_control_summary(
                self.root / "measurements/kernel-control.csv"),
            "same_target_baseline_verified": False,
            "closes_m2_gate": False,
            "source_sha256": digest(self.root / "source/matmul.generated.cu"),
            "object_sha256": digest(self.root / "binary/matmul.o"),
            "executable_sha256": digest(self.root / "binary/matmul-generated"),
        }))
        manifest(self.root, "native-SHA256SUMS", native)
        self.assertTrue(native_attempt_exists(self.root))
        verify_native(self.root)
        control_path = self.root / "measurements/kernel-control.csv"
        original_control = control_path.read_text(encoding="utf-8")
        put(self.root, "measurements/kernel-control.csv",
            original_control.replace("untiled_control,1.0",
                                     "untiled_control,2.0"))
        manifest(self.root, "native-SHA256SUMS", native)
        with self.assertRaisesRegex(RuntimeError, "contradicts"):
            verify_native(self.root)
        put(self.root, "measurements/kernel-control.csv", original_control)
        manifest(self.root, "native-SHA256SUMS", native)
        put(self.root, "measurements/raw.csv", "tampered\n")
        with self.assertRaisesRegex(RuntimeError, "changed"):
            verify_native(self.root)

    def test_kernel_control_rejects_missing_pair(self) -> None:
        put(self.root, "measurements/kernel-control.csv",
            "sample,variant,elapsed_ms\n" +
            "0,untiled_control,1.0\n" * 60)
        with self.assertRaisesRegex(RuntimeError, "order"):
            kernel_control_summary(self.root / "measurements/kernel-control.csv")

    def test_prepared_rejects_tampering_and_unsafe_path(self) -> None:
        put(self.root, "source/matmul.generated.cu", "tampered\n")
        with self.assertRaisesRegex(RuntimeError, "changed"):
            verify_prepared(self.root)
        manifest(self.root, "SHA256SUMS", self.prepared)
        with (self.root / "SHA256SUMS").open("a", encoding="utf-8") as stream:
            stream.write("0" * 64 + "  /etc/passwd\n")
        with self.assertRaisesRegex(RuntimeError, "unsafe"):
            verify_prepared(self.root)

    def test_native_receipt_rejects_false_selection(self) -> None:
        verify_prepared(self.root)
        for relative in NATIVE_EVIDENCE:
            put(self.root, relative, relative)
        put(self.root, "native-receipt.json", json.dumps({
            "schema": "ai-matmul-m2-generated-gpu-run-v1",
            "status": "gpu_correctness_verified_timing_uncalibrated",
            "generated_path_required_each_sample": True,
            "fallback_samples": 0,
            "samples": 30,
            "selected_tile_plan_id": 999,
        }))
        manifest(self.root, "native-SHA256SUMS", list(NATIVE_EVIDENCE))
        with self.assertRaisesRegex(RuntimeError, "contradicts"):
            verify_native(self.root)

    def test_partial_native_attempt_is_not_reused(self) -> None:
        self.assertFalse(native_attempt_exists(self.root))
        put(self.root, "logs/nvcc.log", "compiler failed\n")
        self.assertTrue(native_attempt_exists(self.root))


if __name__ == "__main__":
    unittest.main()
