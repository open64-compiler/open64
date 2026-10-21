"""Host-only checks for CUDA package generation and provenance."""

from pathlib import Path
import json
import tempfile
import unittest
from unittest.mock import patch

from prepare import FAMILIES, prepare, sha256
from prepare_compiler import prepare as prepare_compiler
from compile_source import m2_handoff
from remote import verify
from run_gpu import validate_samples
from test_source_contract import CANDIDATES, SELECTION, SOURCE


class PackageTest(unittest.TestCase):
    def test_all_families_preserve_generated_cu(self):
        with tempfile.TemporaryDirectory() as temporary:
            for family, shape in FAMILIES.items():
                root = prepare(Path(temporary) / family, family)
                source = root / "source/matmul.generated.cu"
                self.assertTrue(source.is_file())
                self.assertIn(f"#define BM {shape[0]}", source.read_text())
                self.assertIn(f"#define BK {shape[2]}", source.read_text())
                self.assertNotIn("@BM@", source.read_text())
                self.assertEqual(sha256(source),
                                 (root / "source.sha256").read_text().split()[0])
                with self.assertRaises(FileExistsError):
                    prepare(root, family)

    def test_return_verification_rejects_modified_source(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = prepare(Path(temporary) / "artifact", "cuda_64")
            source = root / "source/matmul.generated.cu"
            (root / "SHA256SUMS").write_text(
                f"{sha256(source)}  source/matmul.generated.cu\n")
            verify(root)
            source.write_text(source.read_text() + "\n")
            with self.assertRaises(RuntimeError):
                verify(root)

    def test_compiler_package_preserves_selected_source_and_trace(self):
        with tempfile.TemporaryDirectory() as temporary:
            compiler = Path(temporary) / "compiler"
            (compiler / "source").mkdir(parents=True)
            (compiler / "traces").mkdir()
            (compiler / "plans").mkdir()
            (compiler / "binary").mkdir()
            (compiler / "reports/phases").mkdir(parents=True)
            source = compiler / "source/matmul.generated.cu"
            source.write_text(SOURCE, encoding="utf-8")
            (compiler / "traces/compile.t").write_text("AI_OPT 7 CONTEXT test matmul_f32_source\n")
            (compiler / "binary/input.B").write_bytes(b"input")
            (compiler / "binary/lowered.I").write_bytes(b"lowered")
            (compiler / "reports/optimization-summary.log").write_text("summary\n")
            (compiler / "reports/phases/tile.log").write_text("tile\n")
            (compiler / "reports/phases/fetch-pipeline.log").write_text("fetch\n")
            selection = dict(SELECTION,
                             fetch={"selection": {"selected": 4}})
            handoff_path = compiler / "plans/m2-selected-tile.json"
            handoff_path.write_text(json.dumps(m2_handoff(
                source, compiler / "traces/compile.t",
                compiler / "binary/input.B", compiler / "binary/lowered.I",
                CANDIDATES, selection)))
            (compiler / "receipt.json").write_text(json.dumps({
                "source_path": "source/matmul.generated.cu",
                "decision_trace_path": "traces/compile.t",
                "compiler_generated_from_whirl_analysis": True,
                "p8_tma_requested": True,
                "source_sha256": sha256(source),
                "m2_handoff_path": "plans/m2-selected-tile.json",
                "m2_handoff_sha256": sha256(handoff_path),
                "selected_optimization_plan_id": 3,
                "p8_selected_plan_id": 4,
            }))
            files = sorted(path for path in compiler.rglob("*") if path.is_file())
            (compiler / "SHA256SUMS").write_text(
                "".join(f"{sha256(path)}  {path.relative_to(compiler)}\n"
                        for path in files))
            with patch("prepare_compiler.parse", return_value=(
                    CANDIDATES, selection, {"tile_rewrite": "none"})):
                package = prepare_compiler(compiler, Path(temporary) / "gpu")
            self.assertEqual((package / "source/matmul.generated.cu").read_bytes(),
                             source.read_bytes())
            metadata = json.loads((package / "compiler.json").read_text())
            self.assertEqual(metadata["source_sha256"], sha256(source))
            self.assertEqual(metadata["tile_plan_id"], 3)
            self.assertEqual(metadata["fetch_plan_id"], 4)
            self.assertEqual(metadata["m2_handoff_sha256"],
                             sha256(package / "plans/m2-selected-tile.json"))
            self.assertTrue((package / "source/benchmark.cu").is_file())
            with self.assertRaises(FileExistsError):
                prepare_compiler(compiler, package)

    def test_compiler_samples_require_both_complete_variants(self):
        rows = [{"sample": str(i), "variant": variant, "elapsed_ms": "0.1"}
                for variant in ("sync", "tma") for i in range(30)]
        validate_samples(rows, True)
        with self.assertRaisesRegex(RuntimeError, "tma wall-clock samples"):
            validate_samples(rows[:-1], True)
        with self.assertRaisesRegex(RuntimeError, "unexpected wall-clock sample"):
            validate_samples(rows + [{"sample": "0", "variant": "other",
                                      "elapsed_ms": "0.1"}], True)
        invalid = [dict(row) for row in rows]
        invalid[0]["elapsed_ms"] = "nan"
        with self.assertRaisesRegex(RuntimeError, "sync wall-clock samples"):
            validate_samples(invalid, True)


if __name__ == "__main__":
    unittest.main()
