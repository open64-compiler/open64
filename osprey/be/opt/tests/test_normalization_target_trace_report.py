"""Synthetic LN1-c auditor tests, separate from native/assembler evidence."""
from pathlib import Path
import tempfile
import unittest

from normalization_target_trace_report import COLUMNS, HEADER, INSTRUCTIONS, audit, audit_carriers, write_reports
from test_normalization_ref_trace_report import trace as reference
from test_normalization_trace_report import fixture as legacy


def fixture(**changes):
    """Author a positional request/result pair with no invented GPU proof."""
    row = dict(zip(COLUMNS, "11 7 5 2 sm_90 1 1 12080 807 1 0 0 0 1 1 0 3 41880000 conditional_isa_carrier_binding 211".split()))
    row.update(changes)
    return HEADER + "\nAI_NORM_TARGET 1 BIND " + " ".join(row[name] for name in COLUMNS) + "\n"


class NormalizationTargetTraceTests(unittest.TestCase):
    def test_exact_targets_and_floors(self):
        self.assertEqual(len(audit(fixture(), reference())), 1)
        for arch, identifier in (("sm_90", "1"), ("sm_90a", "2"), ("sm_100", "3"), ("sm_100a", "4")):
            self.assertEqual(len(audit(fixture(arch=arch, arch_id=identifier), reference() + legacy())), 1)
        bad = fixture(arch="sm_100", arch_id="3", toolkit="12079", status="5", stages="1", count_bits="00000000", reason="unsupported_toolchain")
        self.assertEqual(audit(bad, reference() + legacy())[0]["status"], "5")
        with self.assertRaises(ValueError):
            audit(bad.replace("sm_100 3", "sm_90 1"), reference() + legacy())

    def test_unbound_v1_and_stale(self):
        v1 = fixture(pu="12801", node="1", recipe="1", arch_id="0", status="1", stages="0", count_bits="00000000", reason="source_unbound", source_line="23")
        self.assertEqual(audit(v1, reference() + legacy())[0]["recipe"], "1")
        self.assertEqual(audit(v1, legacy())[0]["recipe"], "1")
        stale = fixture(source_match="0", status="2", stages="0", arch_id="0", count_bits="00000000", reason="stale_source")
        self.assertEqual(audit(stale, reference() + legacy())[0]["status"], "2")

    def test_all_unsafe_controls_refused(self):
        for change in (dict(environment="0"), dict(fp_options="1"), dict(fp_options="256"),
                       dict(fp_options="2147483648"), dict(fast_math="1"), dict(roundoff="2"), dict(ieee="2")):
            good = fixture(**change, status="6", stages="1", count_bits="00000000", reason="unsafe_environment")
            self.assertEqual(audit(good, reference() + legacy())[0]["status"], "6")
            with self.assertRaises(ValueError):
                audit(fixture(**change), reference() + legacy())

    def test_forged_status_counts_and_schema(self):
        for change in (dict(status="6"), dict(stages="15"), dict(count_bits="3f800000"), dict(arch_id="3"),
                       dict(source_line="212"), dict(source_match="2"), dict(reason="GPU_certified"), dict(toolkit="012080"),
                       dict(toolkit=str(2**32 + 12080))):
            with self.subTest(change=change), self.assertRaises(ValueError):
                audit(fixture(**change), reference() + legacy())
        for text in (fixture().replace(HEADER + "\n", ""), fixture().replace("AI_NORM_TARGET 1", "AI_NORM_TARGET 2")):
            with self.assertRaises(ValueError):
                audit(text, reference() + legacy())

    def test_source_join_is_required(self):
        with self.assertRaises(ValueError):
            audit(fixture(node="99"), reference() + legacy())
        with self.assertRaises(ValueError):
            audit(fixture(), legacy())

    def test_actual_carrier_effects_and_instruction_mutations(self):
        cuda = 'extern "C" __global__ void ln_ref_scalar_carriers\n'
        cuda += "\n".join('__asm__ volatile ' + instruction + ' : "memory" ' + '"l"(' * (2 if i == 4 else 3)
                          for i, instruction in enumerate(INSTRUCTIONS))
        instructions = "\n".join(INSTRUCTIONS)
        audit_carriers(cuda, instructions, instructions)
        for before, after in (("div.rn.f32", "div.approx.f32"), ("__asm__ volatile", "__asm__"),
                              ('"l"(', '"r"('), ("mul.rn.f32", "mul.rn.ftz.f32")):
            with self.assertRaises(ValueError):
                audit_carriers(cuda.replace(before, after), instructions, instructions)
        with self.assertRaises(ValueError):
            audit_carriers(cuda, instructions, instructions.replace("sqrt.rn.f32", "sqrt.approx.f32"))

    def test_report_convention_and_no_execution_claim(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "query.t").write_text(fixture())
            (root / "source.t").write_text(reference() + legacy())
            write_reports(root / "query.t", root / "source.t", root / "reports")
            for path in (root / "reports").iterdir():
                self.assertTrue(path.read_text().startswith("# What it summarizes:"))
            summary = (root / "reports/optimization-summary.log").read_text()
            self.assertIn("LayerNorm_rewrites=0", summary)
            self.assertIn("GPU=not_run", summary)
            with self.assertRaises(FileExistsError):
                write_reports(root / "query.t", root / "source.t", root / "reports")


if __name__ == "__main__":
    unittest.main()
