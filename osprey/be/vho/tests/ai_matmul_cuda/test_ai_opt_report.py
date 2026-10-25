"""Fail-closed checks for the compact AIO-9 optimization trace."""

from pathlib import Path
import tempfile
import unittest

from ai_opt_report import parse, render


def candidate(tile: int, family: str, total: int,
              version: str = "3", source_line: int = 25,
              roof_bytes: int = 335000) -> str:
    fields = ["AI_OPT", version, "TILE_CAND", "1", str(tile), str(tile),
              str(tile), family, "proven", "none", "1", "relative", "low",
              str(total - 2), "0", "0", "2", "0", "0", str(total),
              "32768", "72", "256", "128", "128", "32"]
    if version in ("2", "3", "4", "5", "6", "7"):
        fields.append(str(source_line))
    if version in ("4", "5", "6", "7"):
        fields.extend(("1000", str(roof_bytes), "1",
                       str((roof_bytes + 3349) // 3350),
                       str((roof_bytes + 3349) // 3350)))
    return " ".join(fields)


TRACE = "\n".join((
    candidate(1, "baseline", 120),
    candidate(2, "cuda_64", 100),
    candidate(3, "cuda_128", 90),
    "AI_OPT 3 TILE_SELECT 1 1 3 2 3 3 0 selection_reason=lowest_complete_relative_cost",
    "AI_OPT 3 TILE_ACTION 1 3 source_sidecar none dsl_to_runtime 3",
)) + "\n"


class ReportTest(unittest.TestCase):
    def test_p8_tma_trace_and_phase_report(self):
        trace = "\n".join((
            "AI_OPT 5 ROOF_TARGET 2 h100_sxm_reference 67000 3350 traffic=global_requests_as_cold_hbm_proxy ceiling=published_peak",
            candidate(1, "baseline", 1300, version="5", roof_bytes=1675000),
            candidate(2, "cuda_64", 1000, version="5", roof_bytes=335000),
            candidate(3, "cuda_128", 900, version="5", roof_bytes=268000),
            "AI_OPT 5 TILE_SELECT 1 1 3 2 3 3 0 selection_reason=lowest_complete_relative_cost",
            "AI_OPT 5 FETCH_CAND 1 1 1 demand proven none 1 0 0 1000 0 1000",
            "AI_OPT 5 FETCH_CAND 1 2 2 vector proven none 1 0 32768 800 0 800",
            "AI_OPT 5 FETCH_CAND 1 3 3 async_copy proven none 2 1 65536 1000 500 500",
            "AI_OPT 5 FETCH_CAND 1 4 4 tma_like proven none 2 1 65536 300 200 100",
            "AI_OPT 5 FETCH_SELECT 1 1 4 tma_like selection_reason=lowest_complete_relative_cost",
            "AI_OPT 5 TILE_ACTION 1 3 source_sidecar none dsl_to_runtime 3",
            "AI_OPT 5 FETCH_ACTION 1 4 source_sidecar tma_2d whirl_rewrite=none",
        )) + "\n"
        candidates, selection, action = parse(trace)
        self.assertEqual(selection["fetch"]["selection"]["selected"], 4)
        self.assertEqual(action["tile_rewrite"], "none")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "compile.t"
            source.write_text(trace, encoding="utf-8")
            render(source, root / "reports")
            fetch = (root / "reports/phases/fetch-pipeline.log").read_text()
            self.assertTrue(fetch.startswith("# What it summarizes:"))
            self.assertIn("engine=tma_like", fetch)
            self.assertIn("buffer_bytes=65536", fetch)
            self.assertIn("movement=tma_2d", fetch)
        with self.assertRaisesRegex(ValueError, "missing P8"):
            parse(trace.rsplit("AI_OPT 5 FETCH_ACTION", 1)[0])

    def test_roofline_changes_near_cost_choice(self):
        trace = "\n".join((
            "AI_OPT 4 ROOF_TARGET 2 h100_sxm_reference 67000 3350 traffic=global_requests_as_cold_hbm_proxy ceiling=published_peak",
            candidate(1, "baseline", 1300, version="4", roof_bytes=1675000),
            candidate(2, "cuda_64", 1000, version="4", roof_bytes=335000),
            candidate(3, "cuda_128", 1020, version="4", roof_bytes=268000),
            "AI_OPT 4 TILE_SELECT 1 1 3 2 3 3 0 selection_reason=roofline_near_cost",
            "AI_OPT 4 TILE_ACTION 1 3 source_sidecar none dsl_to_runtime 3",
        )) + "\n"
        candidates, selection, _ = parse(trace)
        self.assertEqual(selection["selected"], 3)
        self.assertEqual(candidates[2]["roof_bound_ns"], 80)
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "compile.t"
            source.write_text(trace, encoding="utf-8")
            render(source, root / "reports")
            report = (root / "reports/phases/tile.log").read_text()
            self.assertIn("selection_reason=roofline_near_cost", report)
            self.assertIn("roof_bound_ns=80", report)
            self.assertIn("FP32=67000 GFLOP/s", report)
            self.assertIn("intensity_flop_per_byte=0.003", report)
            self.assertIn("roof_limit=hbm", report)
        with self.assertRaisesRegex(ValueError, "selection disagrees"):
            parse(trace.replace("TILE_SELECT 1 1 3", "TILE_SELECT 1 1 2")
                  .replace("TILE_ACTION 1 3", "TILE_ACTION 1 2"))

    def test_roofline_cannot_override_large_cost_gap(self):
        trace = "\n".join((
            "AI_OPT 4 ROOF_TARGET 2 h100_sxm_reference 67000 3350 traffic=global_requests_as_cold_hbm_proxy ceiling=published_peak",
            candidate(1, "baseline", 1300, version="4", roof_bytes=1675000),
            candidate(2, "cuda_64", 1000, version="4", roof_bytes=335000),
            candidate(3, "cuda_128", 1100, version="4", roof_bytes=268000),
            "AI_OPT 4 TILE_SELECT 1 1 2 2 3 3 0 selection_reason=lowest_complete_relative_cost",
            "AI_OPT 4 TILE_ACTION 1 2 source_sidecar none dsl_to_runtime 3",
        ))
        self.assertEqual(parse(trace)[1]["selected"], 2)

    def test_roofline_target_and_projection_are_checked(self):
        trace = "\n".join((
            "AI_OPT 4 ROOF_TARGET 2 h100_sxm_reference 67000 3350 traffic=global_requests_as_cold_hbm_proxy ceiling=published_peak",
            candidate(1, "baseline", 120, version="4"),
            candidate(2, "cuda_64", 100, version="4"),
            "AI_OPT 4 TILE_SELECT 1 1 2 2 2 2 0 selection_reason=lowest_complete_relative_cost",
            "AI_OPT 4 TILE_ACTION 1 2 source_sidecar none dsl_to_runtime 3",
        ))
        self.assertEqual(parse(trace)[1]["selected"], 2)
        with self.assertRaisesRegex(ValueError, "reference Roofline ceilings"):
            parse(trace.replace("67000 3350", "67000 3300"))
        with self.assertRaisesRegex(ValueError, "Roofline projection"):
            parse(trace.replace("1000 335000 1 100 100", "1000 335000 1 99 100"))

    def test_reports_record_decision_and_non_rewrite(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            trace = root / "compile.t"
            trace.write_text(TRACE, encoding="utf-8")
            selection, action = render(trace, root / "reports")
            self.assertEqual(selection["selected"], 3)
            self.assertEqual(selection["reason"], "lowest_complete_relative_cost")
            self.assertEqual(action["tile_rewrite"], "none")
            tile = (root / "reports/phases/tile.log").read_text()
            self.assertTrue(tile.startswith("# What it summarizes:"))
            self.assertIn("# compute=max(1,floor(2*M*N*K/1024))", tile)
            self.assertIn("# read_bytes baseline=2*M*N*K*E.", tile)
            self.assertIn("# read_bytes tiled=ceil(M/BM)*ceil(N/BN)*ceil(K/BK)", tile)
            self.assertIn("memory_unhidden is a traffic proxy", tile)
            self.assertIn("candidate=2", tile)
            self.assertIn("decision=not_selected_cost", tile)
            self.assertIn("decision=selected", tile)
            self.assertIn("compute=88", tile)
            self.assertIn("cta=128,128,32 source_line=25", tile)
            self.assertIn("tile_whirl_rewrite=none", tile)
            self.assertIn("selection_reason=lowest_complete_relative_cost", tile)
            summary = (root / "reports/optimization-summary.log").read_text()
            self.assertIn("P7 tile analyzed_no_rewrite", summary)
            self.assertIn("selection_reason=lowest_complete_relative_cost", summary)

    def test_missing_action_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "missing candidate"):
            parse(TRACE.rsplit("AI_OPT 3 TILE_ACTION", 1)[0])

    def test_bad_cost_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "cost components"):
            parse(TRACE.replace(" 90 32768", " 91 32768"))

    def test_bad_choice_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "selection disagrees"):
            parse(TRACE.replace("TILE_SELECT 1 1 3", "TILE_SELECT 1 1 2")
                  .replace("TILE_ACTION 1 3", "TILE_ACTION 1 2"))

    def test_unknown_schema_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "unsupported AI_OPT schema"):
            parse(TRACE.replace("AI_OPT 3 TILE_CAND", "AI_OPT 8 TILE_CAND", 1))

    def test_v6_disabled_phase_coverage_and_reports(self):
        phase_rows = [
            f"AI_OPT 6 PHASE matmul_f32_source {phase} disabled_by_option "
            "cuda_tile_output_unset 0 0 0 0"
            for phase in range(12)
        ]
        trace = "\n".join(("AI_OPT 6 CONTEXT ai_matmul_m0 matmul_f32_source",
                           *phase_rows)) + "\n"
        candidates, selection, action = parse(trace)
        self.assertEqual((candidates, action), ([], {}))
        self.assertEqual(len(selection["phases"]), 12)
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "compile.t"
            source.write_text(trace, encoding="utf-8")
            render(source, root / "reports")
            summary = (root / "reports/optimization-summary.log").read_text()
            full = (root / "reports/optimization.log").read_text()
            self.assertEqual(sum(line.startswith("P") for line in summary.splitlines()), 12)
            self.assertEqual(full.count("# What it summarizes:"), 13)
            for filename in ("semantic.log", "locality.log", "fusion.log",
                             "layout.log", "placement.log", "communication.log",
                             "residency.log", "tile.log", "fetch-pipeline.log",
                             "physical-plan.log", "runtime-variant.log", "feedback.log"):
                phase_file = root / "reports/phases" / filename
                self.assertTrue(phase_file.read_text().startswith("# What it summarizes:"))
        with self.assertRaisesRegex(ValueError, "missing AI optimization context or phase coverage"):
            parse(trace.rsplit("AI_OPT 6 PHASE", 1)[0])
        with self.assertRaisesRegex(ValueError, "invalid or duplicate phase coverage"):
            parse(trace + phase_rows[0] + "\n")

    def test_legacy_emitter_off_has_explicit_phase_reason(self):
        phase_rows = [
            f"AI_OPT 7 PHASE matmul_f32_source {phase} disabled_by_option "
            "cuda_legacy_emit_off 0 0 0 0"
            for phase in range(12)
        ]
        trace = "\n".join(("AI_OPT 7 CONTEXT ai_matmul_m0 matmul_f32_source",
                           *phase_rows)) + "\n"
        candidates, selection, action = parse(trace)
        self.assertEqual((candidates, action), ([], {}))
        self.assertEqual(selection["phases"][7]["reason"],
                         "cuda_legacy_emit_off")

    def test_v6_tile_coverage_must_match_decision(self):
        rows = [
            "AI_OPT 6 CONTEXT ai_matmul_m0 matmul_f32_source",
            "AI_OPT 6 ROOF_TARGET 2 h100_sxm_reference 67000 3350 traffic=global_requests_as_cold_hbm_proxy ceiling=published_peak",
            candidate(1, "baseline", 1300, version="6", roof_bytes=1675000),
            candidate(2, "cuda_64", 1000, version="6", roof_bytes=335000),
            candidate(3, "cuda_128", 900, version="6", roof_bytes=268000),
            "AI_OPT 6 TILE_SELECT 1 1 3 2 3 3 0 selection_reason=lowest_complete_relative_cost",
            "AI_OPT 6 TILE_ACTION 1 3 source_sidecar none dsl_to_runtime 3",
        ]
        for phase in range(12):
            active = phase in (0, 1, 7, 9)
            count = 3 if phase == 7 else 0
            selected = 3 if phase in (7, 9) else 0
            inactive_status = "disabled_by_option" if phase == 8 else "disabled_by_policy"
            rows.append(
                f"AI_OPT 6 PHASE matmul_f32_source {phase} "
                f"{'analyzed_no_rewrite' if active else inactive_status} "
                f"{'source_sidecar_no_whirl_rewrite' if active else 'phase_not_in_matmul_source_slice'} "
                f"{int(active)} {count} {selected} 0"
            )
        trace = "\n".join(rows) + "\n"
        self.assertEqual(parse(trace)[1]["phases"][7]["selected"], 3)
        with self.assertRaisesRegex(ValueError, "P7/P9 coverage disagrees"):
            parse(trace.replace("source_sidecar_no_whirl_rewrite 1 3 3 0",
                                "source_sidecar_no_whirl_rewrite 1 3 2 0"))
        with self.assertRaisesRegex(ValueError, "inactive phase"):
            parse(trace.replace("phase_not_in_matmul_source_slice 0 0 0 0",
                                "phase_not_in_matmul_source_slice 0 1 0 0", 1))

    def test_v7_semantic_and_locality_facts_are_required(self):
        rows = [
            "AI_OPT 7 CONTEXT ai_matmul_m0 matmul_f32_source",
            "AI_OPT 7 ROOF_TARGET 2 h100_sxm_reference 67000 3350 traffic=global_requests_as_cold_hbm_proxy ceiling=published_peak",
            candidate(1, "baseline", 1300, version="7", roof_bytes=1675000).replace(
                " 128 128 32 25", " 0 0 0 25"),
            candidate(2, "cuda_64", 1000, version="7", roof_bytes=335000).replace(
                " 128 128 32 25", " 64 64 16 25"),
            candidate(3, "cuda_128", 900, version="7", roof_bytes=268000),
            "AI_OPT 7 TILE_GRID 1 1 1 128 128 128 0 0 0 0",
            "AI_OPT 7 TILE_GRID 1 2 2 128 128 128 2 2 4 8",
            "AI_OPT 7 TILE_GRID 1 3 3 128 128 128 1 1 1 4",
            "AI_OPT 7 TILE_SELECT 1 1 3 2 3 3 0 selection_reason=lowest_complete_relative_cost",
            "AI_OPT 7 K_LOOP 1 3 3 0 128 32 detached_plan_only",
            "AI_OPT 7 TILE_ACTION 1 3 source_sidecar none dsl_to_runtime 3",
        ]
        for value in (1, 2, 3):
            rows.append(f"AI_OPT 7 SEMANTIC {value} {value} constant 2 [128,128] f32 unique single_use 1 25")
            rows.append(f"AI_OPT 7 LOCALITY {value} {value} static exact_block exact proven_unique contraction high on 65536 1 65536 65536 0 1 3")
        for phase in range(12):
            active = phase in (0, 1, 7, 9)
            status = ("analyzed_no_rewrite" if active else
                      "disabled_by_option" if phase == 8 else
                      "disabled_by_policy")
            rows.append(f"AI_OPT 7 PHASE matmul_f32_source {phase} {status} "
                        f"{'source_sidecar_no_whirl_rewrite' if active else 'phase_not_in_matmul_source_slice'} "
                        f"{int(active)} {3 if phase == 7 else 0} "
                        f"{3 if phase in (7, 9) else 0} 0")
        trace = "\n".join(rows) + "\n"
        self.assertEqual(len(parse(trace)[1]["locality"]), 3)
        self.assertEqual(parse(trace)[0][2]["grid"]["ctas"], 1)
        with_kernel = trace.replace(
            "AI_OPT 7 TILE_ACTION 1 3",
            "AI_OPT 7 WHIRL_KERNEL 1 3 3 /artifact/matmul.device.B\n"
            "AI_OPT 7 TILE_ACTION 1 3")
        whirl_only = with_kernel.replace(
            "TILE_ACTION 1 3 source_sidecar",
            "TILE_ACTION 1 3 device_whirl_sidecar")
        self.assertEqual(parse(whirl_only)[1]["whirl_kernel"]["tile"], 3)
        bound = whirl_only.replace(
            "AI_OPT 7 WHIRL_KERNEL 1 3 3 /artifact/matmul.device.B\n",
            "AI_OPT 7 WHIRL_KERNEL 1 3 3 /artifact/matmul.device.B\n"
            "AI_OPT 7 DEVICE_BIND 0 ai_matmul_m0 3 3 "
            "ai_matmul_tile_f32 /artifact/matmul.device.B\n")
        self.assertEqual(parse(bound)[1]["device_binding"]["entry"],
                         "ai_matmul_tile_f32")
        binding_only = bound.replace(
            "AI_OPT 7 WHIRL_KERNEL 1 3 3 /artifact/matmul.device.B\n", "")
        self.assertEqual(parse(binding_only)[1]["whirl_kernel"]["tile"], 3)
        with self.assertRaisesRegex(ValueError, "device binding disagrees"):
            parse(bound.replace("DEVICE_BIND 0 ai_matmul_m0 3",
                                "DEVICE_BIND 0 other_pu 3"))
        with self.assertRaisesRegex(ValueError, "device WHIRL action has no kernel"):
            parse(whirl_only.replace(
                "AI_OPT 7 WHIRL_KERNEL 1 3 3 /artifact/matmul.device.B\n", ""))
        with self.assertRaisesRegex(ValueError, "device WHIRL kernel disagrees"):
            parse(whirl_only.replace("WHIRL_KERNEL 1 3 3", "WHIRL_KERNEL 1 2 3"))
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "compile.t"
            source.write_text(whirl_only, encoding="utf-8")
            render(source, root / "reports")
            self.assertIn("state=materialized_sidecar",
                          (root / "reports/phases/physical-plan.log").read_text())
            self.assertIn("device_pu=ai_matmul_tile_f32",
                          (root / "reports/optimization-summary.log").read_text())
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "compile.t"
            source.write_text(trace, encoding="utf-8")
            render(source, root / "reports")
            self.assertIn("shape=[128,128]",
                          (root / "reports/phases/semantic.log").read_text())
            self.assertIn("alias=proven_unique",
                          (root / "reports/phases/locality.log").read_text())
            tile_report = (root / "reports/phases/tile.log").read_text()
            self.assertIn("grid_ctas=4 k_iterations=8", tile_report)
            self.assertIn("selection_grid_ctas=1", tile_report)
            self.assertIn("whirl_k_loop=DO_LOOP scope=detached_plan_only", tile_report)
            self.assertIn("device_pu=not_materialized", tile_report)
            self.assertIn("grid_parallelism_cost=unmodeled", tile_report)
            summary = (root / "reports/optimization-summary.log").read_text()
            self.assertIn("selected_grid_ctas=1 grid_parallelism_cost=unmodeled", summary)
        with self.assertRaisesRegex(ValueError, "tile grid disagrees"):
            parse(trace.replace(" 2 2 4 8", " 2 2 3 8"))
        with self.assertRaisesRegex(ValueError, "incomplete tile grid"):
            parse(trace.replace("AI_OPT 7 TILE_GRID 1 2 2 128 128 128 2 2 4 8\n", ""))
        with self.assertRaisesRegex(ValueError, "selected K loop disagrees"):
            parse(trace.replace("K_LOOP 1 3 3 0 128 32", "K_LOOP 1 3 3 0 128 16"))
        legacy_trace = "\n".join(line for line in trace.splitlines()
                                 if " TILE_GRID " not in line) + "\n"
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "compile.t"
            source.write_text(legacy_trace, encoding="utf-8")
            render(source, root / "reports")
            legacy_tile = (root / "reports/phases/tile.log").read_text()
            self.assertNotIn("# Grid:", legacy_tile)
            self.assertNotIn("selection_grid_ctas=", legacy_tile)
        with self.assertRaisesRegex(ValueError, "incomplete or inconsistent P0/P1"):
            parse(trace.replace("AI_OPT 7 LOCALITY 3 3", "AI_OPT 7 LOCALITY 3 4"))
        applied = trace.replace(
            "TILE_ACTION 1 3 source_sidecar none",
            "TILE_ACTION 1 3 source_sidecar generated_runtime_call")
        for phase in (7, 9):
            applied = applied.replace(
                f"AI_OPT 7 PHASE matmul_f32_source {phase} analyzed_no_rewrite "
                "source_sidecar_no_whirl_rewrite",
                f"AI_OPT 7 PHASE matmul_f32_source {phase} rewritten "
                "generated_runtime_call")
            applied = applied.replace(
                f"AI_OPT 7 PHASE matmul_f32_source {phase} rewritten "
                "generated_runtime_call 1 "
                f"{3 if phase == 7 else 0} 3 0",
                f"AI_OPT 7 PHASE matmul_f32_source {phase} rewritten "
                "generated_runtime_call 1 "
                f"{3 if phase == 7 else 0} 3 1")
        self.assertEqual(parse(applied)[2]["tile_rewrite"],
                         "generated_runtime_call")
        with self.assertRaisesRegex(ValueError, "P7/P9 coverage"):
            parse(applied.replace("generated_runtime_call 1 3 3 1",
                                  "generated_runtime_call 1 3 3 0"))

    def test_legacy_trace_has_unknown_source_line(self):
        legacy = "\n".join((
            candidate(1, "baseline", 120, version="1"),
            candidate(2, "cuda_64", 100, version="1"),
            candidate(3, "cuda_128", 90, version="1"),
            "AI_OPT 1 TILE_SELECT 1 1 3 2 3 3 0",
            "AI_OPT 1 TILE_ACTION 1 3 source_sidecar none dsl_to_runtime 3",
        ))
        candidates, _, _ = parse(legacy)
        self.assertIsNone(candidates[0]["source_line"])

    def test_missing_v3_line_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "malformed AI_OPT record"):
            parse(TRACE.replace(" 128 128 32 25", " 128 128 32", 1))

    def test_mixed_schema_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "mixed AI_OPT schema"):
            parse(TRACE.replace("AI_OPT 3 TILE_SELECT", "AI_OPT 1 TILE_SELECT"))

    def test_unrecognized_selection_reason_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "unknown tile selection reason"):
            parse(TRACE.replace("selection_reason=lowest_complete_relative_cost",
                                "selection_reason=unknown"))

    def test_v2_selection_reason_is_not_inferred(self):
        v2 = "\n".join((
            candidate(1, "baseline", 120, version="2"),
            candidate(2, "cuda_64", 100, version="2"),
            candidate(3, "cuda_128", 90, version="2"),
            "AI_OPT 2 TILE_SELECT 1 1 3 2 3 3 0",
            "AI_OPT 2 TILE_ACTION 1 3 source_sidecar none dsl_to_runtime 3",
        ))
        _, selection, _ = parse(v2)
        self.assertEqual(selection["reason"], "not_recorded")


if __name__ == "__main__":
    unittest.main()
