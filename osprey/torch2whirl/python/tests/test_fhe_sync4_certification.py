from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from fhe_sync4_certification import certify_trace


def _trace() -> str:
    lines = ["FUNC_ENTRY <1,1,entry> {line: 1/1}"]
    lines.extend(
        f"FUNC_ENTRY <1,{index},clone{index}> {{line: 1/1}}"
        for index in range(2, 7)
    )
    lines.append(
        "FHE ReLU Materialization Image: version=1 "
        "capabilities=0x00000001 contexts=19 operations=114"
    )
    for context in range(1, 20):
        level = 15 if context <= 16 else 17 if context == 17 else 18
        lines.append(
            f"  [{context}] owner_pu=clone source=value{context}(relu) "
            f"context_identity=1(identity) callsite={context} "
            "role=post_refresh state_version=1 encryption=1 scheme=ckks "
            f"class=ciphertext level={level} scale_bits=56 components=2 "
            "precision_bits=30 slots=32768 alignment_group=0 "
            "layout=ckks.packed pending=0x4 "
            "bootstrap_reason=pre_relu_refresh flags=0x0"
        )
    kinds = (
        ("refresh", 0, 0),
        ("normalize", 0, 100),
        ("approx_stage", 1, 201),
        ("approx_stage", 2, 202),
        ("approx_stage", 3, 203),
        ("reconstruct_relu", 0, 0),
    )
    operation_id = 1
    next_derived_state = 20
    for context in range(1, 20):
        previous = 0
        for ordinal, (kind, stage, parameter) in enumerate(kinds):
            output = context if ordinal == 0 else next_derived_state
            lines.append(
                f"  [{operation_id}] owner_pu=clone source=value{context}(relu) "
                f"context_identity=1 callsite={context} ordinal={ordinal} "
                f"kind={kind} profile=1 stage={stage} range={context} "
                f"input_state={previous} output_state={output} "
                f"parameter_tcon={parameter} flags=0x0"
            )
            operation_id += 1
            if ordinal != 0:
                next_derived_state += 1
            previous = output
    return "\n".join(lines) + "\n"


class FHESync4CertificationTest(unittest.TestCase):
    def certify(self, text: str) -> dict[str, int]:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "artifact.T"
            path.write_text(text, encoding="utf-8")
            return certify_trace(path)

    def test_complete_schedule(self) -> None:
        self.assertEqual(
            self.certify(_trace()),
            {"pus": 6, "contexts": 19, "operations": 114, "refreshes": 19},
        )

    def test_reordered_stage_rejected(self) -> None:
        malformed = _trace().replace(
            "ordinal=3 kind=approx_stage profile=1 stage=2",
            "ordinal=3 kind=approx_stage profile=1 stage=3",
            1,
        )
        with self.assertRaisesRegex(AssertionError, "reordered"):
            self.certify(malformed)

    def test_disconnected_state_rejected(self) -> None:
        malformed = _trace().replace(
            "input_state=1 output_state=20",
            "input_state=99 output_state=20",
            1,
        )
        with self.assertRaisesRegex(AssertionError, "disconnected"):
            self.certify(malformed)


if __name__ == "__main__":
    unittest.main()
