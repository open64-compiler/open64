from __future__ import annotations

import hashlib
import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[4]
PLAN_PATH = "doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx"
PLAN_SHA256 = (
    "0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9"
)

PRIMARY_SYNC3_DOCS = (
    "doc/FHE-SYNC3-NATIVE-PLAN-CONTRACT.md",
    "doc/FHE-SYNC3-CONVERSION-CONTRACT.md",
    "doc/FHE-SYNC3-COMMIT-ACCEPTANCE-PLAN.md",
)

STATUS_DOCS = PRIMARY_SYNC3_DOCS + (
    "doc/FHE-SYNC3-COMMIT19-CERTIFICATION.md",
)

HANDOFF_PATH = "doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md"
HANDOFF_GATE_FIELDS = {
    "SYNC3_CURRENT_VERIFICATION": "UNVERIFIED",
    "SYNC4_MAY_CONSUME_SYNC3": "false",
}

OBSOLETE_TOP_LEVEL_STATUS = (
    "implemented, pending review",
    "full SecureResNet publication remains blocked by `CFHECNN-RELU-002`",
    "Status: execution plan for review. This document does not close any milestone.",
    "Status: **formally accepted and complete**",
)


def read_document(relative_path: str) -> str:
    return (ROOT / relative_path).read_text(encoding="utf-8")


def preamble(document: str) -> str:
    parts = re.split(r"^##\s+", document, maxsplit=1, flags=re.MULTILINE)
    return parts[0]


def authority_excerpt(document: str) -> str:
    match = re.search(
        r"^##\s+Authority[^\n]*\n(?P<body>.*?)(?=^##\s+|\Z)",
        document,
        flags=re.MULTILINE | re.DOTALL,
    )
    if match is None:
        return preamble(document)
    return f"{preamble(document)}\n{match.group('body')}"


class FHESync3PlanConsistencyTest(unittest.TestCase):
    def test_v010_authority_digest_matches_repository_bytes(self) -> None:
        digest = hashlib.sha256((ROOT / PLAN_PATH).read_bytes()).hexdigest().upper()
        self.assertEqual(digest, PLAN_SHA256)

    def test_primary_contracts_pin_the_v010_authority(self) -> None:
        for relative_path in PRIMARY_SYNC3_DOCS:
            with self.subTest(document=relative_path):
                authority = authority_excerpt(read_document(relative_path))
                self.assertIn(PLAN_PATH, authority)
                self.assertIn(PLAN_SHA256, authority)
                active_versions = set(
                    re.findall(
                        r"DSC_FHE_Compiler_Architecture_and_Integration_Plan_"
                        r"v(\d+\.\d+)\.docx",
                        authority,
                    )
                )
                self.assertEqual(active_versions, {"0.10"})

    def test_top_level_status_separates_history_from_current_gate(self) -> None:
        for relative_path in STATUS_DOCS:
            with self.subTest(document=relative_path):
                status = preamble(read_document(relative_path)).lower()
                self.assertIn("historical", status)
                self.assertIn("unverified", status)
                self.assertTrue(
                    "retained" in status
                    and ("accessible" in status or "presently accessible" in status)
                )

    def test_obsolete_top_level_statuses_do_not_reappear(self) -> None:
        for relative_path in STATUS_DOCS:
            with self.subTest(document=relative_path):
                status = preamble(read_document(relative_path))
                for obsolete in OBSOLETE_TOP_LEVEL_STATUS:
                    self.assertNotIn(obsolete, status)

    def test_handoff_has_one_consistent_machine_readable_gate(self) -> None:
        document = read_document(HANDOFF_PATH)
        parsed = {}
        for field, expected in HANDOFF_GATE_FIELDS.items():
            self.assertEqual(document.count(field), 1)
            values = re.findall(
                rf"^{re.escape(field)}=([^\s]+)$", document, flags=re.MULTILINE
            )
            self.assertEqual(values, [expected])
            parsed[field] = values[0]

        may_consume = parsed["SYNC4_MAY_CONSUME_SYNC3"]
        self.assertIn(may_consume, {"true", "false"})
        if parsed["SYNC3_CURRENT_VERIFICATION"] != "VERIFIED":
            self.assertEqual(may_consume, "false")


if __name__ == "__main__":
    unittest.main()
