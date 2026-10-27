"""Negative trace-audit tests for the bounded native R1 contract."""
import unittest
from reduction_trace_report import HEADER, audit

ENTER = "AI_REDUCE 1 FORM preopt_enter 1 2 3 17 1 0 float32 0 verified 144"
EXIT = ENTER.replace("preopt_enter", "preopt_exit")


class ReductionTraceTest(unittest.TestCase):
    def test_complete(self):
        self.assertEqual(len(audit("\n".join((HEADER, ENTER, HEADER, EXIT)))), 1)

    def test_reject_incomplete_or_changed(self):
        for text in ("", HEADER, "\n".join((HEADER, ENTER)),
                     "\n".join((HEADER, EXIT)),
                     "\n".join((ENTER, EXIT)),
                     "\n".join((HEADER, ENTER, ENTER, EXIT)),
                     "\n".join((HEADER, ENTER, EXIT.replace("17", "18"))),
                     "\n".join((HEADER, ENTER, EXIT.replace("144", "0"))),
                     "\n".join((HEADER, ENTER, EXIT.replace("float32", "float16"))),
                     "\n".join((HEADER, ENTER, EXIT + " extra"))):
            with self.subTest(text=text), self.assertRaises(ValueError):
                audit(text)


if __name__ == "__main__":
    unittest.main()
