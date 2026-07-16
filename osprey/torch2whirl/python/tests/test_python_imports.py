from __future__ import annotations

import importlib
from pathlib import Path
import sys
import tempfile
import unittest

from open64_dsc.python_imports import collect_imported_python_callables


class PythonImportDiscoveryTest(unittest.TestCase):
    def test_aliases_share_one_canonical_definition(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "dependency.py").write_text(
                """
def helper(value):
    return value + 1


class Layer:
    def __init__(self, scale):
        self.scale = scale

    def forward(self, value):
        return value * self.scale
""".lstrip(),
                encoding="utf-8",
            )
            (root / "consumer.py").write_text(
                """
from dependency import helper
from dependency import helper as helper_alias
from dependency import Layer as ImportedLayer
""".lstrip(),
                encoding="utf-8",
            )

            sys.path.insert(0, str(root))
            try:
                consumer = importlib.import_module("consumer")
                records = collect_imported_python_callables(consumer)
            finally:
                sys.path.remove(str(root))
                sys.modules.pop("consumer", None)
                sys.modules.pop("dependency", None)

        by_name = {record.canonical_name: record for record in records}
        helper = by_name["dependency.helper"]
        self.assertEqual(
            tuple(helper.import_names),
            ("helper", "helper_alias"),
        )
        self.assertEqual(helper.kind, "function")
        self.assertEqual(helper.signature, "(value)")
        self.assertNotEqual(helper.source_line, 0)
        self.assertEqual(len(helper.implementation_fingerprint), 64)

        forward = by_name["dependency.Layer.forward"]
        self.assertEqual(tuple(forward.import_names), ("ImportedLayer.forward",))
        self.assertEqual(forward.kind, "method")
        self.assertEqual(forward.signature, "(self, value)")


if __name__ == "__main__":
    unittest.main()
