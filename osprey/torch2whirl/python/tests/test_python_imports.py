from __future__ import annotations

import importlib
from pathlib import Path
import sys
import tempfile
import unittest

from open64_dsc.python_classes import collect_python_model_classes
from open64_dsc.python_imports import (
    collect_imported_python_callables,
    collect_python_import_census,
    resolve_reachable_imported_callables,
)
from open64_dsc import WhirlExportOptions, export_to_whirl


class PythonImportDiscoveryTest(unittest.TestCase):
    def _load_fixture(self, files, module_name):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name, source in files.items():
                (root / name).write_text(source.lstrip(), encoding="utf-8")

            sys.path.insert(0, str(root))
            try:
                module = importlib.import_module(module_name)
                yield module
            finally:
                sys.path.remove(str(root))
                for name in tuple(sys.modules):
                    if name in {
                        "consumer",
                        "dependency",
                        "diagnostic_consumer",
                    }:
                        sys.modules.pop(name, None)

    def test_aliases_share_one_canonical_definition(self) -> None:
        files = {
            "dependency.py": """
def helper(value):
    return value + 1


class Layer:
    def __init__(self, scale=2):
        self.scale = scale

    def named_parameters(self, recurse=True):
        return (("weight", object()),)

    def named_buffers(self, recurse=True):
        return (("running", object()),)

    def forward(self, value):
        return value * self.scale
""",
            "consumer.py": """
import dependency
import dependency as dep_alias
from dependency import helper
from dependency import helper as helper_alias
from dependency import Layer as ImportedLayer

AliasLayer = ImportedLayer
""",
        }

        for consumer in self._load_fixture(files, "consumer"):
            records = collect_imported_python_callables(consumer)

        by_name = {record.canonical_name: record for record in records}
        helper = by_name["dependency.helper"]
        self.assertEqual(
            tuple(helper.import_names),
            (
                "dep_alias.helper",
                "dependency.helper",
                "helper",
                "helper_alias",
            ),
        )
        self.assertEqual(
            helper.import_aliases["helper_alias"],
            "dependency.helper",
        )
        self.assertEqual(helper.importing_module, "consumer")
        self.assertEqual(helper.kind, "function")
        self.assertEqual(helper.signature, "(value)")
        self.assertNotEqual(helper.source_line, 0)
        self.assertEqual(len(helper.implementation_fingerprint), 64)

        forward = by_name["dependency.Layer.forward"]
        self.assertEqual(
            tuple(forward.import_names),
            (
                "AliasLayer.forward",
                "ImportedLayer.forward",
                "dep_alias.Layer.forward",
                "dependency.Layer.forward",
            ),
        )
        self.assertEqual(forward.kind, "method")
        self.assertEqual(forward.signature, "(self, value)")
        self.assertEqual(
            forward.reexport_sources,
            {"AliasLayer.forward": "ImportedLayer"},
        )
        self.assertIn("reexport_alias", forward.declaration_kind)

    def test_import_census_reports_unsupported_ambiguous_forms(self) -> None:
        files = {
            "dependency.py": """
class Layer:
    def forward(self, value):
        return value
""",
            "diagnostic_consumer.py": """
import importlib
from dependency import *
from dependency import Layer

Dynamic = __import__("dependency")
AlsoDynamic = importlib.import_module("dependency")
Layer.forward = lambda self, value: value
""",
        }

        for consumer in self._load_fixture(files, "diagnostic_consumer"):
            census = collect_python_import_census(consumer)

        codes = [diagnostic.code for diagnostic in census.diagnostics]
        self.assertIn("wildcard_import", codes)
        self.assertEqual(codes.count("dynamic_import"), 2)
        self.assertIn("monkey_patch_import", codes)
        messages = {
            diagnostic.code: diagnostic.message
            for diagnostic in census.diagnostics
        }
        self.assertIn("not a stable import identity", messages["dynamic_import"])
        self.assertIn("mutates an imported binding", messages["monkey_patch_import"])

    def test_reachable_imports_map_instance_state_to_definition(self) -> None:
        files = {
            "dependency.py": """
class Layer:
    def __init__(self):
        self.scale = 2

    def named_parameters(self, recurse=True):
        return (("weight", object()),)

    def named_buffers(self, recurse=True):
        return (("running", object()),)

    def forward(self, value):
        return value
""",
            "consumer.py": """
from dependency import Layer as ImportedLayer


class Model:
    def __init__(self):
        self.layer = ImportedLayer()

    def named_modules(self):
        return (("", self), ("layer", self.layer))

    def forward(self, value):
        return self.layer.forward(value)
""",
        }

        for consumer in self._load_fixture(files, "consumer"):
            model = consumer.Model()
            definitions, instances = collect_python_model_classes(model)
            census = collect_python_import_census(consumer)
            reachable = resolve_reachable_imported_callables(
                consumer,
                census.callables,
                definitions,
                instances,
                "Model",
            )

        self.assertEqual(len(reachable), 1)
        imported = reachable[0]
        self.assertEqual(imported.canonical_name, "dependency.Layer.forward")
        self.assertEqual(imported.imported_spelling, "ImportedLayer.forward")
        self.assertEqual(imported.defining_module, "dependency")
        self.assertEqual(imported.importing_module, "consumer")
        self.assertEqual(tuple(imported.instance_paths), ("layer",))
        self.assertEqual(tuple(imported.context_identities), ("Model.layer",))
        self.assertEqual(
            tuple(imported.class_state_members),
            ("weight", "running", "scale"),
        )
        self.assertEqual(
            imported.state_to_formal_mapping["self.weight"],
            "layer.weight",
        )
        self.assertEqual(
            imported.state_to_formal_mapping["self.running"],
            "layer.running",
        )
        self.assertEqual(imported.state_to_formal_mapping["self.scale"], "2")

    def test_export_manifest_includes_import_review_evidence(self) -> None:
        files = {
            "dependency.py": """
class Layer:
    def named_parameters(self, recurse=True):
        return (("weight", object()),)

    def named_buffers(self, recurse=True):
        return ()

    def forward(self, value):
        return value
""",
            "consumer.py": """
from dependency import Layer as ImportedLayer


class Model:
    def __init__(self):
        self.layer = ImportedLayer()

    def named_modules(self):
        return (("", self), ("layer", self.layer))

    def forward(self, value):
        return self.layer.forward(value)
""",
        }

        for consumer in self._load_fixture(files, "consumer"):
            module = export_to_whirl(
                consumer.Model(),
                [],
                WhirlExportOptions(backend="mock", verify=False),
            )

        manifest = module.to_manifest()
        self.assertEqual(
            manifest["python_imports"][0]["canonical_name"],
            "dependency.Layer.forward",
        )
        self.assertEqual(
            manifest["python_imports"][0]["import_aliases"],
            {"ImportedLayer.forward": "dependency.Layer.forward"},
        )
        self.assertEqual(manifest["python_import_diagnostics"], [])
        self.assertEqual(
            manifest["python_reachable_imports"][0]["imported_spelling"],
            "ImportedLayer.forward",
        )
        self.assertEqual(
            manifest["python_reachable_imports"][0]["instance_paths"],
            ["layer"],
        )


if __name__ == "__main__":
    unittest.main()
