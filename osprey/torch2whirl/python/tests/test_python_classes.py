from __future__ import annotations

import unittest

from open64_dsc.python_classes import collect_python_model_classes


class _Model:
    def __init__(self) -> None:
        self.training = False

    def forward(self, value):
        return value


class PythonClassDiscoveryTest(unittest.TestCase):
    def test_plain_model_has_class_definition_and_instance_state(self) -> None:
        definitions, instances = collect_python_model_classes(_Model())

        self.assertEqual(len(definitions), 1)
        self.assertEqual(definitions[0].class_name, "_Model")
        self.assertEqual(definitions[0].implementation_method, "forward")
        self.assertEqual(len(definitions[0].implementation_fingerprint), 64)
        self.assertEqual(len(instances), 1)
        self.assertEqual(instances[0].instance_path, "<model>")
        self.assertEqual(instances[0].scalar_state, {"training": "False"})


if __name__ == "__main__":
    unittest.main()
