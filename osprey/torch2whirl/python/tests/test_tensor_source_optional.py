"""Mapped-image source-emitter boundaries; see AI-COMPILER-TENSOR-SOURCE-v0.1.md."""
import hashlib
import importlib.util
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import unittest


def produce(kind, path):
    """Build genuine native fixtures in a fresh process, without source templates."""
    from open64_dsc import load_builder
    from open64_dsc.backend import load_backend
    builder = load_builder("native")
    builder.begin_program()
    for ordinal in range(2 if kind == "multi" else 1):
        pu = builder.minimal_program_unit("unrelated_program_%d" % ordinal)
        file_id = builder.register_source_file(pu, __file__)
        ty = builder.tensor_type("input_type", "float32", 2, "[2,3]", {"layout": "logical"})
        x = builder.declare_pu_formal(pu, "argument", 0, ty, file_id, 1)
        if kind == "unsupported" or ordinal == 1:
            value = builder.common_relu(x)
        else:
            value = builder.operator("common.sub", 1, [x, x],
                {"attr.broadcast_rule": "numpy"}, "difference", ty)
        builder.append_program_unit_value(pu, value)
        builder.declare_pu_result(pu, "answer", 0, ty, file_id=file_id, line=1)
        builder.return_pu_values(pu, [value])
    builder.verify_program()
    assert load_backend("native").finalize_mapped_image(str(path), {})


@unittest.skipUnless(os.environ.get("OPEN64_WHIRL2C") and
    importlib.util.find_spec("open64_dsc._whirl"), "native source tools are not configured")
class TensorSourceTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.tool = Path(os.environ["OPEN64_WHIRL2C"])

    def image(self, kind):
        path = self.root / (kind + ".B")
        subprocess.run([sys.executable, __file__, "--produce", kind, str(path)], check=True)
        return path

    def emit(self, image):
        output = self.root / "result.cu"
        result = subprocess.run([str(self.tool), "-cuda", "-tensor-source",
            "-CLIST:dotc_file=" + str(output), str(image)], capture_output=True, text=True)
        return result, output

    def test_unknown_native_operator_is_rejected_without_publication(self):
        image = self.image("unsupported")
        before = hashlib.sha256(image.read_bytes()).hexdigest()
        result, output = self.emit(image)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("unsupported operator/version: common.relu", result.stderr)
        self.assertFalse(output.exists())
        self.assertEqual(hashlib.sha256(image.read_bytes()).hexdigest(), before)

    def test_late_pu_failure_does_not_publish_a_partial_translation(self):
        result, output = self.emit(self.image("multi"))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("common.relu", result.stderr)
        self.assertFalse(output.exists())

    def test_non_mla_program_compiles_and_executes(self):
        result, output = self.emit(self.image("valid"))
        self.assertEqual(result.returncode, 0, result.stderr)
        text = output.read_text()
        self.assertIn("common.sub.v1", text)
        self.assertNotIn("MLA", text)
        formals = re.findall(r"^  (?:const )?(TENSOR<[^>]+>) &(v\d+)[,)]", text, re.M)
        self.assertEqual(len(formals), 2)
        header = Path(os.environ["OPEN64_TENSOR_REFERENCE_HEADER"])
        shutil.copyfile(header, self.root / header.name)
        runner = self.root / "check.cxx"
        runner.write_text('#include "result.cu"\nint main() {\n'
            'TENSOR<float,2,3> a{{1,-2,3,4,5,6}}, b{};\n'
            'open64_reference_unrelated_program_0(a,b);\n'
            'for(float x:b.data) if(x!=0) return 1; return 0; }\n')
        subprocess.run(["g++", "-std=c++17", "-ffp-contract=off", str(runner),
                        "-o", str(self.root / "check")], check=True)
        subprocess.run([str(self.root / "check")], check=True)

    def test_truncated_image_is_rejected(self):
        image = self.root / "broken.B"
        image.write_bytes(b"\x7fELF")
        result, output = self.emit(image)
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(output.exists())

    def test_unrelated_phase_options_are_ignored(self):
        image = self.image("valid")
        output = self.root / "options.cu"
        result = subprocess.run([str(self.tool), "-cuda", "-tensor-source", "-O3",
            "-TARG:abi=64", "-CLIST:dotc_file=" + str(output), str(image)],
            capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(output.exists())

    def test_normal_cuda_keeps_existing_backend_dispatch(self):
        wrapper = self.root / "whirl2c"
        shutil.copyfile(self.tool, wrapper)
        wrapper.chmod(0o755)
        backend = self.root / "whirl2c_be"
        backend.write_text('#!/bin/sh\nprintf "%s\\n" "$@"\n')
        backend.chmod(0o755)
        result = subprocess.run([str(wrapper), "-cuda", "input.B"], capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("-CLIST:cuda=ON", result.stdout)
        self.assertIn("-fB,input.B", result.stdout)


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--produce":
        produce(sys.argv[2], Path(sys.argv[3]))
    else:
        unittest.main()
