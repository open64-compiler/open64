"""Focused PU-local tensor lowering admission and generic executable controls."""
import copy
import hashlib
import json
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import tempfile
import unittest

from mla_kernel_review import runner


def produce(kind, path):
    """Create a source-independent native tensor PU in an isolated process."""
    from open64_dsc import load_builder
    from open64_dsc.backend import load_backend
    b = load_builder('native')
    b.begin_program()
    pu = b.minimal_program_unit('independent_tensor_program')
    file = b.register_source_file(pu, __file__)
    dtype = 'bool' if kind == 'boolean' else 'float32'
    shape = (2, 1, 3, 5) if kind == 'batch-matmul' else (2, 3)
    descriptor = {'layout': 'column_major' if kind == 'strided' else 'logical'}
    if kind == 'annotated': descriptor['traits'] = 'test.requires_extra_semantics'
    ty = b.tensor_type('input_type', dtype, len(shape), '[' + ','.join(map(str, shape)) + ']', descriptor)
    x = b.declare_pu_formal(pu, 'input', 0, ty, file, 1)
    if kind == 'batch-matmul':
        other = b.tensor_type('weight_type', 'float32', 3, '[4,5,2]', {'layout': 'logical'})
        y = b.declare_pu_formal(pu, 'weight', 1, other, file, 1)
        out = b.tensor_type('result_type', 'float32', 4, '[2,4,3,2]', {'layout': 'logical'})
        op = b.operator('common.matmul', 3, [x, y], {'attr.transpose_kid0': 'false', 'attr.transpose_kid1': 'false',
            'attr.batch_rule': 'numpy', 'attr.accum_dtype': 'float32'}, 'product', out)
    elif kind == 'unsupported':
        op, out = b.common_relu(x), ty
    elif kind == 'maximum':
        out = b.tensor_type('maximum_type', 'float32', 2, '[2,1]', {'layout': 'logical'})
        op = b.operator('common.reduce_max', 1, [x], {'attr.axis': '1', 'attr.keepdim': 'true',
            'attr.accum_dtype': 'float32', 'attr.reassociate': 'false'}, 'maximum', out)
    elif kind == 'boolean':
        op, out = b.operator('common.logical_not', 1, [x], {}, 'inverse', ty), ty
    else:
        op, out = b.operator('common.sub', 1, [x, x], {'attr.broadcast_rule': 'numpy'}, 'difference', ty), ty
    b.set_value_source_position(op, file, 1)
    b.append_program_unit_value(pu, op)
    b.declare_pu_result(pu, 'answer', 0, out, file_id=file, line=1)
    b.return_pu_values(pu, [op])
    b.verify_program()
    assert load_backend('native').finalize_mapped_image(str(path), {})


@unittest.skipUnless(os.environ.get('OPEN64_TENSOR_LOWERER'), 'native lowering tools are not configured')
class TensorKernelTests(unittest.TestCase):
    def setUp(self):
        durable = os.environ.get('OPEN64_TENSOR_UNIT_ARTIFACT_DIR')
        if durable:
            Path(durable).mkdir(parents=True, exist_ok=True)
            self.root = Path(tempfile.mkdtemp(prefix=self._testMethodName + '-', dir=durable))
        else:
            temp = tempfile.TemporaryDirectory()
            self.addCleanup(temp.cleanup)
            self.root = Path(temp.name)
        self.lowerer = Path(os.environ['OPEN64_TENSOR_LOWERER'])
        self.reader = Path(os.environ['OPEN64_IR_B2A'])
        self.w2c = Path(os.environ['OPEN64_WHIRL2C_DEVICE'])

    def run_tool(self, *args, success=True):
        result = subprocess.run(list(map(str, args)), capture_output=True, text=True)
        with (self.root / 'commands.jsonl').open('a') as out:
            out.write(json.dumps(dict(argv=list(map(str, args)), returncode=result.returncode,
                                     stdout=result.stdout, stderr=result.stderr)) + '\n')
        self.assertEqual(result.returncode == 0, success, result.stderr)
        return result

    def fixture(self, kind='valid', level=3):
        image = self.root / (kind + '.B')
        self.run_tool(sys.executable, __file__, '--produce', kind, image)
        recipe = self.root / (kind + '.json')
        before = image.read_bytes()
        self.run_tool(self.lowerer, 'prepare', image, recipe, '--verify', level)
        self.assertEqual(image.read_bytes(), before)
        return image, recipe

    def test_invalid_recipes_refuse_without_checkpoints(self):
        _, recipe = self.fixture()
        base = json.loads(recipe.read_text())
        changes = [lambda p: p.update(threads=64), lambda p: p.update(extra=True),
            lambda p: p['values'][0]['shape'].__setitem__(0, 0),
            lambda p: p['values'][0]['shape'].__setitem__(0, 2**63),
            lambda p: p['values'][0].update(dtype='float16'),
            lambda p: p['operations'][0].update(version=999),
            lambda p: p['operations'][0]['attributes'].update(extra='true'),
            lambda p: p['operations'][0].update(inputs=[999, 0]),
            lambda p: p['operations'][0].update(grid_x=999),
            lambda p: p['operations'][0].update(kernel='different_kernel'),
            lambda p: p['values'][2].update(storage=0),
            lambda p: p.update(source_file='')]
        for i, change in enumerate(changes):
            with self.subTest(case=i):
                p = copy.deepcopy(base)
                change(p)
                path = self.root / ('bad%d.json' % i)
                path.write_text(json.dumps(p))
                before, after = self.root / ('bad%d.before.B' % i), self.root / ('bad%d.after.B' % i)
                self.run_tool(self.lowerer, 'device', path, before, after, success=False)
                self.assertFalse(before.exists() or after.exists())

    def test_duplicate_keys_and_trailing_json_reject(self):
        _, recipe = self.fixture()
        text = recipe.read_text()
        for i, malformed in enumerate((text + '{}', text.replace('"threads" : 128', '"threads" : 128,"threads" : 128'))):
            self.assertNotEqual(text, malformed)
            path = self.root / ('bad%d.json' % i)
            path.write_text(malformed)
            self.run_tool(self.lowerer, 'device', path, self.root / ('b%d.B' % i), self.root / ('a%d.B' % i), success=False)

    def test_unsupported_source_has_no_recipe_or_source_mutation(self):
        image = self.root / 'unsupported.B'
        self.run_tool(sys.executable, __file__, '--produce', 'unsupported', image)
        original = hashlib.sha256(image.read_bytes()).hexdigest()
        recipe = self.root / 'no.json'
        self.run_tool(self.lowerer, 'prepare', image, recipe, '--verify', 3, success=False)
        self.assertFalse(recipe.exists())
        self.assertEqual(hashlib.sha256(image.read_bytes()).hexdigest(), original)

    def test_physical_layout_is_explicitly_rejected(self):
        image = self.root / 'strided.B'
        self.run_tool(sys.executable, __file__, '--produce', 'strided', image)
        result = self.run_tool(self.lowerer, 'prepare', image, self.root / 'no.json', '--verify', 3, success=False)
        self.assertIn('unsupported physical tensor layout/strides', result.stderr)
        self.assertFalse((self.root / 'no.json').exists())

    def test_valid_but_changed_handoff_identity_is_rejected(self):
        _, recipe = self.fixture()
        p = json.loads(recipe.read_text())
        p['source_pu'] += '_changed'
        stale = self.root / 'stale.json'
        stale.write_text(json.dumps(p))
        result = self.run_tool(self.lowerer, 'device', stale, self.root / 'b.B', self.root / 'a.B', success=False)
        self.assertIn('noncanonical or stale launch recipe', result.stderr)
        self.assertFalse((self.root / 'b.B').exists())

    def test_unimplemented_tensor_traits_are_not_silently_dropped(self):
        image = self.root / 'annotated.B'
        self.run_tool(sys.executable, __file__, '--produce', 'annotated', image)
        result = self.run_tool(self.lowerer, 'prepare', image, self.root / 'no.json', '--verify', 3, success=False)
        self.assertIn('unsupported tensor trait/representation field', result.stderr)
        self.assertFalse((self.root / 'no.json').exists())

    def execute_generic(self, kind, level):
        import difflib
        import torch
        _, recipe = self.fixture(kind, level)
        before, after = self.root / 'device.before.B', self.root / 'device.after.B'
        self.run_tool(self.lowerer, 'device', recipe, before, after)
        for image in (before, after):
            self.run_tool(self.reader, '-st', '-src', image, image.with_suffix('.T'))
        (self.root / 'device.before-after.diff').write_text(''.join(difflib.unified_diff(
            before.with_suffix('.T').read_text().splitlines(True), after.with_suffix('.T').read_text().splitlines(True))))
        manifest = self.root / 'launch.json'
        self.run_tool(self.lowerer, 'audit', after, recipe, manifest)
        launch = json.loads(manifest.read_text())
        self.assertEqual(len(launch['kernels']), 2)
        self.assertTrue(all(int(x['block_x']) == (128 if level == 3 else 1) for x in launch['kernels']))
        self.run_tool(self.w2c, '-cuda', '-TARG:abi=n64', '-O0',
            '-CLIST:dotc_file=' + str(self.root / 'kernels.cu'), '-CLIST:doth_file=' + str(self.root / 'kernels.h'), after)
        stub = self.root / 'cuda_runtime.h'
        stub.write_text('#include <cmath>\n#include <cstring>\n#define __global__\n'
            'struct dim3 {unsigned x=0,y=0,z=0;}; static dim3 threadIdx,blockIdx;\n'
            'static float __uint_as_float(unsigned x) {float f; memcpy(&f,&x,4); return f;}\n')
        (self.root / 'host.cxx').write_text(runner(launch, False))
        self.run_tool('g++', '-std=c++17', '-O1', '-ffp-contract=off', '-I' + str(self.root),
                      self.root / 'host.cxx', '-o', self.root / 'host')
        if kind == 'boolean':
            data = bytes((0, 1, 1, 0, 0, 1))
            expected = bytes((1, 0, 0, 1, 1, 0))
        elif kind == 'batch-matmul':
            a = (torch.arange(30).remainder(7) - 3).float().reshape(2, 1, 3, 5)
            b = (torch.arange(40).remainder(5) - 2).float().reshape(4, 5, 2)
            data = a.numpy().tobytes() + b.numpy().tobytes()
            expected = torch.matmul(a, b).numpy().tobytes()
        elif kind == 'maximum':
            data = struct.pack('<6f', 1, float('nan'), 3, float('-inf'), -5, -2)
            expected = None
        else:
            data = struct.pack('<6f', 1, -2, 3, 4, 5, 6)
            expected = b'\0' * 24
        (self.root / 'input.bin').write_bytes(data)
        self.run_tool(self.root / 'host', self.root / 'input.bin', self.root / 'output.bin')
        if expected is None:
            import math
            a, b = struct.unpack('<2f', (self.root / 'output.bin').read_bytes())
            self.assertTrue(math.isnan(a))
            self.assertEqual(b, -2.)
        else:
            self.assertEqual((self.root / 'output.bin').read_bytes(), expected)
        old = after.read_bytes()
        self.run_tool(self.lowerer, 'device', recipe, before, after, success=False)
        self.assertEqual(after.read_bytes(), old)

    def test_non_mla_program_executes_serial(self): self.execute_generic('valid', 0)
    def test_non_mla_program_executes_parallel(self): self.execute_generic('valid', 3)
    def test_O1_keeps_serial_semantics(self): self.execute_generic('valid', 1)
    def test_O2_keeps_serial_semantics(self): self.execute_generic('valid', 2)
    def test_byte_boolean_interface_executes(self): self.execute_generic('boolean', 3)
    def test_unequal_batch_broadcast_matmul_executes(self): self.execute_generic('batch-matmul', 3)
    def test_maximum_propagates_nan(self): self.execute_generic('maximum', 3)


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == '--produce':
        produce(sys.argv[2], Path(sys.argv[3]))
    else:
        unittest.main()
