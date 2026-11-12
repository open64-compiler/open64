"""Direct native preflight checks, independent of FX and the Python verifier."""

import importlib.util
import unittest

from open64_dsc import load_builder

AVAILABLE = importlib.util.find_spec("open64_dsc._whirl") is not None


@unittest.skipUnless(AVAILABLE, "native extension is not built")
class TensorExpressionNativeTests(unittest.TestCase):
    def setUp(self):
        from open64_dsc import _whirl
        self.native = _whirl
        self.builder = load_builder("native")
        self.builder.begin_program()
        self.pu = self.builder.minimal_program_unit("tensor_expression_boundary")
        self.file = self.builder.register_source_file(self.pu, __file__)
        self.ordinal = 0
        self.next_formal = 0

    def tearDown(self):
        self.builder.abort_program()

    def tensor(self, dtype, shape, formal=True):
        self.ordinal += 1
        name = "tensor%d" % self.ordinal
        ty = self.builder.tensor_type(name, dtype, len(shape),
            "[" + ",".join(map(str, shape)) + "]", {"layout": "logical"})
        value = self.builder.declare_pu_formal(self.pu, name, self.next_formal, ty,
                                              self.file, 1) if formal else None
        if formal:
            self.next_formal += 1
        return ty, value

    def create(self, name, kids, attrs, result_ty, version=1):
        return self.native.create_operator_with_result(
            name, version, [kid.value for kid in kids], attrs, "result", result_ty.value)

    def test_native_elementary_float_predicate_and_reduction_types(self):
        f, x = self.tensor("float32", (1, 3, 17))
        b, mask = self.tensor("bool", (1, 3, 17))
        reduced, _ = self.tensor("float32", (1, 3, 1), False)
        broadcast = {"attr.broadcast_rule": "numpy"}
        for name, kids, attrs, ty in (
            ("common.sub", [x, x], broadcast, f),
            ("common.exp", [x], {"attr.fp_semantics": "source_unresolved"}, f),
            ("common.log", [x], {"attr.fp_semantics": "source_unresolved"}, f),
            ("common.isfinite", [x], {}, b),
            ("common.logical_not", [mask], {}, b),
            ("common.greater", [x, x], broadcast, b),
            ("common.select", [mask, x, x], broadcast, f),
            ("common.fill_like", [x], {"attr.value_bits": "ff800000"}, f),
        ):
            with self.subTest(name=name):
                self.assertGreater(self.create(name, kids, attrs, ty), 0)
        attrs = {"attr.axis": "2", "attr.keepdim": "true",
                 "attr.accum_dtype": "float32", "attr.reassociate": "false"}
        self.assertGreater(self.create("common.reduce_max", [x], attrs, reduced), 0)
        self.assertGreater(self.create("common.reduce_sum", [x], attrs, reduced, 3), 0)

    def test_native_rejects_schema_dtype_shape_and_noncanonical_literals(self):
        f, x = self.tensor("float32", (1, 3, 17))
        b, mask = self.tensor("bool", (1, 3, 17))
        wrong, _ = self.tensor("float32", (1, 4, 17), False)
        for name, kids, attrs, ty, version in (
            ("common.exp", [mask], {"attr.fp_semantics": "source_unresolved"}, f, 1),
            ("common.exp", [x], {"attr.fp_semantics": "fast"}, f, 1),
            ("common.exp", [x], {"attr.fp_semantics": "source_unresolved", "extra": "true"}, f, 1),
            ("common.exp", [x], {"attr.fp_semantics": "source_unresolved"}, wrong, 1),
            ("common.exp", [x], {"attr.fp_semantics": "source_unresolved"}, f, 2),
            ("common.greater", [x, x], {"attr.broadcast_rule": "numpy"}, f, 1),
            ("common.select", [x, x, x], {"attr.broadcast_rule": "numpy"}, f, 1),
            ("common.fill_like", [x], {"attr.value_bits": "7fc00000"}, f, 1),
            ("common.fill_like", [x], {"attr.value_bits": "FF800000"}, f, 1),
            ("common.logical_not", [mask, mask], {}, b, 1),
        ):
            with self.subTest(name=name, version=version), self.assertRaises(RuntimeError):
                self.create(name, kids, attrs, ty, version)
        self.assertGreater(self.create("common.isfinite", [x], {}, b), 0)

    def test_native_broadcast_matmul_checks_rank_padding_and_contraction(self):
        _, q = self.tensor("float32", (2, 3, 1, 8))
        _, w = self.tensor("float32", (3, 8, 16))
        result, _ = self.tensor("float32", (2, 3, 1, 16), False)
        attrs = {"attr.transpose_kid0": "false", "attr.transpose_kid1": "false",
                 "attr.batch_rule": "numpy", "attr.accum_dtype": "float32"}
        self.assertGreater(self.create("common.matmul", [q, w], attrs, result, 3), 0)
        _, bad = self.tensor("float32", (5, 8, 16))
        with self.assertRaises(RuntimeError):
            self.create("common.matmul", [q, bad], attrs, result, 3)
        with self.assertRaises(RuntimeError):
            self.create("common.matmul", [q, w], dict(attrs, **{"attr.batch_rule": "prefix"}), result, 3)

    def test_native_reduction_rejects_axis_and_result_shape(self):
        f, x = self.tensor("float32", (1, 3, 17))
        attrs = {"attr.axis": "2", "attr.keepdim": "true",
                 "attr.accum_dtype": "float32", "attr.reassociate": "false"}
        for name, version in (("common.reduce_max", 1), ("common.reduce_sum", 3)):
            with self.subTest(name=name), self.assertRaises(RuntimeError):
                self.create(name, [x], attrs, f, version)


if __name__ == "__main__":
    unittest.main()
