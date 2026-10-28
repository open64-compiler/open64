"""Explicit native v2 boundary tests, not a new Python source matcher.

Design: doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md.
The C++ producer/PREOPT review remains the mapped-image authority.
"""
import ctypes
import importlib.util
import unittest

from open64_dsc import load_builder


AVAILABLE = importlib.util.find_spec("open64_dsc._whirl") is not None
ATTRS = dict(zip(
    ("attr.axis attr.accum_dtype attr.variance_kind attr.epsilon_bits "
     "attr.reassociate attr.state_reformulation attr.contract attr.rsqrt_semantics "
     "attr.mean_order attr.mean_identity_bits attr.mean_finalize attr.count_conversion "
     "attr.fp_environment attr.nan_policy attr.reference_binding").split(),
    ("1 float32 population_centered 3727c5ac false false false "
     "sqrt_rne_then_div_rne_f32 ascending_left_fold 00000000 sum_then_div_rne "
     "exact_f32_from_static_n rne_gradual_no_traps class_only ln_ref_1").split()))


@unittest.skipUnless(AVAILABLE, "open64_dsc._whirl is not built")
class NormalizationRefNativeTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        """Create ordinary runtime formals in one active native PU."""
        from open64_dsc import _whirl
        cls.native = _whirl
        cls.builder = load_builder("native")
        cls.builder.begin_program()
        pu = cls.builder.minimal_program_unit("explicit_ref_boundary")
        file_id = cls.builder.register_source_file(pu, __file__)
        cls.x_ty = cls.builder.tensor_type("ref_x_type", "float32", 2, "[3,17]", {"layout": "row_major"})
        affine = cls.builder.tensor_type("ref_affine_type", "float32", 1, "[17]", {"layout": "row_major"})
        cls.kids = [cls.builder.declare_pu_formal(pu, name, i, ty, file_id, 1).value
                    for i, (name, ty) in enumerate((("x", cls.x_ty), ("gamma", affine), ("beta", affine)))]

    @classmethod
    def tearDownClass(cls):
        """Abort the single native program after all boundary checks."""
        cls.builder.abort_program()

    def test_exact_explicit_v2_and_v1_schema_separation(self):
        """Only explicit complete semantics admit v2; v1 never gains the binding."""
        create = self.native.create_operator_with_result
        self.assertGreater(create("common.mean_variance_norm", 2, self.kids, ATTRS, "ref", self.x_ty.value), 0)
        v1 = dict(list(ATTRS.items())[:8], **{"attr.rsqrt_semantics": "source_unresolved"})
        self.assertGreater(create("common.mean_variance_norm", 1, self.kids, v1, "v1", self.x_ty.value), 0)
        for version, attrs in ((1, ATTRS), (2, v1), (3, ATTRS)):
            with self.subTest(version=version, fields=len(attrs)), self.assertRaises(RuntimeError):
                create("common.mean_variance_norm", version, self.kids, attrs, "bad", self.x_ty.value)

    def test_python_versions_are_not_masked(self):
        """Reject oversized integers before either unsigned conversion can alias v2."""
        for version in (0, 65538, 2**32 + 2, 2**64 + 2, -65534, True, "2", 2.0):
            with self.subTest(version=version):
                with self.assertRaises((ValueError, OverflowError, TypeError)):
                    self.native.create_operator("common.mean_variance_norm", version, self.kids, ATTRS)
                with self.assertRaises((ValueError, OverflowError, TypeError)):
                    self.native.create_operator_with_result("common.mean_variance_norm", version,
                        self.kids, ATTRS, "bad", self.x_ty.value)

    def test_c_abi_rejects_uint16_alias(self):
        """Exercise exported C entry points directly, bypassing Python validation."""
        class Attribute(ctypes.Structure):
            _fields_ = [("name", ctypes.c_char_p), ("value", ctypes.c_char_p)]
        library = ctypes.CDLL(self.native.__file__)
        handle = ctypes.c_ulonglong
        kids = (handle * 3)(*self.kids)
        attrs = (Attribute * 15)(*(Attribute(k.encode(), v.encode()) for k, v in ATTRS.items()))
        base = [ctypes.c_char_p, ctypes.c_uint, ctypes.POINTER(handle), ctypes.c_uint,
                ctypes.POINTER(Attribute), ctypes.c_uint]
        inferred = library.Open64_DSC_Create_Operator
        inferred.argtypes, inferred.restype = base, handle
        explicit = library.Open64_DSC_Create_Operator_With_Result
        explicit.argtypes, explicit.restype = base + [ctypes.c_char_p, handle], handle
        for version in (0, 65538, 0xffffffff):
            with self.subTest(version=version):
                args = (b"common.mean_variance_norm", version, kids, 3, attrs, 15)
                self.assertEqual(inferred(*args), 0)
                self.assertEqual(explicit(*args, b"bad_c", self.x_ty.value), 0)
        self.assertGreater(explicit(b"common.mean_variance_norm", 2, kids, 3, attrs, 15,
                                    b"valid_c", self.x_ty.value), 0)


if __name__ == "__main__":
    unittest.main()
