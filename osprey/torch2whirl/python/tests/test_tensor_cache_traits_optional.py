"""Explicit cache roles, alias propagation and missing-annotation fallback."""
import importlib.util
import unittest

AVAILABLE = importlib.util.find_spec("torch") is not None
if AVAILABLE:
    import torch
    from open64_dsc import export_to_whirl, WhirlExportOptions
    from open64_dsc.tensor_traits import LATENT_KV_CACHE, ROTARY_KEY_CACHE
    from models.absorbed_mla import make_fixture


@unittest.skipUnless(AVAILABLE, "requires optional PyTorch")
class CacheTraitCaptureTests(unittest.TestCase):
    def export(self, declarations=None):
        model, inputs = make_fixture(length=7)
        if declarations is not None:
            model.__open64_tensor_traits__ = declarations
        return export_to_whirl(model, inputs, WhirlExportOptions(backend="mock"))

    def test_explicit_roles_and_only_storage_aliases_inherit(self):
        module = self.export()
        types = {t.name[:-5]: t.descriptor for t in module.tensor_types}
        self.assertEqual(types["cache"]["traits"], LATENT_KV_CACHE)
        self.assertEqual(types["rope_cache"]["traits"], ROTARY_KEY_CACHE)
        traits = {name: desc.get("traits") for name, desc in types.items()}
        for op in module.graph_operators:
            name = op.metadata["semantic_name"]
            if op.name == "common.reshape":
                self.assertEqual(traits[name], traits[op.kids[0]])
            else:
                self.assertIsNone(traits[name], name)
        self.assertEqual(len(module.graph_operators), 46)

    def test_missing_annotations_keep_ordinary_input_path(self):
        module = self.export({})
        self.assertTrue(all("traits" not in t.descriptor for t in module.tensor_types))
        self.assertEqual(len(module.graph_operators), 46)
        self.assertFalse(module.tensor_payloads)

    def test_bad_declarations_are_explicit_errors(self):
        for declarations in ({"unknown": LATENT_KV_CACHE},
                             {"cache": "attention.unknown.v1"},
                             {"allowed": LATENT_KV_CACHE},
                             {"cache": [LATENT_KV_CACHE]},
                             [LATENT_KV_CACHE]):
            with self.subTest(declarations=declarations), self.assertRaises(ValueError):
                self.export(declarations)
        self.export()  # Failed sessions must not poison later exports.

    def test_source_argument_names_require_explicit_declaration(self):
        class Renamed(torch.nn.Module):
            __open64_tensor_traits__ = {"opaque": LATENT_KV_CACHE}

            def forward(self, opaque):
                return (opaque.unsqueeze(1) + 1).squeeze(1)

        module = export_to_whirl(Renamed(), (torch.ones(1, 7, 16),),
                                 WhirlExportOptions(backend="mock"))
        self.assertEqual(module.tensor_types[0].descriptor["traits"], LATENT_KV_CACHE)

    def test_equal_cache_types_do_not_merge_distinct_input_values(self):
        class TwoCaches(torch.nn.Module):
            __open64_tensor_traits__ = {"first": LATENT_KV_CACHE,
                                       "second": LATENT_KV_CACHE}

            def forward(self, first, second):
                return first.unsqueeze(1) + second.unsqueeze(1)

        module = export_to_whirl(TwoCaches(), (torch.ones(1, 7, 16), torch.ones(1, 7, 16)),
                                 WhirlExportOptions(backend="mock"))
        formals = [v for v in module.values if v.value_kind == "formal"]
        self.assertEqual(len(formals), 2)
        self.assertNotEqual(formals[0].handle, formals[1].handle)
        self.assertEqual(module.tensor_types[0].descriptor, module.tensor_types[1].descriptor)

    def test_rank_and_unsupported_lane_are_not_silently_accepted(self):
        class RankTwo(torch.nn.Module):
            __open64_tensor_traits__ = {"cache": LATENT_KV_CACHE}

            def forward(self, cache):
                return (cache.unsqueeze(1) + 1).squeeze(1)

        with self.assertRaises(ValueError):
            export_to_whirl(RankTwo(), (torch.ones(7, 16),), WhirlExportOptions())
        model, inputs = make_fixture(length=7)
        with self.assertRaises(NotImplementedError):
            export_to_whirl(model, inputs, WhirlExportOptions(pu_mode="multiple"))


if __name__ == "__main__":
    unittest.main()
