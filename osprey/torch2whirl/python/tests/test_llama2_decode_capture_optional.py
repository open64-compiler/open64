from __future__ import annotations

import importlib.util
import inspect
from pathlib import Path
import re
import unittest


TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None

if TORCH_AVAILABLE:
    import torch
    import torch.fx
    from torch.fx.passes.shape_prop import ShapeProp

    from open64_dsc import WhirlExportOptions
    from open64_dsc import verify_module
    from open64_dsc import _mock_whirl
    from open64_dsc.builder import (
        REGION_STATE_LAYER_OWNED,
        REGION_STATE_UNIQUE_OWNERSHIP,
        STATE_EFFECT_MODIFY,
        STATE_MUTABLE_BUFFER,
        STATE_UNIQUE_OWNERSHIP,
    )
    from open64_dsc.interpreter import WhirlExportInterpreter
    from models.llama2_model import create_tiny_llama2
    from models.llama2_model import open64_sample_inputs as prefill_sample_inputs
    from models.llama2_decode_model import OPEN64_SAMPLE_INPUT_PROTOCOL
    from models.llama2_decode_model import TinyLlama2DecodeConfig
    from models.llama2_decode_model import create_tiny_llama2_decode
    from models.llama2_decode_model import open64_sample_inputs
    from models.llama2_decode_model import sample_decode_inputs


GOLDEN_GRAPH = Path(__file__).with_name("golden") / "llama2_decode_fx.txt"
CENSUS = Path(__file__).resolve().parents[2] / "LLAMA2-DECODE-OPERATOR-CENSUS.md"


def _format_target(target: object) -> str:
    if isinstance(target, str):
        return target
    module = getattr(target, "__module__", "")
    name = getattr(target, "__name__", repr(target))
    if module and module != "builtins":
        return f"{module}.{name}"
    return name


def _format_arg(value: object) -> str:
    if TORCH_AVAILABLE and isinstance(value, torch.fx.Node):
        return f"%{value.name}"
    if isinstance(value, tuple):
        return "(" + ",".join(_format_arg(item) for item in value) + ")"
    if isinstance(value, list):
        return "[" + ",".join(_format_arg(item) for item in value) + "]"
    if isinstance(value, slice):
        return f"slice({value.start},{value.stop},{value.step})"
    if isinstance(value, dict):
        items = [f"{key}:{_format_arg(value[key])}" for key in sorted(value)]
        return "{" + ",".join(items) + "}"
    return repr(value)


def _format_meta(meta: object) -> str:
    dtype = getattr(meta, "dtype", None)
    shape = getattr(meta, "shape", None)
    if dtype is not None and shape is not None:
        dtype_name = str(dtype).replace("torch.", "")
        dimensions = ",".join(str(dim) for dim in shape)
        return f"{dtype_name}[{dimensions}]"
    if isinstance(meta, (tuple, list)):
        return "(" + ",".join(_format_meta(item) for item in meta) + ")"
    return "-"


def _module_type(graph_module: "torch.fx.GraphModule", target: object) -> str:
    if not isinstance(target, str):
        return "-"
    try:
        module = graph_module.get_submodule(target)
    except AttributeError:
        return "-"
    return type(module).__name__


def normalized_llama2_decode_fx_graph() -> str:
    config = TinyLlama2DecodeConfig()
    model = create_tiny_llama2_decode(config)
    sample_inputs = sample_decode_inputs(config)
    graph_module = torch.fx.symbolic_trace(model)
    ShapeProp(graph_module).propagate(*sample_inputs)

    lines = [
        "# normalized torch.fx graph: tiny llama2 decode",
        (
            "config=vocab:128 hidden:32 layers:2 heads:4 kv_heads:4 "
            "batch:1 decode_seq:1 cache_len:3 max_seq:8 "
            "dtype:float32 token_dtype:int64 position_dtype:int64 "
            "cache_update:functional_append"
        ),
        "inputs=" + ",".join(OPEN64_SAMPLE_INPUT_PROTOCOL),
    ]
    for ordinal, node in enumerate(graph_module.graph.nodes):
        lines.append(
            "|".join(
                (
                    f"{ordinal:03d}",
                    f"name={node.name}",
                    f"op={node.op}",
                    f"target={_format_target(node.target)}",
                    f"args={_format_arg(node.args)}",
                    f"kwargs={_format_arg(node.kwargs)}",
                    f"type={_format_meta(node.meta.get('tensor_meta'))}",
                    f"module={_module_type(graph_module, node.target)}",
                )
            )
        )
    return "\n".join(lines) + "\n"


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class TinyLlama2DecodeEagerOptionalTest(unittest.TestCase):
    def test_source_model_sample_input_protocol_is_explicit(self) -> None:
        config = TinyLlama2DecodeConfig()
        model = create_tiny_llama2_decode(config)
        parameter_names = tuple(inspect.signature(model.forward).parameters)
        sample_inputs = open64_sample_inputs()

        self.assertEqual(parameter_names, OPEN64_SAMPLE_INPUT_PROTOCOL)
        self.assertEqual(len(sample_inputs), len(OPEN64_SAMPLE_INPUT_PROTOCOL))
        self.assertEqual(tuple(sample_inputs[0].shape), (1, 1))
        self.assertEqual(sample_inputs[0].dtype, torch.int64)
        self.assertEqual(tuple(sample_inputs[1].shape), (1,))
        self.assertEqual(sample_inputs[1].dtype, torch.int64)
        self.assertEqual(sample_inputs[1].item(), config.cache_length)
        for cache in sample_inputs[2:]:
            self.assertEqual(tuple(cache.shape), (1, 4, 3, 8))
            self.assertEqual(cache.dtype, torch.float32)

    def test_decode_is_repeatable_and_functionally_appends_cache(self) -> None:
        config = TinyLlama2DecodeConfig()
        model = create_tiny_llama2_decode(config)
        sample_inputs = sample_decode_inputs(config)
        original_inputs = tuple(value.clone() for value in sample_inputs)

        with torch.no_grad():
            first = model(*sample_inputs)
            second = model(*sample_inputs)

        self.assertEqual(len(first), 5)
        self.assertEqual(tuple(first[0].shape), (1, 1, config.vocab_size))
        self.assertEqual(first[0].dtype, torch.float32)
        for first_value, second_value in zip(first, second):
            self.assertTrue(torch.equal(first_value, second_value))

        for actual, original in zip(sample_inputs, original_inputs):
            self.assertTrue(torch.equal(actual, original))

        for cache_input, cache_output in zip(sample_inputs[2:], first[1:]):
            self.assertEqual(tuple(cache_output.shape), (1, 4, 4, 8))
            self.assertTrue(
                torch.equal(cache_output[:, :, :config.cache_length, :],
                            cache_input)
            )
            self.assertNotEqual(cache_output.data_ptr(), cache_input.data_ptr())

    def test_decode_does_not_mutate_model_state(self) -> None:
        model = create_tiny_llama2_decode()
        before = {
            name: value.detach().clone()
            for name, value in model.state_dict().items()
        }

        with torch.no_grad():
            model(*sample_decode_inputs())

        after = model.state_dict()
        self.assertEqual(set(before), set(after))
        for name, value in before.items():
            self.assertTrue(torch.equal(value, after[name]), name)


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class TinyLlama2DecodeFxDiscoveryOptionalTest(unittest.TestCase):
    def test_normalized_fx_graph_matches_golden(self) -> None:
        self.assertEqual(
            GOLDEN_GRAPH.read_text(encoding="utf-8"),
            normalized_llama2_decode_fx_graph(),
        )

    def test_normalized_fx_graph_drift_is_machine_detectable(self) -> None:
        current = normalized_llama2_decode_fx_graph()
        drifted = current.replace(
            "cache_update:functional_append",
            "cache_update:indexed_update",
        )
        self.assertNotEqual(current, drifted)
        self.assertNotEqual(GOLDEN_GRAPH.read_text(encoding="utf-8"), drifted)

    def test_graph_exposes_decode_state_and_cached_attention_shapes(self) -> None:
        graph = normalized_llama2_decode_fx_graph()
        self.assertIn("name=cache_position|op=placeholder", graph)
        self.assertIn("name=layer0_key_cache|op=placeholder", graph)
        self.assertIn("target=torch.index_select", graph)
        self.assertIn("target=torch.cat", graph)
        self.assertIn("type=float32[1,4,1,4]", graph)
        self.assertIn("type=float32[1,4,4,8]", graph)

    def test_census_classifies_every_normalized_node(self) -> None:
        census = CENSUS.read_text(encoding="utf-8")
        node_ordinals = []
        for line in normalized_llama2_decode_fx_graph().splitlines():
            match = re.match(r"^(\d+)\|name=([^|]+)\|", line)
            if match:
                node_ordinals.append(int(match.group(1)))

        covered_ordinals = []
        for first, last in re.findall(
            r"\| `([0-9]{3})(?:-([0-9]{3}))?` \|",
            census,
        ):
            start = int(first)
            end = int(last) if last else start
            covered_ordinals.extend(range(start, end + 1))

        self.assertGreater(len(node_ordinals), 20)
        self.assertEqual(node_ordinals, covered_ordinals)


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class TinyLlama2DecodeWhirlEmissionOptionalTest(unittest.TestCase):
    def _export_decode(self, config: TinyLlama2DecodeConfig | None = None):
        config = config or TinyLlama2DecodeConfig()
        model = create_tiny_llama2_decode(config)
        interpreter = WhirlExportInterpreter(WhirlExportOptions())
        return interpreter.export(model, sample_decode_inputs(config))

    def test_decode_collects_imported_callable_declarations(self) -> None:
        module = self._export_decode()
        by_name = {
            imported.canonical_name: imported
            for imported in module.python_imports
        }

        rms_norm = by_name["models.llama2_model.TinyRMSNorm.forward"]
        self.assertEqual(
            tuple(rms_norm.import_names),
            ("TinyRMSNorm.forward",),
        )
        self.assertEqual(rms_norm.kind, "method")
        self.assertEqual(len(rms_norm.implementation_fingerprint), 64)
        self.assertIn(
            "models.llama2_model.TinyLlama2FeedForward.forward",
            by_name,
        )
        self.assertIn(
            "models.llama2_model.initialize_tiny_llama2",
            by_name,
        )
        self.assertEqual(
            len(module.to_manifest()["python_imports"]),
            len(module.python_imports),
        )

    def test_decode_emits_v2_rope_and_attention_contracts(self) -> None:
        module = self._export_decode()
        verify_module(module)

        rope = [
            operator for operator in module.graph_operators
            if operator.name == "transformer.rotary_embedding" and
            operator.metadata.get("operator_version") == "2"
        ]
        attention = [
            operator for operator in module.graph_operators
            if operator.name == "transformer.attention" and
            operator.metadata.get("operator_version") == "2"
        ]

        self.assertEqual(len(rope), 4)
        self.assertEqual(len(attention), 2)
        for operator in rope:
            self.assertEqual(len(operator.kids), 4)
            self.assertEqual(operator.kids[3], "input1")
            self.assertEqual(
                operator.attrs["attr.position_mode"],
                "explicit_operand",
            )
            self.assertNotIn("attr.position_offset", operator.attrs)
        for layer_index, operator in enumerate(attention):
            self.assertEqual(len(operator.kids), 3)
            self.assertEqual(operator.kids[0], "attention_q_rope")
            self.assertEqual(operator.kids[1], f"input{2 + layer_index * 2}")
            self.assertEqual(operator.kids[2], f"input{3 + layer_index * 2}")
            self.assertEqual(
                operator.attrs["attr.execution_mode"],
                "single_token_decode",
            )
            self.assertEqual(
                operator.attrs["attr.mask_mode"],
                "implicit_prefix_causal",
            )
            self.assertEqual(
                operator.attrs["attr.cache_mode"],
                "functional_append",
            )
            self.assertEqual(operator.attrs["attr.cache_sequence_axis"], "2")

    def test_decode_region_declares_two_unique_layer_states(self) -> None:
        module = self._export_decode()
        program_unit = module.entry_function.handle
        regions = [
            record for record in _mock_whirl._objects.values()
            if record.get("kind") == "region" and
            record.get("program_unit") == program_unit and
            record.get("contract_name") == "transformer.decoder_layer" and
            record.get("contract_version") == 2
        ]

        self.assertEqual(len(regions), 2)
        for layer_index, region in enumerate(regions):
            states = list(region.get("states", ()))
            self.assertEqual(len(states), 2)
            self.assertEqual(states[0][2], 0)
            self.assertEqual(states[1][2], 1)
            self.assertEqual(states[0][1], STATE_EFFECT_MODIFY)
            self.assertEqual(states[1][1], STATE_EFFECT_MODIFY)
            self.assertEqual(
                states[0][3],
                REGION_STATE_UNIQUE_OWNERSHIP | REGION_STATE_LAYER_OWNED,
            )
            self.assertEqual(
                states[1][3],
                REGION_STATE_UNIQUE_OWNERSHIP | REGION_STATE_LAYER_OWNED,
            )
            state_records = [
                _mock_whirl._objects[states[0][0]],
                _mock_whirl._objects[states[1][0]],
            ]
            self.assertEqual(
                state_records[0]["name"], f"layer{layer_index}.key_cache"
            )
            self.assertEqual(
                state_records[1]["name"], f"layer{layer_index}.value_cache"
            )
            for state_record in state_records:
                self.assertEqual(
                    state_record["state_kind"], STATE_MUTABLE_BUFFER
                )
                self.assertEqual(state_record["flags"], STATE_UNIQUE_OWNERSHIP)

            modified_values = [
                _mock_whirl._objects[value]
                for value in region.get("values", ())
                if _mock_whirl._objects[value].get("state_effects")
            ]
            self.assertEqual(len(modified_values), 1)
            self.assertEqual(
                modified_values[0]["opcode_name"],
                "transformer.attention",
            )
            self.assertEqual(modified_values[0]["version"], 2)
            self.assertEqual(
                modified_values[0]["state_effects"],
                [
                    (states[0][0], STATE_EFFECT_MODIFY),
                    (states[1][0], STATE_EFFECT_MODIFY),
                ],
            )

    def test_decode_rejects_unsupported_profiles(self) -> None:
        with self.assertRaisesRegex(NotImplementedError, "grouped-query"):
            self._export_decode(TinyLlama2DecodeConfig(num_kv_heads=2))
        with self.assertRaisesRegex(NotImplementedError, "one-token decode"):
            self._export_decode(TinyLlama2DecodeConfig(decode_sequence_length=2))
        with self.assertRaisesRegex(NotImplementedError, "non-empty prefix"):
            self._export_decode(TinyLlama2DecodeConfig(cache_length=0))
        with self.assertRaisesRegex(NotImplementedError, "RoPE capacity"):
            self._export_decode(
                TinyLlama2DecodeConfig(cache_length=8, max_sequence_length=8)
            )

    def test_decode_rejects_bad_position_and_cache_shapes(self) -> None:
        config = TinyLlama2DecodeConfig()
        model = create_tiny_llama2_decode(config)
        inputs = list(sample_decode_inputs(config))
        inputs[1] = torch.tensor([config.cache_length, config.cache_length + 1])
        interpreter = WhirlExportInterpreter(WhirlExportOptions())
        with self.assertRaisesRegex(ValueError, "ordinal 1"):
            interpreter.export(model, tuple(inputs))

        inputs = list(sample_decode_inputs(config))
        inputs[2] = torch.zeros((1, 4, 3, 7), dtype=torch.float32)
        interpreter = WhirlExportInterpreter(WhirlExportOptions())
        with self.assertRaisesRegex(ValueError, "ordinal 2"):
            interpreter.export(model, tuple(inputs))

    def test_prefill_emission_remains_version_one(self) -> None:
        model = create_tiny_llama2()
        interpreter = WhirlExportInterpreter(WhirlExportOptions())
        module = interpreter.export(model, prefill_sample_inputs())
        verify_module(module)

        self.assertEqual(module.graph_source, "torch.fx+llama2_semantic")
        for operator in module.graph_operators:
            if operator.name in {
                "transformer.rotary_embedding",
                "transformer.attention",
            }:
                self.assertNotEqual(
                    operator.metadata.get("operator_version"),
                    "2",
                )
        attention = [
            operator for operator in module.graph_operators
            if operator.name == "transformer.attention"
        ]
        self.assertTrue(attention)
        self.assertTrue(
            all(
                operator.attrs["attr.cache_mode"] == "none"
                for operator in attention
            )
        )


if __name__ == "__main__":
    unittest.main()
