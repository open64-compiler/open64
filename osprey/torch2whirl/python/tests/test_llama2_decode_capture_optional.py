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


if __name__ == "__main__":
    unittest.main()
