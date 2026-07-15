from __future__ import annotations

import importlib.util
from pathlib import Path
import re
import unittest


TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None

if TORCH_AVAILABLE:
    import torch
    import torch.fx
    from torch.fx.passes.shape_prop import ShapeProp

    from models.llama2_model import TinyLlama2Config
    from models.llama2_model import create_tiny_llama2
    from models.llama2_model import sample_input_ids


GOLDEN_GRAPH = Path(__file__).with_name("golden") / "llama2_prefill_fx.txt"
CENSUS = Path(__file__).resolve().parents[2] / "LLAMA2-OPERATOR-CENSUS.md"


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


def _format_tensor_meta(node: "torch.fx.Node") -> str:
    meta = node.meta.get("tensor_meta")
    if meta is None:
        return "-"
    dtype = str(meta.dtype).replace("torch.", "")
    shape = ",".join(str(dim) for dim in meta.shape)
    return f"{dtype}[{shape}]"


def _module_type(graph_module: "torch.fx.GraphModule", target: object) -> str:
    if not isinstance(target, str):
        return "-"
    try:
        module = graph_module.get_submodule(target)
    except AttributeError:
        return "-"
    return type(module).__name__


def normalized_llama2_fx_graph() -> str:
    config = TinyLlama2Config()
    model = create_tiny_llama2(config)
    input_ids = sample_input_ids(config)
    graph_module = torch.fx.symbolic_trace(model)
    ShapeProp(graph_module).propagate(input_ids)

    lines = [
        "# normalized torch.fx graph: tiny llama2 prefill",
        (
            "config=vocab:128 hidden:32 layers:2 heads:4 "
            "kv_heads:4 batch:1 seq:8 dtype:float32 token_dtype:int64"
        ),
    ]
    for ordinal, node in enumerate(graph_module.graph.nodes):
        args = _format_arg(node.args)
        kwargs = _format_arg(node.kwargs)
        lines.append(
            "|".join(
                (
                    f"{ordinal:03d}",
                    f"name={node.name}",
                    f"op={node.op}",
                    f"target={_format_target(node.target)}",
                    f"args={args}",
                    f"kwargs={kwargs}",
                    f"type={_format_tensor_meta(node)}",
                    f"module={_module_type(graph_module, node.target)}",
                )
            )
        )
    return "\n".join(lines) + "\n"


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class TinyLlama2EagerOptionalTest(unittest.TestCase):
    def test_profile_matches_first_certified_shape_contract(self) -> None:
        config = TinyLlama2Config()

        self.assertEqual(config.vocab_size, 128)
        self.assertEqual(config.hidden_size, 32)
        self.assertEqual(config.num_layers, 2)
        self.assertEqual(config.num_attention_heads, 4)
        self.assertEqual(config.num_kv_heads, config.num_attention_heads)
        self.assertEqual(config.batch_size, 1)
        self.assertEqual(config.sequence_length, 8)
        self.assertEqual(config.head_dim, 8)

    def test_eager_logits_shape_dtype_and_repeatability(self) -> None:
        config = TinyLlama2Config()
        model = create_tiny_llama2(config)
        input_ids = sample_input_ids(config)

        self.assertFalse(model.training)
        self.assertEqual(input_ids.dtype, torch.int64)
        self.assertEqual(tuple(input_ids.shape), (1, 8))

        with torch.no_grad():
            first = model(input_ids)
            second = model(input_ids)

        self.assertEqual(first.dtype, torch.float32)
        self.assertEqual(tuple(first.shape), (1, 8, config.vocab_size))
        self.assertTrue(torch.equal(first, second))

    def test_eager_forward_does_not_mutate_state_or_allocate_cache(self) -> None:
        config = TinyLlama2Config()
        model = create_tiny_llama2(config)
        input_ids = sample_input_ids(config)
        before = {
            name: tensor.detach().clone()
            for name, tensor in model.state_dict().items()
        }

        with torch.no_grad():
            model(input_ids)

        after = model.state_dict()
        self.assertEqual(set(before), set(after))
        for name, tensor in before.items():
            self.assertTrue(torch.equal(tensor, after[name]), name)

        self.assertTrue(before)
        self.assertFalse(any("cache" in name for name in before))
        self.assertIn("token_embedding.weight", before)
        self.assertIn("layers.0.attention.wq.weight", before)
        self.assertIn("layers.1.feed_forward.down_proj.weight", before)
        self.assertIn("norm.weight", before)
        self.assertIn("output.weight", before)


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class TinyLlama2FxDiscoveryOptionalTest(unittest.TestCase):
    def test_normalized_fx_graph_matches_golden(self) -> None:
        self.assertEqual(GOLDEN_GRAPH.read_text(encoding="utf-8"),
                         normalized_llama2_fx_graph())

    def test_normalized_fx_graph_drift_is_machine_detectable(self) -> None:
        current = normalized_llama2_fx_graph()
        drifted = current.replace("target=token_embedding", "target=tok_embed")
        self.assertNotEqual(current, drifted)
        self.assertNotEqual(GOLDEN_GRAPH.read_text(encoding="utf-8"), drifted)

    def test_census_classifies_every_normalized_node(self) -> None:
        census = CENSUS.read_text(encoding="utf-8")
        node_names = []
        for line in normalized_llama2_fx_graph().splitlines():
            match = re.match(r"^\d+\|name=([^|]+)\|", line)
            if match:
                node_names.append(match.group(1))
        self.assertGreater(len(node_names), 20)
        for name in node_names:
            self.assertIn(f"| `{name}` |", census)


if __name__ == "__main__":
    if not TORCH_AVAILABLE:
        raise SystemExit("torch is not installed")
    print(normalized_llama2_fx_graph(), end="")
