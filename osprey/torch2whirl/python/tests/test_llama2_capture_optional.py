from __future__ import annotations

import importlib.util
from pathlib import Path
import re
import tempfile
import unittest


TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None

if TORCH_AVAILABLE:
    import torch
    import torch.fx
    from torch.fx.passes.shape_prop import ShapeProp

    from models.llama2_model import TinyLlama2Config
    from models.llama2_model import create_tiny_llama2
    from models.llama2_model import open64_sample_inputs
    from models.llama2_model import sample_input_ids

    from open64_dsc.cli import _sample_input_from_spec
    from open64_dsc.export import export_to_whirl, save_as_whirl
    from open64_dsc.options import WhirlExportOptions


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


@unittest.skipUnless(TORCH_AVAILABLE, "torch is not installed")
class TinyLlama2WhirlExportOptionalTest(unittest.TestCase):
    def test_mock_export_emits_semantic_prefill_operators(self) -> None:
        config = TinyLlama2Config()
        model = create_tiny_llama2(config)
        module = export_to_whirl(
            model,
            [sample_input_ids(config)],
            WhirlExportOptions(
                backend="mock",
                model_name="llama2",
                external_data_file="llama2.safetensors",
            ),
        )

        self.assertEqual(module.graph_source, "torch.fx+llama2_semantic")
        self.assertEqual(module.input_count, 1)
        self.assertIn("transformer.token_embedding", module.operators)
        self.assertIn("transformer.rms_norm", module.operators)
        self.assertIn("transformer.rotary_embedding", module.operators)
        self.assertIn("transformer.attention", module.operators)
        self.assertIn("transformer.swiglu", module.operators)
        self.assertIn("common.reshape", module.operators)
        self.assertIn("common.transpose", module.operators)
        self.assertEqual(module.operators[-1], "common.output_logits")

        payload_keys = {payload.tensor_key for payload in module.tensor_payloads}
        self.assertIn("token_embedding.weight", payload_keys)
        self.assertIn("layers.0.attention.wq.weight", payload_keys)
        self.assertIn("layers.0.attention.rotary.cos", payload_keys)
        self.assertIn("layers.1.feed_forward.down_proj.weight", payload_keys)
        self.assertIn("output.weight", payload_keys)
        self.assertEqual(
            {payload.storage_file for payload in module.tensor_payloads},
            {"llama2.safetensors"},
        )

        attention = [
            operator for operator in module.graph_operators
            if operator.name == "transformer.attention"
        ][0]
        self.assertEqual(attention.attrs["attr.mask_mode"], "causal")
        self.assertEqual(attention.attrs["attr.query_heads"], "4")
        self.assertEqual(attention.metadata["semantic_name"], "attention")

    def test_mock_export_writes_deterministic_artifacts(self) -> None:
        config = TinyLlama2Config()
        model = create_tiny_llama2(config)
        with tempfile.TemporaryDirectory() as temp_dir:
            output_path = Path(temp_dir) / "llama2.B"
            module = export_to_whirl(
                model,
                [sample_input_ids(config)],
                WhirlExportOptions(
                    backend="mock",
                    model_name="llama2",
                    external_data_file="llama2.safetensors",
                ),
            )
            save_as_whirl(module, str(output_path))

            artifact = output_path.read_text(encoding="utf-8")
            self.assertIn("graph_source=torch.fx+llama2_semantic", artifact)
            self.assertIn("operator.0=transformer.token_embedding", artifact)
            self.assertIn("tensor_payload.0=llama2.safetensors", artifact)
            self.assertTrue((Path(temp_dir) / "llama2.safetensors").is_file())

    def test_int_shape_sample_input_and_source_provider(self) -> None:
        parsed = _sample_input_from_spec("int-shape:1,8")
        self.assertEqual(parsed.dtype, torch.int64)
        self.assertEqual(tuple(parsed.shape), (1, 8))

        provided = open64_sample_inputs()
        self.assertEqual(len(provided), 1)
        self.assertEqual(provided[0].dtype, torch.int64)
        self.assertEqual(tuple(provided[0].shape), (1, 8))

    def test_rejects_float_token_input(self) -> None:
        config = TinyLlama2Config()
        model = create_tiny_llama2(config)
        with self.assertRaisesRegex(NotImplementedError, "input_ids must be int64"):
            export_to_whirl(
                model,
                [torch.ones((1, 8), dtype=torch.float32)],
                WhirlExportOptions(backend="mock"),
            )

    def test_rejects_grouped_query_profile_until_contract_expands(self) -> None:
        config = TinyLlama2Config(num_kv_heads=2)
        model = create_tiny_llama2(config)
        with self.assertRaisesRegex(NotImplementedError, "grouped-query"):
            export_to_whirl(
                model,
                [sample_input_ids(config)],
                WhirlExportOptions(backend="mock"),
            )


if __name__ == "__main__":
    if not TORCH_AVAILABLE:
        raise SystemExit("torch is not installed")
    print(normalized_llama2_fx_graph(), end="")
