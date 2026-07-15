from __future__ import annotations

import importlib.util
import unittest


TORCH_AVAILABLE = importlib.util.find_spec("torch") is not None

if TORCH_AVAILABLE:
    import torch

    from models.llama2_model import TinyLlama2Config
    from models.llama2_model import create_tiny_llama2
    from models.llama2_model import sample_input_ids


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
