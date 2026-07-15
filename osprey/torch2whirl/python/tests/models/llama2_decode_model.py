"""Dependency-light tiny Llama 2 decode fixture for discovery tests.

The fixture exposes KV cache state and cache position as ordinary source-model
inputs and returns updated caches as ordinary outputs. It models functional
append semantics for discovery; it does not define a WHIRL state/effect ABI or
perform frontend lowering.
"""

from __future__ import annotations

from dataclasses import dataclass
import math

import torch

from models.llama2_model import TinyLlama2FeedForward
from models.llama2_model import TinyRMSNorm
from models.llama2_model import initialize_tiny_llama2


@dataclass(frozen=True)
class TinyLlama2DecodeConfig:
    vocab_size: int = 128
    hidden_size: int = 32
    intermediate_size: int = 88
    num_layers: int = 2
    num_attention_heads: int = 4
    num_kv_heads: int = 4
    batch_size: int = 1
    decode_sequence_length: int = 1
    cache_length: int = 3
    max_sequence_length: int = 8
    rms_norm_eps: float = 1.0e-5
    rope_theta: float = 10000.0

    @property
    def head_dim(self) -> int:
        return self.hidden_size // self.num_attention_heads


OPEN64_SAMPLE_INPUT_PROTOCOL = (
    "input_ids",
    "cache_position",
    "layer0_key_cache",
    "layer0_value_cache",
    "layer1_key_cache",
    "layer1_value_cache",
)


class TinyDecodeRotaryEmbedding(torch.nn.Module):
    def __init__(self, config: TinyLlama2DecodeConfig) -> None:
        super().__init__()
        self.half_dim = config.head_dim // 2
        position = torch.arange(config.max_sequence_length, dtype=torch.float32)
        dims = torch.arange(0, config.head_dim, 2, dtype=torch.float32)
        inv_freq = 1.0 / (config.rope_theta ** (dims / config.head_dim))
        freqs = torch.outer(position, inv_freq)
        angles = torch.cat((freqs, freqs), dim=-1)
        self.register_buffer(
            "cos",
            torch.cos(angles).view(
                1,
                1,
                config.max_sequence_length,
                config.head_dim,
            ),
        )
        self.register_buffer(
            "sin",
            torch.sin(angles).view(
                1,
                1,
                config.max_sequence_length,
                config.head_dim,
            ),
        )

    def _rotate_half(self, value: torch.Tensor) -> torch.Tensor:
        left = value[..., :self.half_dim]
        right = value[..., self.half_dim:]
        return torch.cat((-right, left), dim=-1)

    def forward(
        self,
        value: torch.Tensor,
        cache_position: torch.Tensor,
    ) -> torch.Tensor:
        cos = torch.index_select(self.cos, 2, cache_position)
        sin = torch.index_select(self.sin, 2, cache_position)
        return (value * cos) + (self._rotate_half(value) * sin)


class TinyLlama2DecodeAttention(torch.nn.Module):
    def __init__(self, config: TinyLlama2DecodeConfig) -> None:
        super().__init__()
        self.config = config
        self.wq = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.wk = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.wv = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.wo = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.rotary = TinyDecodeRotaryEmbedding(config)

    def _shape_projection(self, value: torch.Tensor) -> torch.Tensor:
        return value.view(
            self.config.batch_size,
            self.config.decode_sequence_length,
            self.config.num_attention_heads,
            self.config.head_dim,
        ).transpose(1, 2)

    def forward(
        self,
        value: torch.Tensor,
        cache_position: torch.Tensor,
        key_cache: torch.Tensor,
        value_cache: torch.Tensor,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        query = self.rotary(
            self._shape_projection(self.wq(value)),
            cache_position,
        )
        current_key = self.rotary(
            self._shape_projection(self.wk(value)),
            cache_position,
        )
        current_value = self._shape_projection(self.wv(value))

        updated_key_cache = torch.cat((key_cache, current_key), dim=2)
        updated_value_cache = torch.cat((value_cache, current_value), dim=2)
        scores = torch.matmul(query, updated_key_cache.transpose(-2, -1))
        scores = scores * (1.0 / math.sqrt(self.config.head_dim))
        probabilities = torch.softmax(scores, dim=-1)
        context = torch.matmul(probabilities, updated_value_cache)
        context = context.transpose(1, 2).contiguous().view(
            self.config.batch_size,
            self.config.decode_sequence_length,
            self.config.hidden_size,
        )
        return self.wo(context), updated_key_cache, updated_value_cache


class TinyLlama2DecodeLayer(torch.nn.Module):
    def __init__(self, config: TinyLlama2DecodeConfig) -> None:
        super().__init__()
        self.attention_norm = TinyRMSNorm(config.hidden_size, config.rms_norm_eps)
        self.attention = TinyLlama2DecodeAttention(config)
        self.ffn_norm = TinyRMSNorm(config.hidden_size, config.rms_norm_eps)
        self.feed_forward = TinyLlama2FeedForward(config)

    def forward(
        self,
        value: torch.Tensor,
        cache_position: torch.Tensor,
        key_cache: torch.Tensor,
        value_cache: torch.Tensor,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        attention_out, updated_key_cache, updated_value_cache = self.attention(
            self.attention_norm(value),
            cache_position,
            key_cache,
            value_cache,
        )
        value = value + attention_out
        feed_forward_out = self.feed_forward(self.ffn_norm(value))
        return (
            value + feed_forward_out,
            updated_key_cache,
            updated_value_cache,
        )


class TinyLlama2DecodeForCausalLM(torch.nn.Module):
    def __init__(self, config: TinyLlama2DecodeConfig) -> None:
        super().__init__()
        self.config = config
        self.token_embedding = torch.nn.Embedding(
            config.vocab_size,
            config.hidden_size,
        )
        self.layers = torch.nn.ModuleList(
            TinyLlama2DecodeLayer(config) for _ in range(config.num_layers)
        )
        self.norm = TinyRMSNorm(config.hidden_size, config.rms_norm_eps)
        self.output = torch.nn.Linear(
            config.hidden_size,
            config.vocab_size,
            bias=False,
        )

    def forward(
        self,
        input_ids: torch.Tensor,
        cache_position: torch.Tensor,
        layer0_key_cache: torch.Tensor,
        layer0_value_cache: torch.Tensor,
        layer1_key_cache: torch.Tensor,
        layer1_value_cache: torch.Tensor,
    ) -> tuple[
        torch.Tensor,
        torch.Tensor,
        torch.Tensor,
        torch.Tensor,
        torch.Tensor,
    ]:
        value = self.token_embedding(input_ids)
        value, layer0_key_cache, layer0_value_cache = self.layers[0](
            value,
            cache_position,
            layer0_key_cache,
            layer0_value_cache,
        )
        value, layer1_key_cache, layer1_value_cache = self.layers[1](
            value,
            cache_position,
            layer1_key_cache,
            layer1_value_cache,
        )
        logits = self.output(self.norm(value))
        return (
            logits,
            layer0_key_cache,
            layer0_value_cache,
            layer1_key_cache,
            layer1_value_cache,
        )


def create_tiny_llama2_decode(
    config: TinyLlama2DecodeConfig | None = None,
) -> TinyLlama2DecodeForCausalLM:
    config = config or TinyLlama2DecodeConfig()
    model = TinyLlama2DecodeForCausalLM(config)
    initialize_tiny_llama2(model)
    model.eval()
    return model


def _sample_cache(
    config: TinyLlama2DecodeConfig,
    ordinal: int,
) -> torch.Tensor:
    shape = (
        config.batch_size,
        config.num_kv_heads,
        config.cache_length,
        config.head_dim,
    )
    values = torch.arange(math.prod(shape), dtype=torch.float32).reshape(shape)
    return ((values % 31.0) - 15.0) / 512.0 + ordinal * 0.001


def sample_decode_inputs(
    config: TinyLlama2DecodeConfig | None = None,
) -> tuple[
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
]:
    config = config or TinyLlama2DecodeConfig()
    input_ids = torch.tensor([[config.cache_length]], dtype=torch.int64)
    cache_position = torch.tensor([config.cache_length], dtype=torch.int64)
    return (
        input_ids,
        cache_position,
        _sample_cache(config, 0),
        _sample_cache(config, 1),
        _sample_cache(config, 2),
        _sample_cache(config, 3),
    )


def open64_sample_inputs() -> tuple[
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
    torch.Tensor,
]:
    return sample_decode_inputs()


def create_model() -> TinyLlama2DecodeForCausalLM:
    return create_tiny_llama2_decode()
