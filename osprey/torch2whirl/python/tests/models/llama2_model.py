"""Dependency-light tiny Llama 2 prefill fixture for discovery tests.

The fixture intentionally preserves Llama-style semantic landmarks while
avoiding FairScale, CUDA-only state, tokenizers, licensed weights, and mutable
KV cache behavior. It is a graph-discovery source model, not a native WHIRL
lowering implementation.
"""

from __future__ import annotations

from dataclasses import dataclass
import math

import torch


@dataclass(frozen=True)
class TinyLlama2Config:
    vocab_size: int = 128
    hidden_size: int = 32
    intermediate_size: int = 88
    num_layers: int = 2
    num_attention_heads: int = 4
    num_kv_heads: int = 4
    batch_size: int = 1
    sequence_length: int = 8
    rms_norm_eps: float = 1.0e-5
    rope_theta: float = 10000.0

    @property
    def head_dim(self) -> int:
        return self.hidden_size // self.num_attention_heads


class TinyRMSNorm(torch.nn.Module):
    def __init__(self, hidden_size: int, eps: float) -> None:
        super().__init__()
        self.eps = eps
        self.weight = torch.nn.Parameter(torch.ones(hidden_size))

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        variance = torch.mean(value * value, dim=-1, keepdim=True)
        normalized = value * torch.rsqrt(variance + self.eps)
        return normalized * self.weight


class TinyRotaryEmbedding(torch.nn.Module):
    def __init__(self, config: TinyLlama2Config) -> None:
        super().__init__()
        position = torch.arange(config.sequence_length, dtype=torch.float32)
        dims = torch.arange(0, config.head_dim, 2, dtype=torch.float32)
        inv_freq = 1.0 / (config.rope_theta ** (dims / config.head_dim))
        freqs = torch.outer(position, inv_freq)
        angles = torch.cat((freqs, freqs), dim=-1)
        self.register_buffer(
            "cos",
            torch.cos(angles).view(1, 1, config.sequence_length, config.head_dim),
        )
        self.register_buffer(
            "sin",
            torch.sin(angles).view(1, 1, config.sequence_length, config.head_dim),
        )

    def _rotate_half(self, value: torch.Tensor) -> torch.Tensor:
        half = value.shape[-1] // 2
        left = value[..., :half]
        right = value[..., half:]
        return torch.cat((-right, left), dim=-1)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        return (value * self.cos) + (self._rotate_half(value) * self.sin)


class TinyLlama2Attention(torch.nn.Module):
    def __init__(self, config: TinyLlama2Config) -> None:
        super().__init__()
        self.config = config
        self.wq = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.wk = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.wv = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.wo = torch.nn.Linear(config.hidden_size, config.hidden_size, bias=False)
        self.rotary = TinyRotaryEmbedding(config)
        causal_mask = torch.triu(
            torch.full(
                (config.sequence_length, config.sequence_length),
                float("-inf"),
            ),
            diagonal=1,
        )
        self.register_buffer(
            "causal_mask",
            causal_mask.view(1, 1, config.sequence_length, config.sequence_length),
        )

    def _shape_projection(self, value: torch.Tensor) -> torch.Tensor:
        return value.view(
            self.config.batch_size,
            self.config.sequence_length,
            self.config.num_attention_heads,
            self.config.head_dim,
        ).transpose(1, 2)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        query = self.rotary(self._shape_projection(self.wq(value)))
        key = self.rotary(self._shape_projection(self.wk(value)))
        val = self._shape_projection(self.wv(value))
        scores = torch.matmul(query, key.transpose(-2, -1))
        scores = scores * (1.0 / math.sqrt(self.config.head_dim))
        scores = scores + self.causal_mask
        probabilities = torch.softmax(scores, dim=-1)
        context = torch.matmul(probabilities, val)
        context = context.transpose(1, 2).contiguous().view(
            self.config.batch_size,
            self.config.sequence_length,
            self.config.hidden_size,
        )
        return self.wo(context)


class TinyLlama2FeedForward(torch.nn.Module):
    def __init__(self, config: TinyLlama2Config) -> None:
        super().__init__()
        self.gate_proj = torch.nn.Linear(
            config.hidden_size,
            config.intermediate_size,
            bias=False,
        )
        self.up_proj = torch.nn.Linear(
            config.hidden_size,
            config.intermediate_size,
            bias=False,
        )
        self.down_proj = torch.nn.Linear(
            config.intermediate_size,
            config.hidden_size,
            bias=False,
        )

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        return self.down_proj(torch.nn.functional.silu(self.gate_proj(value)) *
                              self.up_proj(value))


class TinyLlama2DecoderLayer(torch.nn.Module):
    def __init__(self, config: TinyLlama2Config) -> None:
        super().__init__()
        self.attention_norm = TinyRMSNorm(config.hidden_size, config.rms_norm_eps)
        self.attention = TinyLlama2Attention(config)
        self.ffn_norm = TinyRMSNorm(config.hidden_size, config.rms_norm_eps)
        self.feed_forward = TinyLlama2FeedForward(config)

    def forward(self, value: torch.Tensor) -> torch.Tensor:
        attention_out = self.attention(self.attention_norm(value))
        value = value + attention_out
        feed_forward_out = self.feed_forward(self.ffn_norm(value))
        return value + feed_forward_out


class TinyLlama2ForCausalLM(torch.nn.Module):
    def __init__(self, config: TinyLlama2Config) -> None:
        super().__init__()
        self.config = config
        self.token_embedding = torch.nn.Embedding(
            config.vocab_size,
            config.hidden_size,
        )
        self.layers = torch.nn.ModuleList(
            TinyLlama2DecoderLayer(config) for _ in range(config.num_layers)
        )
        self.norm = TinyRMSNorm(config.hidden_size, config.rms_norm_eps)
        self.output = torch.nn.Linear(config.hidden_size, config.vocab_size,
                                      bias=False)

    def forward(self, input_ids: torch.Tensor) -> torch.Tensor:
        value = self.token_embedding(input_ids)
        for layer in self.layers:
            value = layer(value)
        value = self.norm(value)
        return self.output(value)


def initialize_tiny_llama2(model: torch.nn.Module) -> None:
    """Fill parameters with deterministic, bounded values."""

    with torch.no_grad():
        for ordinal, parameter in enumerate(model.parameters()):
            values = torch.arange(parameter.numel(), dtype=torch.float32)
            values = values.reshape(parameter.shape)
            values = ((values % 29.0) - 14.0) / 256.0
            values = values + (ordinal * 0.0005)
            parameter.copy_(values.to(dtype=parameter.dtype))


def create_tiny_llama2(
    config: TinyLlama2Config | None = None,
) -> TinyLlama2ForCausalLM:
    config = config or TinyLlama2Config()
    model = TinyLlama2ForCausalLM(config)
    initialize_tiny_llama2(model)
    model.eval()
    return model


def sample_input_ids(config: TinyLlama2Config | None = None) -> torch.Tensor:
    config = config or TinyLlama2Config()
    values = torch.arange(config.sequence_length, dtype=torch.int64)
    values = values.remainder(config.vocab_size)
    return values.view(config.batch_size, config.sequence_length)


def create_model() -> TinyLlama2ForCausalLM:
    return create_tiny_llama2()
