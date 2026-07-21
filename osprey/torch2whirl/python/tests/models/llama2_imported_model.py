"""Tiny Llama 2 fixture whose layer definitions are visible through imports."""

from __future__ import annotations

import torch

from .llama2_model import TinyLlama2Config
from .llama2_model import TinyLlama2DecoderLayer
from .llama2_model import TinyRMSNorm
from .llama2_model import initialize_tiny_llama2
from .llama2_model import sample_input_ids


class TinyImportedLlama2ForCausalLM(torch.nn.Module):
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
        self.output = torch.nn.Linear(
            config.hidden_size,
            config.vocab_size,
            bias=False,
        )

    def forward(self, input_ids: torch.Tensor) -> torch.Tensor:
        value = self.token_embedding(input_ids)
        for layer in self.layers:
            value = layer(value)
        value = self.norm(value)
        return self.output(value)


def create_tiny_imported_llama2(
    config: TinyLlama2Config | None = None,
) -> TinyImportedLlama2ForCausalLM:
    config = config or TinyLlama2Config()
    model = TinyImportedLlama2ForCausalLM(config)
    initialize_tiny_llama2(model)
    model.eval()
    return model


def open64_sample_inputs() -> tuple[torch.Tensor]:
    return (sample_input_ids(),)


def create_model() -> TinyImportedLlama2ForCausalLM:
    return create_tiny_imported_llama2()
