"""Deterministic inputs for BUILDING-BLOCK-INGESTION-PLAN.md.

These CPU/FP32 source programs are capture fixtures, not target kernels or
claims of native WHIRL ingestion. Each factory returns fresh, unaliased inputs.
"""

import math

import torch


class Gemm(torch.nn.Module):
    """Rectangular, non-tile-divisible matrix multiplication."""

    def forward(self, lhs, rhs):
        return torch.matmul(lhs, rhs)


class RowReduction(torch.nn.Module):
    """Sum the last dimension without retaining that dimension."""

    def forward(self, value):
        return torch.sum(value, dim=-1)


class StableSoftmax(torch.nn.Module):
    """Explicit stable row Softmax, with a visible normalization boundary."""

    def forward(self, value):
        shifted = value - torch.amax(value, dim=-1, keepdim=True)
        numerator = torch.exp(shifted)
        return numerator / torch.sum(numerator, dim=-1, keepdim=True)


class AffineLayerNorm(torch.nn.Module):
    """Population variance, epsilon inside sqrt, and nontrivial affine terms."""

    def forward(self, value, gamma, beta):
        mean = torch.mean(value, dim=-1, keepdim=True)
        centered = value - mean
        variance = torch.mean(centered * centered, dim=-1, keepdim=True)
        return centered * torch.rsqrt(variance + 1.0e-5) * gamma + beta


class CausalAttention(torch.nn.Module):
    """One non-mutating attention call; True mask entries permit attention."""

    def forward(self, query, key, value, allowed):
        scores = torch.matmul(query, key.transpose(-2, -1)) / math.sqrt(3.0)
        scores = scores.masked_fill(~allowed, float("-inf"))
        return torch.matmul(torch.softmax(scores, dim=-1), value)


class RoutedMoE(torch.nn.Module):
    """Fixed-shape top-2 expert execution with runtime IDs and route weights.

    Routing policy is external: no capacity limit, token dropping, or gate
    renormalization. Repeated IDs mean repeated weighted contributions.
    """

    def forward(self, tokens, route_ids, route_weights, up, down):
        hidden = torch.matmul(tokens[:, None, None, :], up[route_ids])
        hidden = torch.relu(hidden)
        expert_outputs = torch.matmul(hidden, down[route_ids]).squeeze(-2)
        return torch.sum(expert_outputs * route_weights[:, :, None], dim=1)


NAMES = ("gemm", "reduction", "softmax", "layernorm", "attention", "moe")


def _values(shape, offset=0):
    """Generate bounded, exactly reproducible values without global RNG state."""
    count = math.prod(shape)
    return (((torch.arange(count) + offset) % 19 - 9).float() / 8).reshape(shape)


def make_fixture(name):
    """Return a fresh eval module and its deterministic positional inputs."""
    if name == "gemm":
        return Gemm().eval(), (_values((5, 7)), _values((7, 3), 4))
    if name == "reduction":
        return RowReduction().eval(), (_values((3, 17)),)
    if name == "softmax":
        return StableSoftmax().eval(), (_values((3, 17)) * 20 + 1000,)
    if name == "layernorm":
        return AffineLayerNorm().eval(), (
            _values((3, 17)), _values((17,), 2) + 1, _values((17,), 7),
        )
    if name == "attention":
        return CausalAttention().eval(), (
            _values((1, 2, 4, 3)), _values((1, 2, 4, 3), 3),
            _values((1, 2, 4, 2), 5),
            torch.ones(4, 4, dtype=torch.bool).tril(),
        )
    if name == "moe":
        return RoutedMoE().eval(), (
            _values((5, 3)),
            torch.tensor([[0, 1], [0, 1], [1, 0], [0, 1], [0, 0]]),
            torch.tensor([[.75, .25], [.5, .5], [.25, .75], [.5, .5], [.2, .8]]),
            _values((3, 3, 4), 1), _values((3, 4, 3), 6),
        )
    raise ValueError("unknown building block: " + name)
