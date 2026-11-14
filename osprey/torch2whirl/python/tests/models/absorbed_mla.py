"""Single-query absorbed MLA source fixture for MLA-INGESTION-PLAN.md.

The equations follow DeepSeek-V3 inference/model.py's absorbed branch, expressed
as explicit contractions for capture. This is an algorithm extraction, not a
copy of the full upstream layer. RoPE and KV normalization are already applied.
"""

import math

import torch
from open64_dsc.tensor_traits import LATENT_KV_CACHE, ROTARY_KEY_CACHE


INPUT_NAMES = ("query_nope", "query_rope", "cache", "rope_cache",
               "key_up", "value_up", "allowed")
CONTEXT_LENGTHS = {"short": 127, "long": 8193}


class AbsorbedMLA(torch.nn.Module):
    """Read-only decode core returning per-head output and natural-log LSE.

    q [B,H,Dn], qr [B,H,Dr], c [B,L,R], kr [B,L,Dr],
    Uk [H,Dn,R], Uv [H,Dv,R], allowed [B,H,L].
    The scale is the original model scale, not 1/sqrt(R+Dr).
    No final cross-head output projection, cache append, JIT or kernel dispatch.
    """

    __open64_tensor_traits__ = {"cache": LATENT_KV_CACHE,
                               "rope_cache": ROTARY_KEY_CACHE}

    def __init__(self, alpha):
        super().__init__()
        if not math.isfinite(alpha):
            raise ValueError("alpha must be finite")
        self.alpha = float(alpha)

    def forward(self, query_nope, query_rope, cache, rope_cache,
                key_up, value_up, allowed):
        # Move the key up-projection to the query; keep the latent cache shared.
        query = torch.matmul(query_nope.unsqueeze(-2), key_up).squeeze(-2)
        latent_scores = torch.matmul(
            query.unsqueeze(-2), cache.unsqueeze(1).transpose(-2, -1)
        ).squeeze(-2)
        rope_scores = torch.matmul(
            query_rope.unsqueeze(-2), rope_cache.unsqueeze(1).transpose(-2, -1)
        ).squeeze(-2)
        scores = ((latent_scores + rope_scores) * self.alpha).masked_fill(
            ~allowed, float("-inf")
        )

        # All-masked rows use a neutral maximum and denominator: output 0, LSE -inf.
        maximum = torch.amax(scores, dim=-1, keepdim=True)
        maximum = torch.where(torch.isfinite(maximum), maximum,
                              torch.zeros_like(maximum))
        weights = torch.exp(scores - maximum)
        denominator = torch.sum(weights, dim=-1, keepdim=True)
        safe_denominator = torch.where(denominator > 0, denominator,
                                       torch.ones_like(denominator))
        probabilities = weights / safe_denominator
        latent = torch.matmul(
            probabilities.unsqueeze(-2), cache.unsqueeze(1)
        ).squeeze(-2)
        output = torch.matmul(
            latent.unsqueeze(-2), value_up.transpose(-2, -1)
        ).squeeze(-2)
        lse = torch.where(
            denominator > 0, maximum + torch.log(safe_denominator),
            torch.full_like(maximum, float("-inf"))
        ).squeeze(-1)
        return output, lse


def validate_inputs(inputs):
    """Preflight the bounded CPU/FP32 fixture without adding guards to its graph."""
    if len(inputs) != len(INPUT_NAMES):
        raise ValueError("expected seven inputs")
    q, qr, c, kr, uk, uv, allowed = inputs
    ranks = (3, 3, 3, 3, 3, 3, 3)
    for name, tensor, rank in zip(INPUT_NAMES, inputs, ranks):
        if not isinstance(tensor, torch.Tensor) or tensor.ndim != rank:
            raise ValueError(name + " must be a rank-3 tensor")
        if tensor.device.type != "cpu" or not tensor.is_contiguous():
            raise ValueError(name + " must be contiguous CPU storage")
        expected_dtype = torch.bool if name == "allowed" else torch.float32
        if tensor.dtype != expected_dtype:
            raise ValueError(name + " has unsupported dtype")
        if any(size <= 0 for size in tensor.shape):
            raise ValueError(name + " must have positive extents")
        if name != "allowed" and not torch.isfinite(tensor).all().item():
            raise ValueError(name + " must contain finite values")
    batch, heads, nope = q.shape
    rank = c.shape[-1]
    length = c.shape[1]
    rope = qr.shape[-1]
    expected = ((batch, heads, rope), (batch, length, rank),
                (batch, length, rope), (heads, nope, rank),
                (heads, uv.shape[1], rank), (batch, heads, length))
    for name, tensor, shape in zip(INPUT_NAMES[1:], inputs[1:], expected):
        if tuple(tensor.shape) != shape:
            raise ValueError(name + " shape mismatch")


def make_fixture(context="short", length=None):
    """Return a fresh model and deterministic, unaliased inputs; no global RNG use.

    Small unequal dimensions expose transpose/scale errors while allowing scalar
    checking at a realistic long sequence length. Neither case is a GPU tuning
    workload or the full DeepSeek model configuration.
    """
    if context not in CONTEXT_LENGTHS:
        raise ValueError("context must be short or long")
    length = CONTEXT_LENGTHS[context] if length is None else length
    if isinstance(length, bool) or not isinstance(length, int) or length < 1:
        raise ValueError("length must be a positive integer")
    batch, heads, nope, rope, rank, value = 1, 3, 8, 4, 16, 6

    def values(shape, seed):
        generator = torch.Generator(device="cpu").manual_seed(seed)
        return torch.randn(shape, generator=generator, dtype=torch.float32) * 0.25

    # Per-input seeds preserve the same query/weights across both context cases.
    inputs = (
        values((batch, heads, nope), 4107),
        values((batch, heads, rope), 4108),
        values((batch, length, rank), 4109),
        values((batch, length, rope), 4110),
        values((heads, nope, rank), 4111),
        values((heads, value, rank), 4112),
        torch.ones((batch, heads, length), dtype=torch.bool),
    )
    validate_inputs(inputs)
    return AbsorbedMLA(1 / math.sqrt(nope + rope)).eval(), inputs
