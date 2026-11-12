"""Q0 real CPU tensor programs; no native WHIRL or optimization provider.

These are standalone absorbed-MLA re-expressions, not production engine kernels.
All quantization, bit packing, page lookup and functional updates stay visible
to FX. No source cache-role annotation selects or hides operations.
"""
import math
import torch

FORMATS = ("s8-token", "u4-token")
CASES = ("dense_s8_decode", "dense_s8_prefill_tail", "dense_s8_prefill",
         "dense_u4_decode", "paged_s8_decode", "paged_u4_prefill",
         "append_s8_decode", "append_s8_prefill", "append_u4_verify", "fake_s8_decode")


def quantize(value, kind):
    """Per-token F32 scale, RNE, saturation; U4 stores even column in low nibble."""
    maximum = torch.amax(torch.abs(value), dim=-1, keepdim=True)
    scale = torch.clamp(maximum, min=2.0 ** -12) / (127.0 if kind == "s8-token" else 7.0)
    rounded = torch.round(value / scale)
    if kind == "s8-token":
        return torch.clamp(rounded, min=-127, max=127).to(torch.int8), scale
    codes = torch.clamp(rounded + 8.0, min=0, max=15).to(torch.uint8)
    packed = torch.bitwise_or(codes[..., 0::2], torch.bitwise_left_shift(codes[..., 1::2], 4))
    return packed, scale


def dequantize(value, scale, kind):
    """Integer storage survives until explicit unpack/conversion in this graph."""
    if kind == "s8-token":
        return value.to(torch.float32) * scale
    low = torch.bitwise_and(value, 15)
    high = torch.bitwise_right_shift(value, 4)
    codes = torch.stack((low, high), dim=-1).flatten(-2)
    return (codes.to(torch.float32) - 8.0) * scale


def attention(q, qr, cache, rotary, uk, uv, allowed, alpha):
    """Q is explicit; each query/head has independent masked normalization."""
    query = torch.matmul(q, uk)
    scores = (torch.matmul(query, cache.unsqueeze(1).transpose(-2, -1))
              + torch.matmul(qr, rotary.unsqueeze(1).transpose(-2, -1))) * alpha
    scores = scores.masked_fill(~allowed, float("-inf"))
    maximum = torch.amax(scores, dim=-1, keepdim=True)
    maximum = torch.where(torch.isfinite(maximum), maximum, torch.zeros_like(maximum))
    weights = torch.exp(scores - maximum)
    denominator = torch.sum(weights, dim=-1, keepdim=True)
    safe = torch.where(denominator > 0, denominator, torch.ones_like(denominator))
    latent = torch.matmul(weights / safe, cache.unsqueeze(1))
    output = torch.matmul(latent, uv.transpose(-2, -1))
    lse = torch.where(denominator > 0, maximum + torch.log(safe),
                      torch.full_like(maximum, float("-inf"))).squeeze(-1)
    return output, lse


class DenseQDQ(torch.nn.Module):
    """Both producer quantization and packed cache outputs are real tensor ops."""
    def __init__(self, kind="s8-token", fake=False):
        super().__init__()
        self.kind, self.fake = kind, fake
        self.alpha = 1 / math.sqrt(12)

    def forward(self, q, qr, cache, rotary, uk, uv, allowed):
        if self.fake:
            cs = torch.clamp(torch.amax(torch.abs(cache), -1, keepdim=True), min=2.0 ** -12) / 127.0
            rs = torch.clamp(torch.amax(torch.abs(rotary), -1, keepdim=True), min=2.0 ** -12) / 127.0
            c = torch.clamp(torch.round(cache / cs), -127, 127) * cs
            r = torch.clamp(torch.round(rotary / rs), -127, 127) * rs
            output, lse = attention(q, qr, c, r, uk, uv, allowed, self.alpha)
            return output, lse, c, cs, r, rs
        cq, cs = quantize(cache, self.kind)
        rq, rs = quantize(rotary, self.kind)
        c, r = dequantize(cq, cs, self.kind), dequantize(rq, rs, self.kind)
        output, lse = attention(q, qr, c, r, uk, uv, allowed, self.alpha)
        return output, lse, cq, cs, rq, rs


class PagedRead(torch.nn.Module):
    """Page table translation and auxiliary gathers remain in the source graph."""
    def __init__(self, kind, batch, length, page_tokens=4, append=0):
        super().__init__()
        self.kind, self.batch, self.length = kind, batch, length
        self.page_tokens, self.append = page_tokens, append
        self.columns = 16 if kind == "s8-token" else 8
        self.rotary_columns = 4 if kind == "s8-token" else 2
        self.alpha = 1 / math.sqrt(12)

    def forward(self, q, qr, cache, cs, rotary, rs, table, uk, uv, allowed,
                new_cache, new_rotary, write_slots, tokens):
        if self.append:
            cq, cscale = quantize(new_cache, self.kind)
            rq, rscale = quantize(new_rotary, self.kind)
            slots = write_slots.reshape(-1)
            cache = torch.index_copy(cache.reshape(-1, self.columns), 0, slots,
                                     cq.reshape(-1, self.columns)).reshape(-1, self.page_tokens, self.columns)
            cs = torch.index_copy(cs.reshape(-1, 1), 0, slots,
                                  cscale.reshape(-1, 1)).reshape(-1, self.page_tokens, 1)
            rotary = torch.index_copy(rotary.reshape(-1, self.rotary_columns), 0, slots,
                                      rq.reshape(-1, self.rotary_columns)).reshape(-1, self.page_tokens, self.rotary_columns)
            rs = torch.index_copy(rs.reshape(-1, 1), 0, slots,
                                  rscale.reshape(-1, 1)).reshape(-1, self.page_tokens, 1)
        logical = torch.div(tokens, self.page_tokens, rounding_mode="floor")
        offsets = torch.remainder(tokens, self.page_tokens)
        physical = torch.index_select(table, 1, logical)
        slots = (physical * self.page_tokens + offsets.unsqueeze(0)).reshape(-1)
        cq = torch.index_select(cache.reshape(-1, self.columns), 0, slots).reshape(self.batch, self.length, self.columns)
        cscale = torch.index_select(cs.reshape(-1, 1), 0, slots).reshape(self.batch, self.length, 1)
        rq = torch.index_select(rotary.reshape(-1, self.rotary_columns), 0, slots).reshape(self.batch, self.length, self.rotary_columns)
        rscale = torch.index_select(rs.reshape(-1, 1), 0, slots).reshape(self.batch, self.length, 1)
        c, r = dequantize(cq, cscale, self.kind), dequantize(rq, rscale, self.kind)
        output, lse = attention(q, qr, c, r, uk, uv, allowed, self.alpha)
        return output, lse, cache, cs, rotary, rs


def make_fixture(name):
    """Fresh deterministic storage and explicit serving-intent metadata."""
    if name not in CASES:
        raise ValueError("unknown Q0 fixture")
    kind = "u4-token" if "u4" in name else "s8-token"
    paged, append = name.startswith(("paged", "append")), name.startswith("append")
    batch, heads, queries, length = 2, 3, (3 if ("prefill" in name and "tail" not in name) or "verify" in name else 1), (11 if paged else 17)
    page_tokens = 4
    def values(shape, seed):
        return torch.randn(shape, generator=torch.Generator().manual_seed(seed)) * 0.25
    q, qr = values((batch, heads, queries, 8), 7101), values((batch, heads, queries, 4), 7102)
    uk, uv = values((heads, 8, 16), 7103), values((heads, 6, 16), 7104)
    allowed = torch.ones(batch, heads, queries, length, dtype=torch.bool)
    for query in range(queries):
        allowed[:, :, query, length - queries + query + 1:] = False
    allowed[1, 2, :, :] = False
    phase = "speculative-verify" if "verify" in name else "prefill" if "prefill" in name else "decode"
    metadata = dict(case=name,format=kind,storage="paged" if paged else "dense",
                    batch=batch,heads=heads,queries=queries,length=length,latent=16,rotary=4,
                    page_tokens=page_tokens if paged else 0,append=queries if append else 0,
                    phase=phase,phase_evidence="fixture-intent-not-inferred-from-graph",
                    alpha=1/math.sqrt(12),fake=name.startswith("fake"))
    if not paged:
        cache, rotary = values((batch, length, 16), 7105), values((batch, length, 4), 7106)
        return DenseQDQ(kind, metadata["fake"]).eval(), (q, qr, cache, rotary, uk, uv, allowed), metadata
    pages = (length + page_tokens - 1) // page_tokens
    count = batch * pages + 3
    permutation = [(i * 5 + 2) % count for i in range(batch * pages)]
    table = torch.tensor(permutation, dtype=torch.int64).reshape(batch, pages)
    table[1, 0] = table[0, 0]  # Real shared immutable prefix; tail pages remain private.
    cache, cs = quantize(values((count, page_tokens, 16), 7105), kind)
    rotary, rs = quantize(values((count, page_tokens, 4), 7106), kind)
    new_cache = values((batch, queries, 16), 7107)
    new_rotary = values((batch, queries, 4), 7108)
    writes = [[int(table[b, (length-queries+i)//page_tokens])*page_tokens + (length-queries+i)%page_tokens
               for i in range(queries)] for b in range(batch)]
    write_slots = torch.tensor(writes, dtype=torch.int64)
    metadata.update(physical_pages=count,logical_pages=pages,shared_prefix=True)
    inputs = (q, qr, cache, cs, rotary, rs, table, uk, uv, allowed,
              new_cache, new_rotary, write_slots, torch.arange(length,dtype=torch.int64))
    return PagedRead(kind,batch,length,page_tokens,queries if append else 0).eval(), inputs, metadata
