"""Deterministic multi-tile Q5 fixtures using the original Q0 source algorithms."""
import math
import torch
from models.mla_quantized_paged import DenseQDQ, PagedRead, quantize
CASES = ('dense_s8_decode_long', 'paged_u4_prefill_long')


def make_fixture(name):
    """Use genuine 8193-token tensors, partial pages and an immutable prefix."""
    if name not in CASES:
        raise ValueError(name)
    b, h, q, l, p = 2, 3, 3 if name.startswith('paged') else 1, 8193, 64
    kind = 'u4-token' if 'u4' in name else 's8-token'
    def values(shape, seed):
        return torch.randn(shape, generator=torch.Generator().manual_seed(seed))*0.25
    query, qr = values((b, h, q, 8), 7101), values((b, h, q, 4), 7102)
    uk, uv = values((h, 8, 16), 7103), values((h, 6, 16), 7104)
    allowed = torch.ones((b, h, q, l), dtype=torch.bool)
    for j in range(q):
        allowed[:, :, j, l-q+j+1:] = False
    allowed[1, 2, :, :] = False
    meta = dict(case=name, format=kind, storage='paged' if name.startswith('paged') else 'dense',
                batch=b, heads=h, queries=q, length=l, latent=16, rotary=4, page_tokens=p if name.startswith('paged') else 0,
                append=0, phase='prefill' if q > 1 else 'decode', alpha=1/math.sqrt(12), fake=False)
    if name.startswith('dense'):
        inputs = (query, qr, values((b, l, 16), 7105), values((b, l, 4), 7106), uk, uv, allowed)
        return DenseQDQ(kind).eval(), inputs, meta
    pages = (l+p-1)//p
    count = b*pages+3
    table = torch.tensor([(i*5+2) % count for i in range(b*pages)], dtype=torch.int64).reshape(b, pages)
    table[1, 0] = table[0, 0]
    cache, cs = quantize(values((count, p, 16), 7105), kind)
    rotary, rs = quantize(values((count, p, 4), 7106), kind)
    slots = torch.tensor([[int(table[i, (l-q+j)//p])*p+(l-q+j) % p for j in range(q)] for i in range(b)], dtype=torch.int64)
    inputs = (query, qr, cache, cs, rotary, rs, table, uk, uv, allowed, values((b, q, 16), 7107), values((b, q, 4), 7108), slots, torch.arange(l, dtype=torch.int64))
    meta.update(physical_pages=count, logical_pages=pages, shared_prefix=True)
    return PagedRead(kind, b, l, p, 0).eval(), inputs, meta
