"""Scalar Python/FP64 oracles for building_blocks.py; no torch/numpy required.

These validate source semantics within tolerance, not native FP32 regrouping
or a GPU numerical contract. Inputs/outputs are nested Python lists.
"""

import math


def _matmul(lhs, rhs):
    """Evaluate a two-dimensional contraction using scalar accurate sums."""
    return [[math.fsum(a * b for a, b in zip(row, col))
             for col in zip(*rhs)] for row in lhs]


def _softmax(row):
    """Normalize finite logits stably; attention masks use negative infinity."""
    maximum = max(row)
    values = [math.exp(value - maximum) for value in row]
    total = math.fsum(values)
    return [value / total for value in values]


def reference(name, inputs):
    """Evaluate one fixture independently of its tensor implementation."""
    if name == "gemm":
        return _matmul(*inputs)
    if name == "reduction":
        return [math.fsum(row) for row in inputs[0]]
    if name == "softmax":
        return [_softmax(row) for row in inputs[0]]
    if name == "layernorm":
        value, gamma, beta = inputs
        result = []
        for row in value:
            mean = math.fsum(row) / len(row)
            variance = math.fsum((x - mean) ** 2 for x in row) / len(row)
            result.append([(x - mean) / math.sqrt(variance + 1.0e-5) * g + b
                           for x, g, b in zip(row, gamma, beta)])
        return result
    if name == "attention":
        query, key, value, allowed = inputs
        batches = []
        for qb, kb, vb in zip(query, key, value):
            heads = []
            for qh, kh, vh in zip(qb, kb, vb):
                scores = _matmul(qh, list(zip(*kh)))
                probabilities = [_softmax([
                    score / math.sqrt(3.0) if ok else -math.inf
                    for score, ok in zip(row, mask)])
                    for row, mask in zip(scores, allowed)]
                heads.append(_matmul(probabilities, vh))
            batches.append(heads)
        return batches
    if name == "moe":
        tokens, ids, weights, up, down = inputs
        result = []
        for token, routes, gates in zip(tokens, ids, weights):
            contributions = []
            for expert, gate in zip(routes, gates):
                hidden = [max(0, x) for x in _matmul([token], up[expert])[0]]
                contributions.append([gate * x for x in _matmul([hidden], down[expert])[0]])
            result.append([math.fsum(col) for col in zip(*contributions)])
        return result
    raise ValueError("unknown building block: " + name)
