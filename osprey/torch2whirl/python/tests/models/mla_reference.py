"""Independent scalar FP64 expanded-key/value oracle for absorbed MLA.

No torch or numpy imports. Deliberately reconstruct K and V per token rather
than absorb Uk into Q. This checks the algebra and weight orientation of the
captured implementation independently. True mask entries permit attention.
"""

import math


def expanded_reference(query_nope, query_rope, cache, rope_cache,
                       key_up, value_up, allowed, alpha):
    """Return nested-list outputs [B,H,Dv] and LSE [B,H] for finite valid inputs.

    Inputs are validated by the tensor fixture boundary. Reduction uses fsum;
    all-masked heads return zero and -inf. This is an oracle, not a kernel.
    """
    outputs, lses = [], []
    for batch, queries in enumerate(query_nope):
        batch_output, batch_lse = [], []
        for head, query in enumerate(queries):
            visible = [token for token, valid in enumerate(allowed[batch][head]) if valid]
            if not visible:
                batch_output.append([0.0] * len(value_up[head]))
                batch_lse.append(-math.inf)
                continue
            scores, values = [], []
            for token in visible:
                latent = cache[batch][token]
                key = [math.fsum(weight * value for weight, value in zip(row, latent))
                       for row in key_up[head]]
                value = [math.fsum(weight * item for weight, item in zip(row, latent))
                         for row in value_up[head]]
                score = alpha * (
                    math.fsum(left * right for left, right in zip(query, key))
                    + math.fsum(left * right for left, right in
                                zip(query_rope[batch][head], rope_cache[batch][token]))
                )
                if not math.isfinite(score):
                    raise ValueError("nonfinite reference score")
                scores.append(score)
                values.append(value)
            maximum = max(scores)
            weights = [math.exp(score - maximum) for score in scores]
            denominator = math.fsum(weights)
            batch_output.append([
                math.fsum(weight * value[component] for weight, value in zip(weights, values))
                / denominator for component in range(len(value_up[head]))
            ])
            batch_lse.append(maximum + math.log(denominator))
        outputs.append(batch_output)
        lses.append(batch_lse)
    return outputs, lses
