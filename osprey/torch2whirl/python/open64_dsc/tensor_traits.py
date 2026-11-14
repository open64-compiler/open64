"""Declared tensor content roles for the pure tensor-expression ingestion lane.

Models may provide __open64_tensor_traits__ mapping source argument names to
these versioned strings. Q1's native source query can infer roles from copied
QDQ/use dataflow; no role is inferred from names, shapes, dtype or sample data alone.
Roles do not promise cache residency or immutability between invocations.
"""

LATENT_KV_CACHE = "attention.latent_kv_cache.v1"
ROTARY_KEY_CACHE = "attention.rotary_key_cache.v1"
CACHE_TRAITS = frozenset((LATENT_KV_CACHE, ROTARY_KEY_CACHE))


def declared_cache_traits(model, nodes, types, inferred=None):
    """Validate explicit input declarations before native tensor products exist."""
    declarations = getattr(model, "__open64_tensor_traits__", {})
    if type(declarations) is not dict or len(declarations) > len(nodes):
        raise ValueError("__open64_tensor_traits__ must map input names to cache traits")
    inputs = {str(node.target): node.name for node in nodes}
    result = {}
    inferred = inferred or {}
    for argument, trait in declarations.items():
        if type(argument) is not str or argument not in inputs:
            raise ValueError("cache trait names an unknown source argument")
        if type(trait) is not str or trait not in CACHE_TRAITS:
            raise ValueError("unsupported tensor cache trait")
        name = inputs[argument]
        dtype, shape = types[name]
        proof = inferred.get(name)
        if proof is not None and proof["role"] != trait:
            raise ValueError("cache declaration contradicts inferred dataflow role")
        if len(shape) != 3 or (dtype != "float32" and
                not (dtype in ("int8", "uint8") and proof is not None and proof["encoding"])):
            raise ValueError("cache trait requires rank-three FP32 or natively proved encoded cache storage")
        result[name] = trait
    return result
