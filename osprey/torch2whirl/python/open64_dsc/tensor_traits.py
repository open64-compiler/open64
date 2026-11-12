"""Declared tensor content roles for the pure tensor-expression ingestion lane.

Models may provide __open64_tensor_traits__ mapping source argument names to
these versioned strings. No cache role is inferred from names, shapes or data.
Roles do not promise cache residency or immutability between invocations.
"""

LATENT_KV_CACHE = "attention.latent_kv_cache.v1"
ROTARY_KEY_CACHE = "attention.rotary_key_cache.v1"
CACHE_TRAITS = frozenset((LATENT_KV_CACHE, ROTARY_KEY_CACHE))


def declared_cache_traits(model, nodes, types):
    """Validate explicit input declarations before native tensor products exist."""
    declarations = getattr(model, "__open64_tensor_traits__", {})
    if type(declarations) is not dict or len(declarations) > len(nodes):
        raise ValueError("__open64_tensor_traits__ must map input names to cache traits")
    inputs = {str(node.target): node.name for node in nodes}
    result = {}
    for argument, trait in declarations.items():
        if type(argument) is not str or argument not in inputs:
            raise ValueError("cache trait names an unknown source argument")
        if type(trait) is not str or trait not in CACHE_TRAITS:
            raise ValueError("unsupported tensor cache trait")
        name = inputs[argument]
        dtype, shape = types[name]
        if dtype != "float32" or len(shape) != 3:
            raise ValueError("cache trait requires a rank-three FP32 [batch,token,column] input")
        result[name] = trait
    return result
