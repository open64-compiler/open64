# Synthetic source fixture for the native Llama 2 decode-state IR test.

def decode_layer(builder):
    """Describe the first reviewed single-token decode profile."""

    # The line numbers below are part of the source-position fixture.

    # Inputs use the observed FX decode protocol.

    query = builder.model_input("query")
    key_cache = builder.model_input("key_cache")
    value_cache = builder.model_input("value_cache")
    rope_cos = builder.model_input("rope_cos")
    rope_sin = builder.model_input("rope_sin")
    cache_position = builder.model_input("cache_position")

    # The REGION owns the two uniquely owned mutable cache states.


    decoder = builder.region("transformer.decoder_layer.v2")
    positioned_query = builder.rotary_embedding_v2(query, rope_cos, rope_sin, cache_position)
    cached_attention = builder.attention_v2(positioned_query, key_cache, value_cache)
    builder.modify_state(decoder, "key_cache", cached_attention)
    builder.modify_state(decoder, "value_cache", cached_attention)
    return cached_attention
