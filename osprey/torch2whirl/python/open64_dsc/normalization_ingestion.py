"""Connect exact centered affine Python normalization to native WHIRL.

Design: doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md. This is
source ingestion, not numerical authorization, target planning or lowering.
"""
import struct

from .mapping import common
from .row_characterization import (CharacterizationError, characterize_row_graph,
                                   match_centered_affine_normalization_structure)
from .row_ingestion import export_native_row


def epsilon_f32_bits(epsilon):
    """Encode the source FP32 scalar; reject overflow/underflow, never repair."""
    try:
        bits, = struct.unpack(">I", struct.pack(">f", epsilon))
    except (OverflowError, TypeError, struct.error) as exc:
        raise CharacterizationError("epsilon must convert to positive finite FP32") from exc
    if type(epsilon) not in (float, int) or not 0 < bits < 0x7f800000:
        raise CharacterizationError("epsilon must convert to positive finite FP32")
    return "%08x" % bits


def try_export_centered_affine_normalization(interpreter, model, inputs, captured_graph):
    """Accept only the whole pure recipe, validating before shape evaluation.

    A graph containing rsqrt is a candidate, not permission to reinterpret it.
    Other graph families retain their existing path; no class-name dispatch.
    """
    if captured_graph is None:
        return None
    import torch
    graph, traced = captured_graph
    if not any(node.op == "call_function" and node.target is torch.rsqrt for node in graph.nodes):
        return None
    try:
        input_nodes, result_node, epsilon = match_centered_affine_normalization_structure(traced)
        bits = epsilon_f32_bits(epsilon)
        if (len(inputs) != 3 or any(type(value) is not torch.Tensor or
                value.dtype != torch.float32 or not value.is_contiguous() for value in inputs)):
            raise CharacterizationError("three contiguous FP32 tensor inputs required")
        x, gamma, beta = inputs
        if (x.dim() != 2 or any(size <= 0 for size in x.shape) or
                tuple(gamma.shape) != (x.shape[1],) or tuple(beta.shape) != (x.shape[1],)):
            raise CharacterizationError("positive X[R,N], gamma[N], beta[N] required")
        # Only the exact pure recipe can execute for descriptor propagation.
        # Sample agreement cannot prove range, accuracy or numerical permission.
        from torch.fx.passes.shape_prop import ShapeProp
        ShapeProp(traced).propagate(*inputs)
        if characterize_row_graph(traced)["kind"] != "layernorm":
            raise CharacterizationError("centered affine characterization required")
    except CharacterizationError as exc:
        raise NotImplementedError("unsupported native centered affine normalization: " + str(exc)) from exc
    attrs = {"attr.axis": "1", "attr.accum_dtype": "float32",
             "attr.variance_kind": "population_centered", "attr.epsilon_bits": bits,
             "attr.reassociate": "false", "attr.state_reformulation": "false",
             "attr.contract": "false", "attr.rsqrt_semantics": "source_unresolved"}
    return export_native_row(interpreter, model, input_nodes, result_node,
                             common.MEAN_VARIANCE_NORM, "mean_variance_norm", attrs)
