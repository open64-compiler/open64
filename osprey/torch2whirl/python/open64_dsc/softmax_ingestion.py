"""Connect exact stable-Softmax source dataflow to its native WHIRL contract.

Design: doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md. No exp choice,
numerical authorization, target planning or executable lowering lives here.
"""
import operator

from .mapping import common
from .row_characterization import (CharacterizationError, characterize_row_graph,
                                   match_stable_softmax_structure)
from .row_ingestion import export_native_row


def try_export_stable_softmax(interpreter, model, inputs, captured_graph):
    """Consume only a complete exact row recipe, not an embedded or opaque call.

    Division-output candidates outside this subset fail before evaluation or
    builder allocation; other graph families keep their existing path.
    """
    if captured_graph is None:
        return None
    import torch
    graph, traced = captured_graph
    outputs = [node for node in graph.nodes if node.op == "output"]
    output = outputs[0].args[0] if len(outputs) == 1 and outputs[0].args else None
    if (getattr(output, "op", None) != "call_function" or
            output.target is not operator.truediv):
        return None
    try:
        input_node, result_node = match_stable_softmax_structure(traced)
        if (len(inputs) != 1 or type(inputs[0]) is not torch.Tensor or
                inputs[0].dtype != torch.float32 or inputs[0].dim() != 2 or
                any(size <= 0 for size in inputs[0].shape) or
                not inputs[0].is_contiguous()):
            raise CharacterizationError("positive contiguous rank-2 FP32 input required")
        # Only the structurally verified pure recipe may be shape-evaluated.
        # Values cannot establish finiteness, numerical acceptance or permission.
        from torch.fx.passes.shape_prop import ShapeProp
        ShapeProp(traced).propagate(*inputs)
        facts = characterize_row_graph(traced)
        if facts["kind"] != "softmax":
            raise CharacterizationError("stable-Softmax characterization required")
    except CharacterizationError as exc:
        raise NotImplementedError("unsupported native stable Softmax: " + str(exc)) from exc
    attrs = {"attr.axis": "1", "attr.accum_dtype": "float32",
             "attr.reassociate": "false", "attr.state_reformulation": "false",
             "attr.exp_semantics": "source_unresolved"}
    return export_native_row(interpreter, model, input_node, result_node,
                             common.STABLE_SOFTMAX, "stable_softmax", attrs)
