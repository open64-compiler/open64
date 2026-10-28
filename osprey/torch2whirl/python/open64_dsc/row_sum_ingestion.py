"""Connect the exact R1 row-sum matcher to the published native contract.

Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md. No target planning,
numerical permission inference or executable reduction implementation lives here.
"""
from .mapping import common
from .row_ingestion import export_native_row
from .row_characterization import (CharacterizationError, characterize_row_graph,
                                   match_row_sum_structure)


def try_export_row_sum(interpreter, model, inputs, captured_graph):
    """Return a native-contract module only for a complete exact sum graph.

    Other graph families retain their existing path. A sum-output graph outside
    this slice fails explicitly rather than falling back to a synthetic model.
    """
    if captured_graph is None:
        return None
    import torch
    graph, traced = captured_graph
    if len(list(graph.nodes)) != 3:
        return None
    outputs = [node for node in graph.nodes if node.op == "output"]
    output = outputs[0].args[0] if len(outputs) == 1 and outputs[0].args else None
    if (getattr(output, "op", None) != "call_function" or
            output.target is not torch.sum):
        return None
    try:
        input_node, sum_node = match_row_sum_structure(traced)
        if (len(inputs) != 1 or type(inputs[0]) is not torch.Tensor or
                inputs[0].dtype != torch.float32 or inputs[0].dim() != 2 or
                any(size <= 0 for size in inputs[0].shape) or
                not inputs[0].is_contiguous()):
            raise CharacterizationError("positive contiguous rank-2 FP32 input required")
        # Only the already-validated pure three-node graph may be evaluated.
        # ShapeProp establishes shapes, not numerical acceptance or permission.
        from torch.fx.passes.shape_prop import ShapeProp
        ShapeProp(traced).propagate(*inputs)
        facts = characterize_row_graph(traced)
        if facts["kind"] != "reduction":
            raise CharacterizationError("row-sum characterization required")
    except CharacterizationError as exc:
        raise NotImplementedError("unsupported native row sum: " + str(exc)) from exc

    attrs = {"attr.axis": "1", "attr.keepdim": "false",
             "attr.accum_dtype": "float32", "attr.reassociate": "false"}
    return export_native_row(interpreter, model, (input_node,), sum_node,
                             common.REDUCE_SUM, "row_sum", attrs)
