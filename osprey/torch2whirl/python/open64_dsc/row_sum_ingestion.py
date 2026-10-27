"""Connect the exact R1 row-sum matcher to the published native contract.

Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md. No target planning,
numerical permission inference or executable reduction implementation lives here.
"""
import inspect

from .mapping import common
from .module import (WhirlModule, WhirlOperatorRecord, WhirlProgramUnitRecord,
                     WhirlTensorTypeRecord, WhirlValueRecord)
from .python_classes import collect_python_model_classes
from .python_imports import collect_python_import_census, resolve_reachable_imported_callables
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

    path = sum_node.meta.get("open64_source_path")
    line = sum_node.meta.get("open64_source_line")
    if not path or not isinstance(line, int) or line <= 0:
        raise NotImplementedError("native row sum requires an inspectable sum call-site")
    builder = interpreter.builder()
    entry_name = interpreter._model_class_name(model)
    pu = builder.minimal_program_unit(entry_name)
    file_id = builder.register_source_file(pu, path)
    types = []
    handles = []
    for name, node in (("input0_type", input_node), ("row_sum_type", sum_node)):
        shape = tuple(node.meta["tensor_meta"].shape)
        logical = "[" + ",".join(str(size) for size in shape) + "]"
        descriptor = dict(kind="tensor", dtype="float32", rank=len(shape),
                          logical_shape=logical, layout="contiguous")
        handle = builder.tensor_type(name, "float32", len(shape), logical, descriptor)
        handles.append(handle)
        types.append(WhirlTensorTypeRecord(name, handle.value, "float32",
                                          len(shape), logical, descriptor))
    source = builder.declare_pu_formal(pu, "input0", 0, handles[0], file_id, line)
    builder.declare_pu_result(pu, "row_sum_output", 0, handles[1],
                              file_id=file_id, line=line)
    attrs = {"attr.axis": "1", "attr.keepdim": "false",
             "attr.accum_dtype": "float32", "attr.reassociate": "false"}
    result = builder.operator(common.REDUCE_SUM, 1, [source], attrs,
                              result_name="row_sum_value", result_type=handles[1])
    builder.set_value_source_position(result, file_id, line)
    builder.append_program_unit_value(pu, result)
    builder.return_pu_values(pu, [result])
    definitions, instances = collect_python_model_classes(model)
    model_module = inspect.getmodule(type(model))
    census = collect_python_import_census(model_module) if model_module is not None else None
    imports = census.callables if census else ()
    return WhirlModule(
        options=interpreter._options, model_name=interpreter._model_name(model),
        input_count=1, entry_function=WhirlProgramUnitRecord(
            entry_name, pu.value, [common.REDUCE_SUM]), graph_source="torch.fx",
        operators=[common.REDUCE_SUM], tensor_types=types,
        values=[WhirlValueRecord("input0", source.value, types[0].name, "formal",
                                 builder.value_result_symbol(source).value),
                WhirlValueRecord("row_sum_value", result.value, types[1].name,
                                 "operator_result", builder.value_result_symbol(result).value)],
        graph_operators=[WhirlOperatorRecord(common.REDUCE_SUM, result.value,
            ["input0"], attrs, {"semantic_name": "row_sum_value",
                                "source_file": path, "source_line": str(line),
                                "lowering_hint": "r1:native_contract_only"})],
        python_class_definitions=definitions, python_class_instances=instances,
        python_imports=imports,
        python_import_diagnostics=census.diagnostics if census else (),
        python_reachable_imports=resolve_reachable_imported_callables(
            model_module, imports, definitions, instances, entry_name),
    )
