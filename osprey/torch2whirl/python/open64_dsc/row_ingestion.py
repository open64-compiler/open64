"""Shared frontend emission for already-validated native row contracts.

Design: doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md and the native
reduction/Softmax contracts. C++ owns native records/types; this supplies descriptor
requests, formals, results and returns, never numerical or target decisions.
"""
import inspect

from .module import (WhirlModule, WhirlOperatorRecord, WhirlProgramUnitRecord,
                     WhirlTensorTypeRecord, WhirlValueRecord)
from .python_classes import collect_python_model_classes
from .python_imports import collect_python_import_census, resolve_reachable_imported_callables


def export_native_row(interpreter, model, input_nodes, result_node, opcode, stem, attrs):
    """Emit a pure whole-PU row recipe after structural/descriptor preflight.

    Callers supply the published exact attributes and matched source nodes.
    Sample values are never embedded as constants or numerical permissions.
    """
    path = result_node.meta.get("open64_source_path")
    line = result_node.meta.get("open64_source_line")
    if not path or not isinstance(line, int) or line <= 0:
        raise NotImplementedError("native " + stem + " requires an inspectable result call-site")
    builder = interpreter.builder()
    entry_name = interpreter._model_class_name(model)
    pu = builder.minimal_program_unit(entry_name)
    file_id = builder.register_source_file(pu, path)
    types = []
    handles = []
    named_nodes = [("input%d_type" % index, node) for index, node in enumerate(input_nodes)]
    named_nodes.append((stem + "_type", result_node))
    for name, node in named_nodes:
        shape = tuple(node.meta["tensor_meta"].shape)
        logical = "[" + ",".join(str(size) for size in shape) + "]"
        descriptor = dict(kind="tensor", dtype="float32", rank=len(shape),
                          logical_shape=logical, layout="contiguous")
        handle = builder.tensor_type(name, "float32", len(shape), logical, descriptor)
        handles.append(handle)
        types.append(WhirlTensorTypeRecord(name, handle.value, "float32",
                                          len(shape), logical, descriptor))
    sources = [builder.declare_pu_formal(pu, "input%d" % index, index, handles[index], file_id, line)
               for index in range(len(input_nodes))]
    builder.declare_pu_result(pu, stem + "_output", 0, handles[-1],
                              file_id=file_id, line=line)
    result = builder.operator(opcode, 1, sources, attrs,
                              result_name=stem + "_value", result_type=handles[-1])
    builder.set_value_source_position(result, file_id, line)
    builder.append_program_unit_value(pu, result)
    builder.return_pu_values(pu, [result])
    definitions, instances = collect_python_model_classes(model)
    model_module = inspect.getmodule(type(model))
    census = collect_python_import_census(model_module) if model_module is not None else None
    imports = census.callables if census else ()
    return WhirlModule(
        options=interpreter._options, model_name=interpreter._model_name(model),
        input_count=len(input_nodes), entry_function=WhirlProgramUnitRecord(
            entry_name, pu.value, [opcode]), graph_source="torch.fx",
        operators=[opcode], tensor_types=types,
        values=[WhirlValueRecord("input%d" % index, source.value, types[index].name, "formal",
                                 builder.value_result_symbol(source).value)
                for index, source in enumerate(sources)] +
               [WhirlValueRecord(stem + "_value", result.value, types[-1].name,
                                 "operator_result", builder.value_result_symbol(result).value)],
        graph_operators=[WhirlOperatorRecord(opcode, result.value,
            ["input%d" % index for index in range(len(input_nodes))], attrs, {"semantic_name": stem + "_value",
                                "source_file": path, "source_line": str(line),
                                "lowering_hint": "r1:native_contract_only"})],
        python_class_definitions=definitions, python_class_instances=instances,
        python_imports=imports,
        python_import_diagnostics=census.diagnostics if census else (),
        python_reachable_imports=resolve_reachable_imported_callables(
            model_module, imports, definitions, instances, entry_name),
    )
