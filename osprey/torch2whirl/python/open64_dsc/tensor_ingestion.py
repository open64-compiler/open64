"""Pure FX tensor-expression ingestion using published native contracts.

Design: doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md.
Preflight reads descriptors and static graph attributes only; it never executes
tensor arithmetic or creates native products until the whole graph is admitted.
"""

import inspect
import operator
import struct

from .module import (WhirlModule, WhirlOperatorRecord, WhirlProgramUnitRecord,
                     WhirlTensorTypeRecord, WhirlValueRecord)
from .python_classes import collect_python_model_classes
from .tensor_contract import VERSIONS, infer_expression


def try_export_tensor_expressions(interpreter, model, inputs, captured):
    """Export supported pure tensor graphs, including views and composed MLA.

    Other model families retain their path. Once this lane is selected,
    unsupported nodes/attributes fail explicitly and the caller aborts its
    builder session. No model class name or sample tensor value selects it.
    """
    if captured is None:
        return None
    import torch
    graph, _ = captured
    functions = {
        torch.matmul: "matmul", operator.matmul: "matmul",
        torch.add: "add", operator.add: "add",
        torch.sub: "sub", operator.sub: "sub",
        torch.mul: "mul", operator.mul: "mul",
        torch.div: "div", operator.truediv: "div",
        torch.gt: "greater", operator.gt: "greater",
        torch.exp: "exp", torch.log: "log", torch.amax: "reduce_max",
        torch.sum: "reduce_sum", torch.isfinite: "isfinite",
        torch.where: "select", operator.invert: "logical_not",
        torch.logical_not: "logical_not", torch.full_like: "full_like",
        torch.ones_like: "ones_like", torch.zeros_like: "zeros_like",
        torch.unsqueeze: "unsqueeze", torch.squeeze: "squeeze",
        torch.transpose: "transpose",
        torch.abs: "abs", torch.round: "round_even", torch.clamp: "clamp",
        torch.index_select: "gather", torch.index_copy: "scatter",
        torch.remainder: "rem",
    }
    methods = {"unsqueeze", "squeeze", "transpose", "masked_fill", "reshape", "to"}
    legacy_methods = methods - {"reshape", "to"}
    triggers = legacy_methods | {"exp", "log", "isfinite", "select", "logical_not",
                          "full_like", "ones_like", "zeros_like", "abs", "round_even",
                          "clamp", "gather", "scatter", "rem"}
    nodes = list(graph.nodes)
    from .quantized_patterns import u4_patterns
    replacements, skipped = u4_patterns(nodes)
    pure_boundary = not any(node.op in ("call_module", "get_attr") for node in nodes)
    if not any((node.op == "call_method" and
                (node.target in legacy_methods or (pure_boundary and node.target in methods))) or
               (node.op == "call_function" and functions.get(node.target) in triggers)
               for node in nodes):
        return None
    types, plans, placeholders = {}, [], []
    definitions = {}
    output_names = None
    literal = 0

    def reject(node, reason):
        raise NotImplementedError("unsupported FX graph node: %s:%s (%s)" %
                                  (node.op, node.target, reason))

    # Classify the whole graph before descriptor propagation or native products.
    for node in nodes:
        if node in replacements or node in skipped:
            continue
        if (node.op in ("placeholder", "output") or
                (node.op == "call_method" and node.target in methods) or
                (node.op == "call_function" and node.target in functions)):
            continue
        reject(node, "not in the pure tensor-expression contract")

    def axis(value, rank, insertion=False):
        if type(value) is not int:
            raise ValueError("static integer axis required")
        limit = rank + (1 if insertion else 0)
        if not -limit <= value < limit:
            raise ValueError("axis out of range")
        return value % limit

    def emit(name, opcode, sources, attrs, node, version=None):
        if version is None:
            version = VERSIONS.get(opcode, 1)
        if name in types:
            raise ValueError("duplicate graph value")
        descriptor = infer_expression(opcode, version, attrs, [types[item] for item in sources])
        types[name] = descriptor
        plans.append((name, opcode, version, sources, attrs, node))
        definitions[name] = (opcode, sources, attrs)
        return name

    def ref(value, like, node):
        nonlocal literal
        if isinstance(value, torch.fx.Node):
            if value.name not in types:
                raise ValueError("non-topological operand")
            return value.name
        if type(value) not in (float, int) or like is None:
            raise ValueError("tensor or numeric scalar required")
        literal += 1
        if types[like][0] == "int64":
            if type(value) is not int:
                raise ValueError("integer index literal required")
            return emit("__literal_%d" % literal, "common.fill_like", [like],
                        {"attr.value_i64": str(value)}, node, 2)
        bits = struct.pack(">f", float(value)).hex()
        return emit("__literal_%d" % literal, "common.fill_like", [like],
                    {"attr.value_bits": bits}, node)

    try:
        for node in nodes:
            if node in skipped:
                continue
            if node.op == "placeholder":
                index = len(placeholders)
                if index >= len(inputs) or type(inputs[index]) is not torch.Tensor:
                    raise ValueError("tensor input required")
                tensor = inputs[index]
                shape = tuple(tensor.shape)
                dtype = str(tensor.dtype).replace("torch.", "")
                if (tensor.device.type != "cpu" or not tensor.is_contiguous() or
                        dtype not in ("float32", "bool", "int8", "uint8", "int64") or not 1 <= len(shape) <= 16 or
                        any(type(n) is not int or n <= 0 for n in shape)):
                    raise ValueError("positive contiguous static CPU FP32/bool inputs required")
                types[node.name] = dtype, shape
                placeholders.append(node.name)
                continue
            if node.op == "output":
                value = node.args[0]
                outputs = value if isinstance(value, (tuple, list)) else (value,)
                if not outputs or any(not isinstance(item, torch.fx.Node) or item.name not in types
                                      for item in outputs):
                    raise ValueError("flat tensor result tuple required")
                output_names = [item.name for item in outputs]
                continue
            if node in replacements:
                opcode, source_node = replacements[node]
                source = ref(source_node, None, node)
                if opcode == "common.pack_u4":
                    conversion = definitions.get(source)
                    if not conversion or conversion[0] != "common.convert" or conversion[2].get("attr.dst_dtype") != "uint8":
                        raise ValueError("pack requires proven nibble codes")
                    clamp = definitions.get(conversion[1][0])
                    if not clamp or clamp[0] != "common.clamp" or clamp[2] != {
                            "attr.has_min": "true", "attr.min_bits": "00000000",
                            "attr.has_max": "true", "attr.max_bits": "41700000"}:
                        raise ValueError("pack requires proven nibble bounds")
                emit(node.name, opcode, [source], {"attr.axis": str(len(types[source][1])-1),
                     "attr.nibble_order": "even_low", "attr.code_bits": "4"}, node)
                continue
            kind = (node.target if node.op == "call_method" and node.target in methods else
                    functions.get(node.target) if node.op == "call_function" else None)
            if kind is None:
                reject(node, "not in the pure tensor-expression contract")
            args, kwargs = node.args, dict(node.kwargs)
            if not args or not isinstance(args[0], torch.fx.Node):
                # Scalar-left binary forms are deliberately outside this bounded lane.
                raise ValueError("first argument must be a tensor")
            source = ref(args[0], None, node)
            rank = len(types[source][1])
            if kind == "reshape":
                if kwargs or len(args) < 2:
                    raise ValueError("static reshape required")
                shape = tuple(args[1]) if len(args) == 2 and isinstance(args[1], (tuple, list)) else tuple(args[1:])
                if any(type(n) is not int or n == 0 or n < -1 for n in shape) or shape.count(-1) > 1:
                    raise ValueError("invalid reshape extents")
                import math
                total, known = math.prod(types[source][1]), math.prod(n for n in shape if n != -1)
                if -1 in shape:
                    if total % known:
                        raise ValueError("inexact inferred reshape")
                    shape = tuple(total//known if n == -1 else n for n in shape)
                emit(node.name, "common.reshape", [source], {"attr.target_shape": ",".join(map(str, shape))}, node)
            elif kind == "to":
                if kwargs or len(args) != 2 or args[1] not in (torch.float32, torch.int8, torch.uint8):
                    raise ValueError("explicit byte/F32 dtype conversion required")
                src, dst = types[source][0], str(args[1]).replace("torch.", "")
                if src == "float32":
                    # A general torch cast truncates/wraps. checked_integral is
                    # equivalent only after a bounded integral producer.
                    clamp = definitions.get(source)
                    low, high = (-128,127) if dst == "int8" else (0,255)
                    if (not clamp or clamp[0] != "common.clamp" or
                            clamp[2].get("attr.has_min") != "true" or clamp[2].get("attr.has_max") != "true" or
                            not low <= struct.unpack(">f",bytes.fromhex(clamp[2]["attr.min_bits"]))[0] <= high or
                            not low <= struct.unpack(">f",bytes.fromhex(clamp[2]["attr.max_bits"]))[0] <= high):
                        raise ValueError("byte conversion requires integral clamp/range proof")
                    rounded = definitions.get(clamp[1][0])
                    if rounded and rounded[0] == "common.add":
                        literal_def = definitions.get(rounded[1][1])
                        if not literal_def or literal_def[0] != "common.fill_like":
                            raise ValueError("integral offset proof")
                        offset = struct.unpack(">f",bytes.fromhex(literal_def[2]["attr.value_bits"]))[0]
                        if not offset.is_integer():
                            raise ValueError("integral offset proof")
                        rounded = definitions.get(rounded[1][0])
                    if not rounded or rounded[0] != "common.round_even":
                        raise ValueError("byte conversion requires rounded integral producer")
                emit(node.name, "common.convert", [source], {"attr.src_dtype": src, "attr.dst_dtype": dst,
                     "attr.mode": "checked_integral" if src == "float32" else "exact"}, node)
            elif kind in ("abs", "round_even", "clamp"):
                attrs = {}
                if kind == "round_even":
                    attrs["attr.rounding"] = "ties_to_even"
                if kind == "clamp":
                    if len(args) > 3 or set(kwargs)-{"min", "max"} or (len(args)>1 and "min" in kwargs) or (len(args)>2 and "max" in kwargs):
                        raise ValueError("clamp bounds")
                    lo = args[1] if len(args)>1 else kwargs.get("min")
                    hi = args[2] if len(args)>2 else kwargs.get("max")
                    for name, value in (("min", lo), ("max", hi)):
                        if value is not None and type(value) not in (float, int):
                            raise ValueError("static clamp bounds required")
                        attrs["attr.has_"+name] = str(value is not None).lower()
                        attrs["attr."+name+"_bits"] = struct.pack(">f", 0.0 if value is None else float(value)).hex()
                elif kwargs or len(args) != 1:
                    raise ValueError("unary quantization attributes")
                emit(node.name, "common."+kind, [source], attrs, node)
            elif kind in ("gather", "scatter"):
                if kwargs or len(args) != (3 if kind == "gather" else 4):
                    raise ValueError("indexed operation arity")
                attrs = {"attr.axis": str(axis(args[1], rank)), "attr.bounds": "guarded_nonnegative",
                         "attr.alias_rule": "fresh_result"}
                sources = [source, ref(args[2], None, node)]
                if kind == "scatter":
                    attrs["attr.conflicts"] = "unique_indices"
                    sources.append(ref(args[3], None, node))
                emit(node.name, "common."+kind, sources, attrs, node, 2 if kind == "scatter" else 1)
            elif kind in ("unsqueeze", "squeeze", "transpose"):
                allowed = {"dim0", "dim1"} if kind == "transpose" else {"dim"}
                if set(kwargs) - allowed:
                    raise ValueError("unsupported view keyword")
                if ((kind != "transpose" and len(args) > 1 and "dim" in kwargs) or
                        (kind == "transpose" and
                         ((len(args) > 1 and "dim0" in kwargs) or
                          (len(args) > 2 and "dim1" in kwargs)))):
                    raise ValueError("duplicate view axis")
                if kind == "transpose":
                    if len(args) > 3: raise ValueError("transpose arity")
                    first = axis(args[1] if len(args) > 1 else kwargs["dim0"], rank)
                    second = axis(args[2] if len(args) > 2 else kwargs["dim1"], rank)
                    permutation = list(range(rank))
                    permutation[first], permutation[second] = permutation[second], permutation[first]
                    emit(node.name, "common.transpose", [source],
                         {"attr.permutation": ",".join(map(str, permutation))}, node)
                else:
                    if len(args) > 2: raise ValueError("view arity")
                    dim = args[1] if len(args) > 1 else kwargs.get("dim")
                    shape = list(types[source][1])
                    if kind == "unsqueeze":
                        shape.insert(axis(dim, rank, True), 1)
                    elif dim is None:
                        shape = [n for n in shape if n != 1]
                    else:
                        index = axis(dim, rank)
                        if shape[index] == 1:
                            shape.pop(index)
                    emit(node.name, "common.reshape", [source],
                         {"attr.target_shape": ",".join(map(str, shape))}, node)
            elif kind in ("full_like", "ones_like", "zeros_like"):
                if kwargs or len(args) != (2 if kind == "full_like" else 1):
                    raise ValueError("unsupported fill_like attributes")
                value = args[1] if kind == "full_like" else (1 if kind == "ones_like" else 0)
                if type(value) not in (float, int): raise ValueError("static fill scalar required")
                emit(node.name, "common.fill_like", [source],
                     {"attr.value_bits": struct.pack(">f", float(value)).hex()}, node)
            elif kind in ("reduce_max", "reduce_sum"):
                if set(kwargs) - {"dim", "keepdim", "dtype"} or len(args) > 3:
                    raise ValueError("unsupported reduction attributes")
                if ((len(args) > 1 and "dim" in kwargs) or
                        (len(args) > 2 and "keepdim" in kwargs)):
                    raise ValueError("duplicate reduction attribute")
                dim = args[1] if len(args) > 1 else kwargs.get("dim")
                keep = args[2] if len(args) > 2 else kwargs.get("keepdim", False)
                if type(keep) is not bool or kwargs.get("dtype", torch.float32) != torch.float32:
                    raise ValueError("FP32 reduction required")
                if kind == "reduce_max" and "dtype" in kwargs:
                    raise ValueError("amax dtype keyword is unsupported")
                emit(node.name, "common." + kind, [source], {
                    "attr.axis": str(axis(dim, rank)), "attr.keepdim": str(keep).lower(),
                    "attr.accum_dtype": "float32", "attr.reassociate": "false"}, node)
            elif kind == "masked_fill":
                if kwargs or len(args) != 3: raise ValueError("masked_fill arity")
                mask = ref(args[1], None, node)
                fill = ref(args[2], source, node)
                emit(node.name, "common.select", [mask, fill, source],
                     {"attr.broadcast_rule": "numpy"}, node)
            elif kind == "select":
                if kwargs or len(args) != 3: raise ValueError("where arity")
                emit(node.name, "common.select", [source, ref(args[1], None, node),
                     ref(args[2], None, node)], {"attr.broadcast_rule": "numpy"}, node)
            elif kind in ("exp", "log", "isfinite", "logical_not"):
                if kwargs or len(args) != 1: raise ValueError("unary expression arity")
                attrs = {"attr.fp_semantics": "source_unresolved"} if kind in ("exp", "log") else {}
                emit(node.name, "common." + kind, [source], attrs, node)
            else:
                integer = types[source][0] == "int64" and kind in ("add", "mul", "div", "rem")
                if (kwargs != ({"rounding_mode": "floor"} if integer and kind == "div" else {}) or len(args) != 2):
                    raise ValueError("binary expression attributes/arity")
                rhs = ref(args[1], source, node)
                attrs = ({"attr.transpose_kid0": "false", "attr.transpose_kid1": "false",
                          "attr.batch_rule": "numpy", "attr.accum_dtype": "float32"}
                         if kind == "matmul" else {"attr.broadcast_rule": "numpy"})
                if integer:
                    attrs["attr.integer_domain"] = "nonnegative_i64_checked"
                    if kind in ("div", "rem"):
                        attrs["attr.rounding"] = "floor"
                emit(node.name, "common." + kind, [source, rhs], attrs, node, 2 if integer else None)
        if len(placeholders) != len(inputs) or output_names is None:
            raise ValueError("input/output boundary mismatch")
    except (ValueError, TypeError, KeyError, OverflowError, struct.error) as error:
        raise NotImplementedError("unsupported static tensor expression: " + str(error)) from error

    # Explicit content roles belong to tensor descriptors. Only storage-aliasing
    # reshapes inherit them; materialized transpose/arithmetic results do not
    # acquire a persistent-cache role from their input's purpose.
    builder = interpreter.builder()
    records = [dict(name=name, opcode="formal", version=0, dtype=types[name][0],
                    shape=list(types[name][1]), operands=[], attrs={}) for name in placeholders]
    records += [dict(name=name, opcode=opcode, version=version, dtype=types[name][0],
                     shape=list(types[name][1]), operands=list(sources), attrs=dict(attrs))
                for name, opcode, version, sources, attrs, _ in plans]
    quantized_source = any(opcode in ("common.convert", "common.pack_u4", "common.unpack_u4", "common.round_even")
                           for _, opcode, _, _, _, _ in plans)
    inferred = builder.infer_quantized_cache_traits(records) if quantized_source else {}
    from .tensor_traits import declared_cache_traits
    cache_traits = declared_cache_traits(
        model, [node for node in nodes if node.op == "placeholder"], types, inferred)
    cache_traits.update({name: fact["role"] for name, fact in inferred.items()})
    for name, opcode, _, sources, _, _ in plans:
        if opcode == "common.reshape" and sources[0] in cache_traits:
            cache_traits[name] = cache_traits[sources[0]]

    # All source nodes and descriptors are admitted before the first PU product.
    entry = interpreter._model_class_name(model)
    pu = builder.minimal_program_unit(entry)
    path = inspect.getsourcefile(model.forward)
    line = inspect.getsourcelines(model.forward)[1]
    if not path:
        raise NotImplementedError("inspectable tensor source required")
    file_id = builder.register_source_file(pu, path)
    type_records, values, operators, handles, type_handles = [], [], [], {}, {}

    def tensor_type(name):
        dtype, shape = types[name]
        logical = "[" + ",".join(map(str, shape)) + "]"
        # Logical layout carries tensor values, not a physical allocation or copy.
        descriptor = dict(kind="tensor", dtype=dtype, rank=len(shape),
                          logical_shape=logical, layout="logical")
        if name in cache_traits:
            descriptor["traits"] = cache_traits[name]
        if name in inferred and inferred[name]["encoding"] and dtype in ("int8", "uint8"):
            descriptor["quantization"] = inferred[name]["encoding"]
        ty = builder.tensor_type(name + "_type", dtype, len(shape), logical, descriptor)
        type_records.append(WhirlTensorTypeRecord(name + "_type", ty.value, dtype,
                                                  len(shape), logical, descriptor))
        type_handles[name] = ty
        return ty

    for ordinal, name in enumerate(placeholders):
        ty = tensor_type(name)
        handle = builder.declare_pu_formal(pu, name, ordinal, ty, file_id, line)
        handles[name] = handle
        if name in inferred:
            builder.attach_value_metadata(handle, {"cache.inference."+k: v for k, v in inferred[name].items()})
        values.append(WhirlValueRecord(name, handle.value, name + "_type", "formal",
                                      builder.value_result_symbol(handle).value))
    for name, opcode, version, sources, attrs, node in plans:
        ty = tensor_type(name)
        handle = builder.operator(opcode, version, [handles[item] for item in sources],
                                  attrs, result_name=name, result_type=ty)
        builder.set_value_source_position(handle, file_id, line)
        interpreter._bind_fx_source_position(pu, node, handle)
        builder.append_program_unit_value(pu, handle)
        handles[name] = handle
        if name in inferred:
            builder.attach_value_metadata(handle, {"cache.inference."+k: v for k, v in inferred[name].items()})
        values.append(WhirlValueRecord(name, handle.value, name + "_type", "operator_result",
                                      builder.value_result_symbol(handle).value))
        metadata = dict(interpreter._fx_node_metadata(node, opcode))
        metadata.update(semantic_name=name, operator_version=str(version),
                        source_contract="tensor_expression_v1",
                        source_file=node.meta.get("open64_source_path", path),
                        source_line=str(node.meta.get("open64_source_line", line)))
        operators.append(WhirlOperatorRecord(opcode, handle.value, sources, attrs, metadata))
    for ordinal, name in enumerate(output_names):
        builder.declare_pu_result(pu, "output%d" % ordinal, ordinal, type_handles[name],
                                  file_id=file_id, line=line)
    builder.return_pu_values(pu, [handles[name] for name in output_names])
    definitions, instances = collect_python_model_classes(model)
    return WhirlModule(
        options=interpreter._options, model_name=interpreter._model_name(model),
        input_count=len(inputs), entry_function=WhirlProgramUnitRecord(
            entry, pu.value, [item.name for item in operators]), graph_source="torch.fx",
        operators=[item.name for item in operators], tensor_types=type_records,
        values=values, graph_operators=operators,
        python_class_definitions=definitions, python_class_instances=instances)
