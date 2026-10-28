"""Read-only FX row-region characterization; not a WHIRL optimizer or IR.

Scope/authority: BUILDING-BLOCK-INGESTION-PLAN.md and
doc/AI-COMPILER-COMMON-EXECUTION-PLANNING.md. Recognize bounded pure FP32
dataflows, derive semantic requirements and report conditional alternatives.
No builder calls, native opcode allocation, target policy or transformation.
"""

from dataclasses import dataclass
import hashlib
import json
import math
import operator


class CharacterizationError(ValueError):
    """The graph lies outside the explicitly recognized row-region subset."""


@dataclass(frozen=True)
class RowNumericalPermissions:
    """Independent caller permissions, never inferred from sample agreement."""

    reassociate: bool = False
    state_reformulation: bool = False

    def __post_init__(self):
        for value in (self.reassociate, self.state_reformulation):
            if type(value) is not bool:
                raise ValueError("numerical permissions must be explicit booleans")


class _RowMatcher:
    """Match edges and static attributes; reject extra effects or unowned nodes."""

    def __init__(self, graph_module):
        import torch
        self.torch = torch
        self.nodes = list(graph_module.graph.nodes)
        self.inputs = [n for n in self.nodes if n.op == "placeholder"]
        self.used = set(self.inputs)
        outputs = [n for n in self.nodes if n.op == "output"]
        if len(outputs) != 1 or len(outputs[0].args) != 1 or outputs[0].kwargs:
            raise CharacterizationError("one tensor output required")
        self.used.add(outputs[0])
        self.output = outputs[0].args[0]
        if not hasattr(self.output, "op"):
            raise CharacterizationError("one tensor node output required")
        self.output_node = outputs[0]

    def call(self, node, target, arity):
        """Require an exact pure callable identity, arity and no extra options."""
        if (getattr(node, "op", None) != "call_function" or
                node.target is not target or len(node.args) != arity or node.kwargs):
            raise CharacterizationError("unsupported dataflow or callable")
        self.used.add(node)
        return node.args

    def reduce(self, node, target, keepdim):
        """Canonicalize only explicit last-axis FP32 reductions; no dtype casts."""
        if getattr(node, "op", None) != "call_function" or node.target is not target:
            raise CharacterizationError("unexpected reduction operator")
        if not 1 <= len(node.args) <= 3 or set(node.kwargs) - {"dim", "keepdim"}:
            raise CharacterizationError("unsupported reduction options")
        if (len(node.args) >= 2 and "dim" in node.kwargs or
                len(node.args) == 3 and "keepdim" in node.kwargs):
            raise CharacterizationError("duplicate reduction attributes")
        dim = node.args[1] if len(node.args) >= 2 else node.kwargs.get("dim")
        actual_keep = node.args[2] if len(node.args) == 3 else node.kwargs.get("keepdim", False)
        if isinstance(dim, (tuple, list)) and len(dim) == 1:
            dim = dim[0]
        if type(dim) is not int or dim not in (-1, 1):
            raise CharacterizationError("only the last axis of a rank-2 input is supported")
        if type(actual_keep) is not bool or actual_keep != keepdim:
            raise CharacterizationError("reduction keepdim mismatch")
        self.used.add(node)
        return node.args[0]

    def shape(self, node, expected=None):
        """Require concrete propagated FP32 metadata and validate inferred shapes."""
        meta = getattr(node, "meta", {}).get("tensor_meta")
        if meta is None or not hasattr(meta, "shape") or meta.dtype != self.torch.float32:
            raise CharacterizationError("concrete ShapeProp FP32 tensor metadata required")
        shape = tuple(meta.shape)
        if any(type(size) is not int or size <= 0 for size in shape):
            raise CharacterizationError("positive concrete extents required")
        if expected is not None and shape != tuple(expected):
            raise CharacterizationError("propagated shape disagrees with row dataflow")
        return shape

    def stable_softmax(self):
        """Check exact max/subtract/exp/sum/divide edges without evaluating them."""
        t = self.torch
        if len(self.inputs) != 1:
            raise CharacterizationError("Softmax requires one row input without unused arguments")
        x = self.inputs[0]
        numerator, denominator = self.call(self.output, operator.truediv, 2)
        shifted, = self.call(numerator, t.exp, 1)
        value, maximum = self.call(shifted, operator.sub, 2)
        if (value is not x or self.reduce(maximum, t.amax, True) is not x or
                self.reduce(denominator, t.sum, True) is not numerator):
            raise CharacterizationError("Softmax max/exp/sum must share the same row dataflow")
        return maximum, denominator

    def centered_affine_normalization(self):
        """Check exact population-centered affine edges without evaluation."""
        t = self.torch
        if len(self.inputs) != 3:
            raise CharacterizationError("normalization requires X, gamma and beta inputs")
        x, gamma, beta = self.inputs
        scaled, actual_beta = self.call(self.output, operator.add, 2)
        normalized, actual_gamma = self.call(scaled, operator.mul, 2)
        centered, inverse = self.call(normalized, operator.mul, 2)
        variance_plus_eps, = self.call(inverse, t.rsqrt, 1)
        variance, epsilon = self.call(variance_plus_eps, operator.add, 2)
        square = self.reduce(variance, t.mean, True)
        lhs, rhs = self.call(square, operator.mul, 2)
        value, mean = self.call(centered, operator.sub, 2)
        if (actual_beta is not beta or actual_gamma is not gamma or value is not x or
                lhs is not centered or rhs is not centered or
                self.reduce(mean, t.mean, True) is not x):
            raise CharacterizationError("LayerNorm requires centered population variance and affine inputs")
        try:
            valid = type(epsilon) in (float, int) and math.isfinite(epsilon) and epsilon > 0
        except OverflowError:
            valid = False
        if not valid:
            raise CharacterizationError("positive finite scalar epsilon required")
        return [mean, variance, variance_plus_eps, inverse], epsilon

    def match(self):
        """Derive kind, axes, sizes, epsilon and accesses from the actual graph."""
        t = self.torch
        if len(self.inputs) not in (1, 3):
            raise CharacterizationError("one row tensor or row tensor plus gamma/beta required")
        x = self.inputs[0]
        shape = self.shape(x)
        if len(shape) != 2:
            raise CharacterizationError("rank-2 row tensor required")
        rows, width = shape
        stats = []
        epsilon = None
        if self.output.op == "call_function" and self.output.target is t.sum:
            if len(self.inputs) != 1 or self.reduce(self.output, t.sum, False) is not x:
                raise CharacterizationError("sum must directly reduce the row input")
            kind, result_shape = "reduction", (rows,)
        elif self.output.op == "call_function" and self.output.target is operator.truediv:
            stats = list(self.stable_softmax())
            kind, result_shape = "softmax", shape
        else:
            stats, epsilon = self.centered_affine_normalization()
            kind, result_shape = "layernorm", shape
        if self.used != set(self.nodes):
            raise CharacterizationError("extra operations, effects, attributes or unused inputs are unsupported")
        for node in self.nodes:
            expected = result_shape if node in (self.output, self.output_node) else shape
            if node in stats:
                expected = (rows, 1)
            elif node in self.inputs[1:]:
                expected = (width,)
            self.shape(node, expected)
        return kind, rows, width, epsilon


def _fingerprint(nodes):
    """Bind facts to topology/attributes/metadata, not Python class or node names."""
    ids = {node: index for index, node in enumerate(nodes)}

    def encode(value):
        if hasattr(value, "op") and hasattr(value, "users"):
            return {"node": ids[value]}
        if isinstance(value, (tuple, list)):
            return [encode(item) for item in value]
        if isinstance(value, dict):
            return {key: encode(item) for key, item in sorted(value.items())}
        return value

    structure = []
    for node in nodes:
        target = node.target
        if node.op == "call_function":
            target = target.__module__ + "." + target.__name__
        elif node.op == "placeholder":
            target = "input"
        meta = node.meta["tensor_meta"]
        structure.append([node.op, str(target), encode(node.args), encode(node.kwargs),
                          list(meta.shape), str(meta.dtype), list(meta.stride)])
    return hashlib.sha256(json.dumps(structure, sort_keys=True).encode("ascii")).hexdigest()


def match_row_sum_structure(graph_module):
    """Validate the complete pure sum graph before any shape evaluation.

    Shared by read-only R1 analysis and ingestion. This publishes no IR and
    deliberately excludes embedded sums, extra operations and unused inputs.
    """
    matcher = _RowMatcher(graph_module)
    if (len(matcher.inputs) != 1 or
            matcher.reduce(matcher.output, matcher.torch.sum, False)
            is not matcher.inputs[0] or matcher.used != set(matcher.nodes)):
        raise CharacterizationError("one direct row sum and one input required")
    return matcher.inputs[0], matcher.output


def match_stable_softmax_structure(graph_module):
    """Preflight the complete pure source recipe before ShapeProp or allocation."""
    matcher = _RowMatcher(graph_module)
    matcher.stable_softmax()
    if matcher.used != set(matcher.nodes):
        raise CharacterizationError("extra Softmax operations, effects or unused inputs")
    return matcher.inputs[0], matcher.output


def match_centered_affine_normalization_structure(graph_module):
    """Preflight the complete pure affine recipe before ShapeProp/allocation."""
    matcher = _RowMatcher(graph_module)
    _, epsilon = matcher.centered_affine_normalization()
    if matcher.used != set(matcher.nodes):
        raise CharacterizationError("extra normalization operations, effects or unused inputs")
    return tuple(matcher.inputs), matcher.output, epsilon


def characterize_row_graph(graph_module, permissions=None):
    """Return analysis-only facts for a complete pure row graph, without mutation.

    Caller supplies current ShapeProp metadata. The fingerprint invalidates the
    report after graph/attribute/descriptor changes; finite/overflow conditions
    remain unresolved runtime/numerical obligations, not facts from one sample.
    """
    permissions = RowNumericalPermissions() if permissions is None else permissions
    if not isinstance(permissions, RowNumericalPermissions):
        raise ValueError("RowNumericalPermissions required")
    matcher = _RowMatcher(graph_module)
    kind, rows, width, epsilon = matcher.match()
    fingerprint = _fingerprint(matcher.nodes)
    specs = {
        "reduction": {
            "state": {"identity": "0", "update": "s + X[r,k]", "merge": "s_left + s_right",
                      "finalize": "s"},
            "strategies": [("source_sum", ()), ("partition_and_merge", ("reassociate",))],
            "native_requirements": ["sum axes/keepdim/identity/order", "row region input/result contract"],
        },
        "softmax": {
            "state": {"source": "m=max(X[r,:]); l=sum(exp(X[r,:]-m))",
                      "identity": "empty state (n=0), handled before exponent evaluation",
                      "merge": "m=max(m1,m2); l=exp(m1-m)*l1 + exp(m2-m)*l2",
                      "finalize": "Y[r,j]=exp(X[r,j]-m)/l"},
            "strategies": [("retain_exp", ()), ("recompute_exp_same_reductions", ()),
                           ("online_max_sum_state", ("reassociate", "state_reformulation"))],
            "native_requirements": ["amax/sum reduction contracts", "sub/exp/div broadcast and numerical policy",
                                    "stable normalization region boundary"],
        },
        "layernorm": {
            "state": {"source": "mu=mean(X[r,:]); var=mean((X[r,:]-mu)^2)",
                      "alternate": "Welford (n,mu,M2)", "identity": "n=0; mu=0; M2=0",
                      "merge": "n=n1+n2; d=mu2-mu1; mu=mu1+d*n2/n; M2=M2_1+M2_2+d*d*n1*n2/n; empty-state guards",
                      "finalize": "(X[r,j]-mu)*rsqrt(var+epsilon)*gamma[j]+beta[j]",
                      "alternate_finalize": "var=M2/n"},
            "strategies": [("centered_population_variance", ()),
                           ("welford_partition_merge", ("reassociate", "state_reformulation"))],
            "native_requirements": ["mean with population denominator", "centered variance/epsilon/rsqrt contract",
                                    "affine broadcast and normalization region boundary"],
        },
    }
    spec = specs[kind]
    alternatives = []
    for strategy, required in spec["strategies"]:
        missing = [name for name in required if not getattr(permissions, name)]
        alternatives.append({"id": strategy, "required_permissions": list(required),
                             "missing_permissions": missing,
                             "status": "permission_rejected" if missing else "requires_numerical_acceptance",
                             "selected": False})
    accesses = [{"operand": matcher.inputs[0].name, "relation": "X[r,k]", "mode": "read", "phase": "statistics"}]
    if kind != "reduction":
        accesses.append({"operand": matcher.inputs[0].name, "relation": "X[r,j]",
                         "mode": "read", "phase": "finalize_semantics_not_physical_traffic"})
    if kind == "layernorm":
        accesses.extend({"operand": node.name, "relation": label + "[j]", "mode": "read", "phase": "finalize"}
                        for node, label in zip(matcher.inputs[1:], ("gamma", "beta")))
    return {
        "schema": 1, "kind": kind, "status": "source_characterized_under_preconditions",
        "graph_sha256": fingerprint, "region_id": kind + ":" + fingerprint,
        "source_nodes": [node.name for node in matcher.nodes],
        "domains": {"output_rows": [0, rows], "reduction": [0, width], "axis": 1,
                    "output_columns": [0, width] if kind != "reduction" else None,
                    "indices": {"row": "r", "reduction": "k", "output_column": "j" if kind != "reduction" else None},
                    "output_shape": list(matcher.output.meta["tensor_meta"].shape)},
        "dtype": "float32", "epsilon": epsilon, "variance_denominator": width if kind == "layernorm" else None,
        "accesses": accesses, "output_access": "Y[r]" if kind == "reduction" else "Y[r,j]",
        "effects": {"input_writes": False, "input_alias": "may_alias; all accesses read-only",
                    "result_storage": "fresh pure-op result; no physical placement selected"},
        "ownership": {"logical_output_owner": "one owner per output element",
                      "partition_merge_owner": "required if partitioned; not yet mapped to threads/CTAs"},
        "state": spec["state"],
        "state_proof_scope": "matched real-arithmetic recipe; floating-point equivalence not proved",
        "preconditions": {"positive_concrete_shapes": "proved_from_metadata",
                          "finite_inputs": "not_proved", "numerical_intermediate_domain": "not_proved",
                          "candidate_error_acceptance": "not_proved"},
        "permissions": {"reassociate": permissions.reassociate, "state_reformulation": permissions.state_reformulation},
        "alternatives": alternatives, "native_requirements": spec["native_requirements"],
        "native_whirl": "pending; no native operators or canonicalization added",
        "transformation": "none", "cost_comparison": "not_performed",
    }
