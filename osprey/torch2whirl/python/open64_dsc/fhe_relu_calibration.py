"""Deterministic, identity-bound ReLU range calibration support."""

from __future__ import annotations

import copy
import hashlib
import json
import math
import platform
import re
from dataclasses import dataclass
from typing import Any, Iterable, Mapping, Optional, Sequence


SCHEMA = "open64.fhe.relu.context-ranges.v1"
APPROVED_STATUS = "approved"
EXPECTED_CONTEXT_COUNT = 19
PROFILE_NAME = "ace.chebyshev.sign.7x15x13.depth11.v1"
COEFFICIENT_MANIFEST_SHA256 = (
    "75132d449852303ec3e44e86c8a5b5ffc196c0643cf7fadff453d797c2266931"
)


class CalibrationManifestError(ValueError):
    """Raised when calibration evidence is incomplete or inconsistent."""


@dataclass(frozen=True)
class CalibrationSample:
    """One deterministically ordered model invocation."""

    sample_id: str
    inputs: tuple[Any, ...]


def canonical_json_bytes(value: Mapping[str, Any]) -> bytes:
    return (
        json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True)
        + "\n"
    ).encode("ascii")


def manifest_sha256(manifest: Mapping[str, Any]) -> str:
    content = copy.deepcopy(dict(manifest))
    content.pop("manifest_sha256", None)
    return hashlib.sha256(canonical_json_bytes(content)).hexdigest()


def finalize_manifest(manifest: Mapping[str, Any]) -> dict[str, Any]:
    result = copy.deepcopy(dict(manifest))
    result["manifest_sha256"] = manifest_sha256(result)
    return result


def file_sha256(path: Any) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as stream:
        while True:
            block = stream.read(1024 * 1024)
            if not block:
                break
            digest.update(block)
    return digest.hexdigest()


def _update_tensor_digest(digest: Any, name: str, tensor: Any) -> None:
    contiguous = tensor.detach().to(device="cpu").contiguous()
    digest.update(name.encode("utf-8"))
    digest.update(b"\0")
    digest.update(str(contiguous.dtype).encode("ascii"))
    digest.update(b"\0")
    digest.update(canonical_json_bytes({"shape": list(contiguous.shape)}))
    digest.update(contiguous.numpy().tobytes(order="C"))


def model_state_sha256(model: Any) -> str:
    digest = hashlib.sha256()
    for name, tensor in sorted(model.state_dict().items()):
        _update_tensor_digest(digest, name, tensor)
    return digest.hexdigest()


def _require_sha256(value: object, field: str) -> None:
    if not isinstance(value, str) or len(value) != 64:
        raise CalibrationManifestError(f"{field} must be a SHA-256 string")
    if any(ch not in "0123456789abcdef" for ch in value):
        raise CalibrationManifestError(f"{field} must use lowercase hexadecimal")


def _identity_key(context: Mapping[str, Any]) -> tuple[object, ...]:
    return (
        context.get("owner_pu_st"),
        context.get("source_relu_value_id"),
        context.get("context_pu_identity_id"),
        context.get("context_callsite_id"),
    )


def _routing_key(context: Mapping[str, Any]) -> tuple[object, ...]:
    return (context.get("module_path"), context.get("invocation_ordinal"))


def validate_context_template(contexts: Sequence[Mapping[str, Any]]) -> None:
    if len(contexts) != EXPECTED_CONTEXT_COUNT:
        raise CalibrationManifestError("exactly 19 ReLU contexts are required")
    identities: set[tuple[object, ...]] = set()
    routes: set[tuple[object, ...]] = set()
    root_count = 0
    for context in contexts:
        identity = _identity_key(context)
        route = _routing_key(context)
        if (
            not isinstance(identity[0], str)
            or re.fullmatch(r"<\d+,\d+>", identity[0]) is None
            or any(not isinstance(item, int) for item in identity[1:])
            or any(item <= 0 for item in identity[1:3])
            or identity[3] < 0
        ):
            raise CalibrationManifestError("invalid ReLU context identity")
        if identity in identities:
            raise CalibrationManifestError("duplicate ReLU context identity")
        identities.add(identity)
        if (
            not isinstance(route[0], str)
            or not route[0]
            or not isinstance(route[1], int)
            or route[1] < 0
            or route in routes
        ):
            raise CalibrationManifestError("invalid or duplicate ReLU route")
        routes.add(route)
        if identity[3] == 0:
            root_count += 1
    if root_count != 1:
        raise CalibrationManifestError(
            "one root and eighteen called ReLU contexts are required"
        )


def validate_calibration_manifest(
    manifest: Mapping[str, Any],
    *,
    expected_contexts: Optional[Sequence[Mapping[str, Any]]] = None,
    require_approved: bool = False,
) -> None:
    if manifest.get("schema") != SCHEMA:
        raise CalibrationManifestError("unknown ReLU calibration schema")
    if manifest.get("status") not in ("candidate_unapproved", APPROVED_STATUS):
        raise CalibrationManifestError("unknown ReLU calibration status")
    if manifest.get("profile_name") != PROFILE_NAME:
        raise CalibrationManifestError("unexpected ReLU composite profile")
    _require_sha256(manifest.get("coefficient_manifest_sha256"),
                    "coefficient_manifest_sha256")
    if manifest.get("coefficient_manifest_sha256") != \
            COEFFICIENT_MANIFEST_SHA256:
        raise CalibrationManifestError("coefficient manifest identity mismatch")
    _require_sha256(manifest.get("manifest_sha256"), "manifest_sha256")
    if manifest_sha256(manifest) != manifest.get("manifest_sha256"):
        raise CalibrationManifestError("calibration manifest hash mismatch")
    source_artifact = manifest.get("source_artifact")
    if not isinstance(source_artifact, dict):
        raise CalibrationManifestError("source artifact identity is missing")
    for field in (
        "binary_whirl_sha256",
        "model_source_sha256",
        "parameter_payload_sha256",
    ):
        _require_sha256(source_artifact.get(field), field)

    contexts = manifest.get("contexts")
    if not isinstance(contexts, list):
        raise CalibrationManifestError("calibration contexts are missing")
    validate_context_template(contexts)
    if expected_contexts is not None:
        validate_context_template(expected_contexts)
        if (
            [_identity_key(item) for item in contexts] !=
                [_identity_key(item) for item in expected_contexts]
            or [_routing_key(item) for item in contexts] !=
                [_routing_key(item) for item in expected_contexts]
        ):
            raise CalibrationManifestError(
                "calibration identities do not match persisted Open64 identities"
            )

    collection = manifest.get("collection")
    authority = manifest.get("calibration_authority")
    if not isinstance(collection, dict) or not isinstance(authority, dict):
        raise CalibrationManifestError("calibration authority is incomplete")
    sample_count = collection.get("sample_count")
    if not isinstance(sample_count, int) or sample_count <= 0:
        raise CalibrationManifestError("calibration sample count is invalid")
    _require_sha256(collection.get("sample_order_sha256"),
                    "sample_order_sha256")
    _require_sha256(collection.get("observed_input_sha256"),
                    "observed_input_sha256")
    _require_sha256(collection.get("observed_model_state_sha256"),
                    "observed_model_state_sha256")
    sample_ids = collection.get("sample_ids")
    if (
        not isinstance(sample_ids, list)
        or len(sample_ids) != sample_count
        or len(set(sample_ids)) != sample_count
        or any(not isinstance(item, str) or not item for item in sample_ids)
    ):
        raise CalibrationManifestError("calibration sample order is invalid")
    expected_order_hash = hashlib.sha256(
        canonical_json_bytes({"sample_ids": sample_ids})
    ).hexdigest()
    if collection.get("sample_order_sha256") != expected_order_hash:
        raise CalibrationManifestError("calibration sample-order hash mismatch")
    if authority.get("out_of_range_policy") != "reject":
        raise CalibrationManifestError("v1 out-of-range policy must be reject")

    for context in contexts:
        for field in (
            "observed_min",
            "observed_max",
            "observed_abs_max",
            "per_sample_abs_q99",
            "per_sample_abs_q999",
            "bound_b",
        ):
            value = context.get(field)
            if not isinstance(value, (int, float)) or not math.isfinite(value):
                raise CalibrationManifestError(
                    f"context {context.get('instance_path')} lacks {field}"
                )
        if context["observed_min"] > context["observed_max"]:
            raise CalibrationManifestError("context extrema are reversed")
        if context["bound_b"] <= 0:
            raise CalibrationManifestError("normalization bound B must be positive")
        if context["bound_b"] < context["observed_abs_max"]:
            raise CalibrationManifestError("normalization bound excludes observations")
        if context.get("sample_count") != sample_count:
            raise CalibrationManifestError("context sample count is incomplete")
        if not isinstance(context.get("element_count"), int) or \
                context["element_count"] <= 0:
            raise CalibrationManifestError("context element count is invalid")
        if context.get("nonfinite_count") != 0:
            raise CalibrationManifestError("nonfinite calibration value observed")
        if context.get("outlier_count") != 0:
            raise CalibrationManifestError("calibration bound has outliers")

    if not require_approved:
        return
    if manifest.get("status") != APPROVED_STATUS:
        raise CalibrationManifestError("calibration manifest is not approved")
    required_authority = (
        "dataset_name",
        "dataset_version",
        "dataset_split",
        "dataset_sha256",
        "preprocessing",
        "preprocessing_sha256",
        "trained_checkpoint_identity",
        "trained_checkpoint_sha256",
        "collector_revision",
        "environment",
        "bound_rule",
    )
    if any(not authority.get(field) for field in required_authority):
        raise CalibrationManifestError("approved calibration authority is incomplete")
    policy_text = " ".join(
        str(authority.get(field, "")).lower()
        for field in (
            "dataset_name",
            "dataset_version",
            "dataset_split",
            "sample_selection",
            "preprocessing",
            "trained_checkpoint_identity",
            "approval_identity",
        )
    )
    if any(marker in policy_text for marker in (
        "fixture", "synthetic", "placeholder", "none:"
    )):
        raise CalibrationManifestError(
            "fixture or placeholder evidence cannot be approved"
        )
    for field in (
        "dataset_sha256",
        "preprocessing_sha256",
        "trained_checkpoint_sha256",
    ):
        _require_sha256(authority.get(field), field)
    if authority.get("acceptance_data_role") != "disjoint_held_out":
        raise CalibrationManifestError(
            "calibration and acceptance data roles are not separated"
        )
    if not authority.get("approval_identity"):
        raise CalibrationManifestError("calibration approval identity is missing")


class _ContextStatistics:
    def __init__(self) -> None:
        self.observed_min = math.inf
        self.observed_max = -math.inf
        self.element_count = 0
        self.nonfinite_count = 0
        self.sample_abs_maxima: list[float] = []

    def observe(self, tensor: Any) -> None:
        import torch

        detached = tensor.detach().to(device="cpu", dtype=torch.float64)
        flat = detached.reshape(detached.shape[0], -1)
        finite = torch.isfinite(flat)
        self.nonfinite_count += int((~finite).sum().item())
        if not bool(finite.all().item()):
            raise CalibrationManifestError("nonfinite pre-ReLU value observed")
        self.observed_min = min(self.observed_min, float(flat.min().item()))
        self.observed_max = max(self.observed_max, float(flat.max().item()))
        self.element_count += int(flat.numel())
        self.sample_abs_maxima.extend(
            float(value) for value in flat.abs().amax(dim=1).tolist()
        )

    @staticmethod
    def _quantile(values: Sequence[float], probability: float) -> float:
        ordered = sorted(values)
        index = max(0, math.ceil(probability * len(ordered)) - 1)
        return ordered[index]

    def finish(self, safety_factor: float, minimum_bound: float) -> dict[str, Any]:
        if not self.sample_abs_maxima:
            raise CalibrationManifestError("a ReLU context was not observed")
        observed_abs_max = max(self.sample_abs_maxima)
        bound = max(observed_abs_max * safety_factor, minimum_bound)
        if not math.isfinite(bound) or bound <= 0:
            raise CalibrationManifestError("normalization bound B must be positive")
        return {
            "observed_min": self.observed_min,
            "observed_max": self.observed_max,
            "observed_abs_max": observed_abs_max,
            "per_sample_abs_q99": self._quantile(self.sample_abs_maxima, 0.99),
            "per_sample_abs_q999": self._quantile(self.sample_abs_maxima, 0.999),
            "sample_count": len(self.sample_abs_maxima),
            "element_count": self.element_count,
            "nonfinite_count": self.nonfinite_count,
            "bound_b": bound,
            "outlier_count": sum(
                value > bound for value in self.sample_abs_maxima
            ),
        }


class IdentityBoundReluCalibrationCollector:
    """Collect pre-ReLU statistics keyed by persisted Open64 identities."""

    def __init__(
        self,
        model: Any,
        context_template: Sequence[Mapping[str, Any]],
        *,
        safety_factor: float = 1.0,
        minimum_positive_bound: float = 1.0e-12,
    ) -> None:
        validate_context_template(context_template)
        if not math.isfinite(safety_factor) or safety_factor < 1.0:
            raise CalibrationManifestError("safety factor must be at least one")
        if not math.isfinite(minimum_positive_bound) or \
                minimum_positive_bound <= 0:
            raise CalibrationManifestError("minimum bound must be positive")
        self._model = model
        self._contexts = [copy.deepcopy(dict(item)) for item in context_template]
        self._safety_factor = safety_factor
        self._minimum_bound = minimum_positive_bound

    def collect(
        self,
        samples: Iterable[CalibrationSample],
        *,
        profile_name: str,
        coefficient_manifest_sha256: str,
        calibration_authority: Mapping[str, Any],
        source_artifact: Mapping[str, Any],
        status: str = "candidate_unapproved",
    ) -> dict[str, Any]:
        import torch

        _require_sha256(coefficient_manifest_sha256,
                        "coefficient_manifest_sha256")
        named_modules = dict(self._model.named_modules())
        routes: dict[str, list[dict[str, Any]]] = {}
        statistics: dict[tuple[object, ...], _ContextStatistics] = {}
        for context in self._contexts:
            module_path = context["module_path"]
            if module_path not in named_modules:
                raise CalibrationManifestError(
                    f"ReLU module path does not exist: {module_path}"
                )
            routes.setdefault(module_path, []).append(context)
            statistics[_identity_key(context)] = _ContextStatistics()
        for route in routes.values():
            route.sort(key=lambda item: item["invocation_ordinal"])
            if [item["invocation_ordinal"] for item in route] != \
                    list(range(len(route))):
                raise CalibrationManifestError(
                    "ReLU invocation ordinals must be contiguous per module"
                )

        invocation_counts: dict[str, int] = {}
        handles = []

        def make_hook(module_path: str):
            def hook(_module: Any, inputs: tuple[Any, ...]) -> None:
                ordinal = invocation_counts.get(module_path, 0)
                candidates = routes[module_path]
                if ordinal >= len(candidates):
                    raise CalibrationManifestError(
                        f"unexpected ReLU invocation for {module_path}"
                    )
                if len(inputs) != 1 or not torch.is_tensor(inputs[0]):
                    raise CalibrationManifestError(
                        f"ReLU input is not one tensor for {module_path}"
                    )
                if inputs[0].ndim == 0 or inputs[0].shape[0] != 1:
                    raise CalibrationManifestError(
                        "v1 calibration requires one sample per model invocation"
                    )
                context = candidates[ordinal]
                statistics[_identity_key(context)].observe(inputs[0])
                invocation_counts[module_path] = ordinal + 1
            return hook

        for module_path in sorted(routes):
            handles.append(
                named_modules[module_path].register_forward_pre_hook(
                    make_hook(module_path)
                )
            )

        sample_ids: list[str] = []
        input_digest = hashlib.sha256()
        try:
            self._model.eval()
            with torch.no_grad():
                for sample in samples:
                    if not isinstance(sample.sample_id, str) or not sample.sample_id:
                        raise CalibrationManifestError("sample ID is empty")
                    if sample.sample_id in sample_ids:
                        raise CalibrationManifestError("sample IDs are not unique")
                    input_digest.update(sample.sample_id.encode("utf-8"))
                    input_digest.update(b"\0")
                    for input_ordinal, value in enumerate(sample.inputs):
                        if not torch.is_tensor(value):
                            raise CalibrationManifestError(
                                "v1 calibration inputs must be tensors"
                            )
                        _update_tensor_digest(
                            input_digest, f"input{input_ordinal}", value
                        )
                    invocation_counts.clear()
                    self._model(*sample.inputs)
                    for module_path, expected in routes.items():
                        if invocation_counts.get(module_path, 0) != len(expected):
                            raise CalibrationManifestError(
                                f"missing ReLU invocation for {module_path}"
                            )
                    sample_ids.append(sample.sample_id)
        finally:
            for handle in handles:
                handle.remove()
        if not sample_ids:
            raise CalibrationManifestError("calibration sample set is empty")

        contexts = []
        for template in self._contexts:
            context = copy.deepcopy(template)
            context.update(
                statistics[_identity_key(template)].finish(
                    self._safety_factor, self._minimum_bound
                )
            )
            contexts.append(context)
        authority = copy.deepcopy(dict(calibration_authority))
        authority.setdefault("out_of_range_policy", "reject")
        authority.setdefault(
            "bound_rule",
            f"max_abs_times_safety_factor:{self._safety_factor:.17g}",
        )
        authority.setdefault("collector_revision", "open64-sync3-relu-calibration-v1")
        authority.setdefault(
            "environment",
            {
                "python": platform.python_version(),
                "torch": torch.__version__,
                "platform": platform.platform(),
            },
        )
        manifest = {
            "schema": SCHEMA,
            "status": status,
            "profile_name": profile_name,
            "coefficient_manifest_sha256": coefficient_manifest_sha256,
            "source_artifact": copy.deepcopy(dict(source_artifact)),
            "calibration_authority": authority,
            "identity_key": [
                "owner_pu_st",
                "source_relu_value_id",
                "context_pu_identity_id",
                "context_callsite_id",
            ],
            "collection": {
                "sample_count": len(sample_ids),
                "sample_ids": sample_ids,
                "sample_order_sha256": hashlib.sha256(
                    canonical_json_bytes({"sample_ids": sample_ids})
                ).hexdigest(),
                "observed_input_sha256": input_digest.hexdigest(),
                "observed_model_state_sha256": model_state_sha256(self._model),
                "seed": authority.get("sample_seed"),
            },
            "contexts": contexts,
        }
        result = finalize_manifest(manifest)
        validate_calibration_manifest(result, expected_contexts=self._contexts)
        return result
