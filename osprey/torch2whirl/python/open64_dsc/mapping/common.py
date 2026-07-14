"""Common substrate operator names planned for Python graph ingestion."""

ADD = "common.add"
FLATTEN = "common.flatten"
LINEAR = "common.linear"
MATMUL = "common.matmul"
MODEL_INPUT = "common.model_input"
OUTPUT_LOGITS = "common.output_logits"
RELU = "common.relu"
RESIDUAL_ADD = "common.residual_add"
TENSOR_CONST = "common.tensor_const"

UNARY_OPERATORS = {
    FLATTEN,
    OUTPUT_LOGITS,
    RELU,
}

TERNARY_OPERATORS = {
    LINEAR,
}

FX_OPERATOR_MAP = {
    "add": ADD,
    "flatten": FLATTEN,
    "linear": LINEAR,
    "matmul": MATMUL,
    "residual_add": RESIDUAL_ADD,
    "relu": RELU,
}
