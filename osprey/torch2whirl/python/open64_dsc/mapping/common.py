"""Common substrate operator names planned for Python graph ingestion."""

ADD = "common.add"
FLATTEN = "common.flatten"
LINEAR = "common.linear"
MATMUL = "common.matmul"
REDUCE_SUM = "common.reduce_sum"
STABLE_SOFTMAX = "common.stable_softmax"
MEAN_VARIANCE_NORM = "common.mean_variance_norm"
MODEL_INPUT = "common.model_input"
OUTPUT_LOGITS = "common.output_logits"
RELU = "common.relu"
RESIDUAL_ADD = "common.residual_add"
RESHAPE = "common.reshape"
TENSOR_CONST = "common.tensor_const"
TRANSPOSE = "common.transpose"

UNARY_OPERATORS = {
    REDUCE_SUM,
    STABLE_SOFTMAX,
    FLATTEN,
    OUTPUT_LOGITS,
    RELU,
}

TERNARY_OPERATORS = {
    MEAN_VARIANCE_NORM,
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
