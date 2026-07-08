"""Common substrate operator names planned for Python graph ingestion."""

ADD = "common.add"
FLATTEN = "common.flatten"
LINEAR = "common.linear"
MATMUL = "common.matmul"
OUTPUT_LOGITS = "common.output_logits"
RELU = "common.relu"
RESIDUAL_ADD = "common.residual_add"

UNARY_OPERATORS = {
    FLATTEN,
    OUTPUT_LOGITS,
    RELU,
}

FX_OPERATOR_MAP = {
    "add": ADD,
    "flatten": FLATTEN,
    "matmul": MATMUL,
    "residual_add": RESIDUAL_ADD,
    "relu": RELU,
}
