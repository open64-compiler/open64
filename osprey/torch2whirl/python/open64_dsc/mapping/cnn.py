"""CNN operator names planned for the first torch2whirl vertical slice."""

CONV2D = "cnn.conv2d"
BATCH_NORM_INFER = "cnn.batch_norm_infer"
MAX_POOL2D = "cnn.max_pool2d"
GLOBAL_AVG_POOL2D = "cnn.global_avg_pool2d"

UNARY_OPERATORS = {
    GLOBAL_AVG_POOL2D,
    MAX_POOL2D,
}

FX_OPERATOR_MAP = {
    "adaptive_avg_pool2d": GLOBAL_AVG_POOL2D,
    "max_pool2d": MAX_POOL2D,
}
