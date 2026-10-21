/* Minimal host runtime for the direct F32 m0 ABI, not a production provider. */

#include "runtime.h"
#include "open64_dsl_runtime_abi.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint32_t rows;
    uint32_t cols;
    float *values;
} M0_TENSOR;

static M0_TENSOR *owned[3];
static uint32_t owned_count;
static M0_TENSOR *last_result;
static const char *error_message;

static int
shape_of(const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *desc,
         uint32_t *rows, uint32_t *cols)
{
    int64_t shape[2];
    if (desc == NULL || desc->abi_version != OPEN64_DSL_RUNTIME_ABI_VERSION ||
        desc->header_size != OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE ||
        desc->total_size < sizeof(*desc) ||
        desc->dtype != OPEN64_DSL_DTYPE_F32 || desc->rank != 2 ||
        desc->shape_count != 2 ||
        desc->shape_offset < sizeof(*desc) ||
        desc->shape_offset > desc->total_size - sizeof(shape) ||
        desc->stride_count != 0 || desc->stride_offset != 0) {
        error_message = "unsupported F32 rank-2 descriptor";
        return 0;
    }
    memcpy(shape, (const char *)desc + desc->shape_offset, sizeof(shape));
    if (shape[0] <= 0 || shape[1] <= 0 ||
        shape[0] > UINT32_MAX || shape[1] > UINT32_MAX ||
        (uint64_t)shape[0] * (uint64_t)shape[1] > SIZE_MAX / sizeof(float)) {
        error_message = "invalid matrix extent";
        return 0;
    }
    *rows = (uint32_t)shape[0];
    *cols = (uint32_t)shape[1];
    return 1;
}

static M0_TENSOR *
new_tensor(uint32_t rows, uint32_t cols)
{
    M0_TENSOR *tensor;
    if (owned_count == 3) {
        error_message = "unexpected extra tensor allocation";
        return NULL;
    }
    tensor = (M0_TENSOR *)calloc(1, sizeof(*tensor));
    if (tensor == NULL) {
        error_message = "tensor allocation failed";
        return NULL;
    }
    tensor->values = (float *)calloc((size_t)rows * cols, sizeof(float));
    if (tensor->values == NULL) {
        free(tensor);
        error_message = "tensor data allocation failed";
        return NULL;
    }
    tensor->rows = rows;
    tensor->cols = cols;
    owned[owned_count++] = tensor;
    return tensor;
}

OPEN64_DSL_TENSOR_HANDLE
__open64_dsl_tensor_const_v1(const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                             const OPEN64_DSL_SCALAR_VALUE_V1 *value)
{
    uint32_t rows, cols, bits;
    size_t i;
    float scalar;
    M0_TENSOR *tensor;
    if (!shape_of(result, &rows, &cols))
        return NULL;
    if (value == NULL || value->kind != OPEN64_DSL_SCALAR_FLOAT_BITS ||
        value->bit_width != 32 || value->bits_hi != 0 ||
        (value->bits_lo >> 32) != 0) {
        error_message = "unsupported F32 scalar";
        return NULL;
    }
    bits = (uint32_t)value->bits_lo;
    memcpy(&scalar, &bits, sizeof(scalar));
    tensor = new_tensor(rows, cols);
    if (tensor == NULL)
        return NULL;
    for (i = 0; i < (size_t)rows * cols; ++i)
        tensor->values[i] = scalar;
    return tensor;
}

OPEN64_DSL_TENSOR_HANDLE
__open64_dsl_matmul_v1(OPEN64_DSL_TENSOR_HANDLE kid0,
                       OPEN64_DSL_TENSOR_HANDLE kid1,
                       const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                       uint32_t flags)
{
    M0_TENSOR *a = (M0_TENSOR *)kid0;
    M0_TENSOR *b = (M0_TENSOR *)kid1;
    M0_TENSOR *c;
    uint32_t rows, cols, i, j, k;
    if (!shape_of(result, &rows, &cols))
        return NULL;
    if (flags != OPEN64_DSL_MATMUL_FLAG_NONE || a == NULL || b == NULL ||
        a->rows != rows || b->cols != cols || a->cols != b->rows) {
        error_message = "matmul shape or flags mismatch";
        return NULL;
    }
    c = new_tensor(rows, cols);
    if (c == NULL)
        return NULL;
    for (i = 0; i < rows; ++i)
        for (j = 0; j < cols; ++j) {
            float sum = 0.0f;
            for (k = 0; k < a->cols; ++k)
                sum += a->values[(size_t)i * a->cols + k] *
                       b->values[(size_t)k * cols + j];
            c->values[(size_t)i * cols + j] = sum;
        }
    last_result = c;
    return c;
}

int
ai_matmul_m0_validate(uint32_t rows, uint32_t cols, float expected)
{
    size_t i;
    if (error_message != NULL || owned_count != 3 || last_result == NULL ||
        last_result->rows != rows || last_result->cols != cols)
        return 0;
    for (i = 0; i < (size_t)rows * cols; ++i)
        if (last_result->values[i] != expected) {
            error_message = "numerical mismatch";
            return 0;
        }
    return 1;
}

const char *
ai_matmul_m0_error(void)
{
    return error_message == NULL ? "missing or incomplete result" : error_message;
}

void
ai_matmul_m0_reset(void)
{
    uint32_t i;
    for (i = 0; i < owned_count; ++i) {
        free(owned[i]->values);
        free(owned[i]);
        owned[i] = NULL;
    }
    owned_count = 0;
    last_result = NULL;
    error_message = NULL;
}
