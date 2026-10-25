/* Minimal host runtime for the direct F32 m0 ABI, not a production provider. */

#include "runtime.h"
#include "open64_dsl_runtime_abi.h"
#ifdef AI_MATMUL_M2_RUNTIME
#include "generated_runtime.h"
#endif

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
static int last_generated_path;

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

static void
cpu_matmul_into(const M0_TENSOR *a, const M0_TENSOR *b, M0_TENSOR *c)
{
    uint32_t i, j, k;
    for (i = 0; i < c->rows; ++i)
        for (j = 0; j < c->cols; ++j) {
            float sum = 0.0f;
            for (k = 0; k < a->cols; ++k)
                sum += a->values[(size_t)i * a->cols + k] *
                       b->values[(size_t)k * c->cols + j];
            c->values[(size_t)i * c->cols + j] = sum;
        }
}

#ifdef AI_MATMUL_M2_RUNTIME
static void
discard_last_tensor(M0_TENSOR *tensor)
{
    if (owned_count != 0 && owned[owned_count - 1] == tensor) {
        owned[--owned_count] = NULL;
        free(tensor->values);
        free(tensor);
    }
}
#endif

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
    uint32_t rows, cols;
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
    cpu_matmul_into(a, b, c);
    last_result = c;
    return c;
}

#ifdef AI_MATMUL_M2_RUNTIME
static int
valid_generated_plan(const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 *plan)
{
    uint64_t shared, padded_shared, threads;
    if (plan == NULL ||
        plan->abi_version != OPEN64_DSL_RUNTIME_ABI_VERSION ||
        plan->header_size != OPEN64_DSL_GENERATED_MATMUL_PLAN_V1_SIZE ||
        plan->total_size != OPEN64_DSL_GENERATED_MATMUL_PLAN_V1_SIZE ||
        plan->provider != OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED ||
        plan->target_profile != OPEN64_DSL_TARGET_PROFILE_NVIDIA_HOPPER ||
        plan->optimization_plan_id == 0 || plan->tile_plan_id == 0 ||
        plan->cta_m == 0 || plan->cta_n == 0 || plan->cta_k == 0 ||
        plan->thread_m == 0 || plan->thread_n == 0 ||
        plan->cta_m % plan->thread_m || plan->cta_n % plan->thread_n ||
        (plan->fallback_provider != OPEN64_DSL_PHYSICAL_PROVIDER_UNSPECIFIED &&
         plan->fallback_provider != OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT) ||
        plan->reserved != 0)
        return 0;
    shared = ((uint64_t)plan->cta_m * plan->cta_k +
              (uint64_t)plan->cta_k * plan->cta_n) * sizeof(float);
    padded_shared = shared + (uint64_t)plan->cta_k * sizeof(float);
    threads = (uint64_t)(plan->cta_m / plan->thread_m) *
              (plan->cta_n / plan->thread_n);
    return (shared == plan->shared_bytes_per_stage ||
            (plan->cta_k >= 32 &&
             padded_shared == plan->shared_bytes_per_stage)) &&
           threads == plan->threads_per_cta && threads <= 1024;
}

OPEN64_DSL_TENSOR_HANDLE
__open64_dsl_matmul_generated_v1
        (OPEN64_DSL_TENSOR_HANDLE kid0, OPEN64_DSL_TENSOR_HANDLE kid1,
         const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result, uint32_t flags,
         const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 *plan)
{
    M0_TENSOR *a = (M0_TENSOR *)kid0;
    M0_TENSOR *b = (M0_TENSOR *)kid1;
    M0_TENSOR *c;
    uint32_t rows, cols;
    last_generated_path = 0;
    if (!valid_generated_plan(plan) || !shape_of(result, &rows, &cols) ||
        flags != OPEN64_DSL_MATMUL_FLAG_NONE || a == NULL || b == NULL ||
        a->rows != rows || b->cols != cols || a->cols != b->rows) {
        error_message = "generated matmul plan, shape or flags mismatch";
        return NULL;
    }
    c = new_tensor(rows, cols);
    if (c == NULL)
        return NULL;
    if (ai_matmul_m2_launch(a->values, b->values, c->values,
                            rows, cols, a->cols, plan) == 0) {
        last_generated_path = 1;
    } else if (plan->fallback_provider ==
               OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT) {
        cpu_matmul_into(a, b, c);
        last_generated_path = 2;
    } else {
        discard_last_tensor(c);
        error_message = "generated CUDA launch failed without fallback";
        return NULL;
    }
    last_result = c;
    return c;
}
#endif

int
ai_matmul_m0_last_generated_path(void)
{
    return last_generated_path;
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
    last_generated_path = 0;
}
