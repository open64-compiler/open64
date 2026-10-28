/* Execute the emitted typed source or real backend-compiled object against
 * independent IEEE widening and strict FP32 arithmetic. Not GPU evidence.
 * Design: doc/AI-COMPILER-MMA3-TYPED-SIMT-BASELINE.md. */
#include <cuda_runtime.h>
#include <algorithm>
#include <cfenv>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <vector>

dim3 threadIdx = {0, 0, 0}, blockIdx = {0, 0, 0};
extern "C" void typed_simt_case(unsigned short *, unsigned short *, float *);
static bool capture_bits = false;
static std::vector<unsigned> captured;

/* Record the real generated bitcast input before any arithmetic can quiet a
 * NaN or change a signed zero. memcpy implements the CUDA builtin exactly. */
extern "C" float __uint_as_float(unsigned value)
{
    if (capture_bits) captured.push_back(value);
    float result;
    std::memcpy(&result, &value, sizeof(result));
    return result;
}

/* Preserve exact FP32 bits, including zero sign; no numeric cast is used. */
static unsigned VHO_DSL_test_bits(float value)
{
    unsigned result;
    std::memcpy(&result, &value, sizeof(result));
    return result;
}

/* Independent mathematical decode, not the compiler's integer normalization
 * recurrence. Special encodings additionally specify the exact widened bits. */
static float VHO_DSL_test_decode(unsigned short raw, unsigned format)
{
    const unsigned mantissa_bits = format == 5 ? 10 : 7;
    const unsigned exponent_mask = format == 5 ? 31 : 255;
    const int bias = format == 5 ? 15 : 127;
    const unsigned sign = raw >> 15;
    const unsigned exponent = (raw >> mantissa_bits) & exponent_mask;
    const unsigned fraction = raw & ((1u << mantissa_bits) - 1);
    if (exponent == exponent_mask) {
        unsigned bits = (sign << 31) | 0x7f800000u | (fraction << (23 - mantissa_bits));
        float value; std::memcpy(&value, &bits, sizeof(value)); return value;
    }
    double significand = exponent ? 1.0 + std::ldexp(static_cast<double>(fraction), -static_cast<int>(mantissa_bits)) :
        std::ldexp(static_cast<double>(fraction), -static_cast<int>(mantissa_bits));
    double magnitude = std::ldexp(significand, static_cast<int>(exponent ? exponent : 1) - bias);
    return static_cast<float>(sign ? -magnitude : magnitude);
}

/* Source-order, separately rounded FP32 multiplication and accumulation. */
static float VHO_DSL_test_dot(const std::vector<unsigned short> &input_a,
    const std::vector<unsigned short> &input_b, unsigned row, unsigned column,
    unsigned width, unsigned reduction, unsigned format)
{
    volatile float accumulator = 0.0f;
    for (unsigned index = 0; index < reduction; ++index) {
        volatile float product = VHO_DSL_test_decode(input_a[row * reduction + index], format) *
            VHO_DSL_test_decode(input_b[index * width + column], format);
        accumulator = accumulator + product;
    }
    return accumulator;
}

/* Execute one serial source entry or all launched SIMT outputs, including
 * excess blocks and hostile unsigned coordinates to audit address guards. */
static void VHO_DSL_test_launch(std::vector<unsigned short> &input_a, std::vector<unsigned short> &input_b,
    float *output, unsigned rows, unsigned columns, unsigned threads, bool serial)
{
    if (serial) {
        typed_simt_case(input_a.data(), input_b.data(), output);
        return;
    }
    for (unsigned row = 0; row <= rows; ++row)
        for (unsigned block = 0; block <= (columns + threads - 1) / threads; ++block)
            for (unsigned lane = 0; lane < threads; ++lane) {
                threadIdx = {lane, 0, 0}; blockIdx = {block, row, 0};
                typed_simt_case(input_a.data(), input_b.data(), output);
            }
    threadIdx = {0xffffffffu, 0, 0}; blockIdx = {0xffffffffu, 0xffffffffu, 0};
    typed_simt_case(input_a.data(), input_b.data(), output);
    threadIdx = {0, 0, 0}; blockIdx = {0xffffffffu, 0, 0};
    typed_simt_case(input_a.data(), input_b.data(), output);
}

/* Structured raw source data never passes through the compiler's packer or
 * a float-to-half shim, so widening and accumulation are tested separately. */
static void VHO_DSL_test_inputs(std::vector<unsigned short> &input_a,
    std::vector<unsigned short> &input_b, unsigned rows, unsigned columns, unsigned reduction,
    unsigned format, unsigned pattern)
{
    const unsigned short one = format == 5 ? 0x3c00 : 0x3f80;
    for (unsigned index = 0; index < input_a.size(); ++index)
        input_a[index] = one + ((index * 13) % 32) + (index % 3 == 0 ? 0x8000 : 0);
    for (unsigned index = 0; index < input_b.size(); ++index)
        input_b[index] = one + ((index * 7) % 32) + (index % 5 == 0 ? 0x8000 : 0);
    if (!pattern) return;
    std::fill(input_b.begin(), input_b.end(), one);
    unsigned short value = 0;
    if (pattern == 3) value = 1;
    if (pattern == 4) value = 0x8001;
    if (pattern == 5) value = 0x8000;
    if (pattern == 6) value = format == 5 ? 0x7c00 : 0x7f80;
    if (pattern == 7) value = format == 5 ? 0xfc00 : 0xff80;
    if (pattern == 8) value = format == 5 ? 0x7e31 : 0x7fc5;
    if (pattern == 9) value = format == 5 ? 0x7c01 : 0x7f81;
    if (pattern == 10) value = format == 5 ? 0x7bff : 0x7f7f;
    std::fill(input_a.begin(), input_a.end(), value);
    if (pattern == 10) std::fill(input_b.begin(), input_b.end(), value);
    if (pattern == 1 || pattern == 2) {
        const unsigned short large = format == 5 ? 0x7bff : 0x4b80;
        const unsigned short small = format == 5 ? 0x1000 : 0x3b80;
        for (unsigned row = 0; row < rows; ++row)
            for (unsigned index = 0; index < reduction; ++index)
                input_a[row * reduction + index] = pattern == 1 ?
                    (index % 3 == 0 ? large : index % 3 == 1 ? one : large | 0x8000) :
                    (index ? small : one);
    }
    if (pattern == 11 && format == 6 && reduction >= 2) {
        for (unsigned row = 0; row < rows; ++row) {
            input_a[row * reduction] = 0x1a80;
            input_a[row * reduction + 1] = 0x1a00;
        }
        std::fill(input_b.begin(), input_b.end(), 0x1a00);
    }
}

/* Compare the executed kernel with strict FP32 references, immutable inputs,
 * guarded output canaries and exhaustive 16-bit widening when K=1,N=65536. */
int main(int argc, char **argv)
{
    if (argc != 8) return 2;
    const unsigned format = std::strtoul(argv[1], NULL, 10), rows = std::strtoul(argv[2], NULL, 10);
    const unsigned columns = std::strtoul(argv[3], NULL, 10), reduction = std::strtoul(argv[4], NULL, 10);
    const unsigned threads = std::strtoul(argv[5], NULL, 10);
    const bool serial = std::atoi(argv[6]) != 0, instrumented = std::atoi(argv[7]) != 0;
    if (std::fesetround(FE_TONEAREST)) return 3;
    std::vector<unsigned short> input_a(rows * reduction), input_b(reduction * columns);
    unsigned canary_bits = 0x4a123456;
    float canary; std::memcpy(&canary, &canary_bits, sizeof(canary));
    std::vector<float> output(rows * columns + 16, canary);
    const bool exhaustive = rows == 1 && columns == 65536 && reduction == 1;
    for (unsigned pattern = 0; pattern < (exhaustive ? 1u : 12u); ++pattern) {
        VHO_DSL_test_inputs(input_a, input_b, rows, columns, reduction, format, pattern);
        if (exhaustive) {
            input_a[0] = format == 5 ? 0x3c00 : 0x3f80;
            for (unsigned value = 0; value < 65536; ++value) input_b[value] = value;
        }
        const std::vector<unsigned short> saved_a = input_a, saved_b = input_b;
        std::fill(output.begin(), output.end(), canary);
        captured.clear(); capture_bits = exhaustive && instrumented;
        VHO_DSL_test_launch(input_a, input_b, output.data() + 8, rows, columns, threads, serial);
        capture_bits = false;
        if (input_a != saved_a || input_b != saved_b) return 4;
        for (unsigned index = 0; index < 8; ++index)
            if (VHO_DSL_test_bits(output[index]) != canary_bits ||
                VHO_DSL_test_bits(output[rows * columns + 8 + index]) != canary_bits) return 5;
        for (unsigned row = 0; row < rows; ++row)
            for (unsigned column = 0; column < columns; ++column) {
                float expected = VHO_DSL_test_dot(input_a, input_b, row, column, columns, reduction, format);
                float actual = output[row * columns + column + 8];
                if (!(std::isnan(expected) ? std::isnan(actual) : VHO_DSL_test_bits(expected) == VHO_DSL_test_bits(actual))) {
                    std::fprintf(stderr, "mismatch pattern=%u row=%u col=%u actual=%08x expected=%08x\n",
                        pattern, row, column, VHO_DSL_test_bits(actual), VHO_DSL_test_bits(expected)); return 6;
                }
            }
        if (exhaustive && instrumented) {
            if (captured.size() != 131072) return 7;
            for (unsigned value = 0; value < 65536; ++value)
                if (captured[value * 2] != 0x3f800000 ||
                    captured[value * 2 + 1] != VHO_DSL_test_bits(VHO_DSL_test_decode(value, format))) return 8;
        }
        if (pattern == 11 && format == 6 && reduction >= 2) {
            if (VHO_DSL_test_bits(output[8]) != 1) return 9;
            float fused = std::fma(VHO_DSL_test_decode(0x1a00, 6), VHO_DSL_test_decode(0x1a00, 6),
                VHO_DSL_test_decode(0x1a80, 6) * VHO_DSL_test_decode(0x1a00, 6));
            if (VHO_DSL_test_bits(fused) != 2) return 10;
        }
    }
    std::printf("Typed SIMT host passed: format=%u M=%u N=%u K=%u mode=%s patterns=%u widening=%u source_unchanged=true FP32=true native_gpu=false\n",
        format, rows, columns, reduction, serial ? "serial" : "simt", exhaustive ? 1 : 12, exhaustive && instrumented ? 65536 : 0);
    return 0;
}
