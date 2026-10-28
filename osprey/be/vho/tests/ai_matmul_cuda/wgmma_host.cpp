/* Execute actual emitted addressing/packing/guards with a test-only completed
 * atom substitution. Arithmetic/protocol emulation is not GPU validation.
 * Design: doc/AI-COMPILER-MMA4-H-HOPPER-LOWERING.md. */
#include <cuda_runtime.h>
#include <algorithm>
#include <atomic>
#include <cmath>
#include <condition_variable>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <stdexcept>
#include <thread>
#include <vector>

extern "C" void typed_simt_case(unsigned short *, unsigned short *, float *);
thread_local dim3 threadIdx = {0, 0, 0};
dim3 blockIdx = {0, 0, 0}, blockDim = {128, 1, 1}, gridDim = {1, 1, 1};
static std::mutex VHO_DSL_wgmma_mutex;
static std::condition_variable VHO_DSL_wgmma_condition;
static unsigned VHO_DSL_wgmma_arrived = 0, VHO_DSL_wgmma_generation = 0, VHO_DSL_wgmma_format = 5;
static thread_local unsigned VHO_DSL_wgmma_phase = 0;
static std::atomic<unsigned> VHO_DSL_wgmma_atoms(0), VHO_DSL_wgmma_proxies(0);

/* Decode mathematically rather than using the compiler's bit manipulations. */
static float VHO_DSL_wgmma_decode(unsigned short raw)
{
    unsigned mantissa = VHO_DSL_wgmma_format == 5 ? 10 : 7;
    unsigned mask = VHO_DSL_wgmma_format == 5 ? 31 : 255;
    int bias = VHO_DSL_wgmma_format == 5 ? 15 : 127;
    unsigned exponent = (raw >> mantissa) & mask, fraction = raw & ((1u << mantissa) - 1);
    double significand = std::ldexp(double(fraction), -int(mantissa)) + (exponent ? 1.0 : 0.0);
    return float(std::ldexp(significand, int(exponent ? exponent : 1) - bias) * (raw & 0x8000 ? -1.0 : 1.0));
}

/* Verify each thread publishes before the CTA join; no data readiness is
 * inferred from host scheduling or from the later completion join. */
void VHO_DSL_wgmma_host_proxy()
{
    if (VHO_DSL_wgmma_phase != 0) throw std::runtime_error("proxy before prior reuse join");
    VHO_DSL_wgmma_phase = 1; ++VHO_DSL_wgmma_proxies;
}

/* A reusable 128-thread CTA barrier enforces both publication and release.
 * Every participant follows the same two distinct protocol phases. */
void VHO_DSL_wgmma_host_barrier()
{
    if (VHO_DSL_wgmma_phase != 1 && VHO_DSL_wgmma_phase != 3) throw std::runtime_error("wrong CTA barrier phase");
    VHO_DSL_wgmma_phase = VHO_DSL_wgmma_phase == 1 ? 2 : 0;
    std::unique_lock<std::mutex> lock(VHO_DSL_wgmma_mutex);
    unsigned generation = VHO_DSL_wgmma_generation;
    if (++VHO_DSL_wgmma_arrived == 128) {
        VHO_DSL_wgmma_arrived = 0; ++VHO_DSL_wgmma_generation; VHO_DSL_wgmma_condition.notify_all();
    } else VHO_DSL_wgmma_condition.wait(lock, [generation] { return generation != VHO_DSL_wgmma_generation; });
}

/* Model the completed atom's mathematical effect only. Independent scalar
 * reference checks the dense original buffers; this checks actual repacking
 * and register-to-output addressing, not ISA order/subnormal behavior. */
void VHO_DSL_wgmma_host_atom(float *fragment, unsigned short *input_a, unsigned short *input_b)
{
    if (VHO_DSL_wgmma_phase != 2) throw std::runtime_error("MMA before operand publication");
    const unsigned lane = threadIdx.x % 32, warp = threadIdx.x / 32;
    for (unsigned reg = 0; reg < 32; ++reg) {
        unsigned row = warp * 16 + lane / 4 + ((reg >> 1) & 1) * 8;
        unsigned column = (reg >> 2) * 8 + (lane & 3) * 2 + (reg & 1);
        for (unsigned reduction = 0; reduction < 16; ++reduction) {
            unsigned offset_a = (row >> 3) * 128 + (reduction >> 3) * 64 + (row & 7) * 8 + (reduction & 7);
            unsigned offset_b = (column >> 3) * 128 + (reduction >> 3) * 64 + (column & 7) * 8 + (reduction & 7);
            fragment[reg] += VHO_DSL_wgmma_decode(input_a[offset_a]) * VHO_DSL_wgmma_decode(input_b[offset_b]);
        }
    }
    VHO_DSL_wgmma_phase = 3; ++VHO_DSL_wgmma_atoms;
}

/* Launch one CTA at a time so the emitted static host-shared arrays retain
 * CTA-local ownership. Actual GPU launch/resource behavior is not modeled. */
static void VHO_DSL_wgmma_launch(unsigned short *input_a, unsigned short *input_b, float *output, bool simt)
{
    if (simt) {
        for (unsigned lane = 0; lane < 128; ++lane) { threadIdx = {lane, 0, 0}; typed_simt_case(input_a, input_b, output); }
    } else {
        std::vector<std::thread> workers;
        for (unsigned lane = 0; lane < 128; ++lane) workers.emplace_back([=] {
            threadIdx = {lane, 0, 0}; typed_simt_case(input_a, input_b, output);
            if (VHO_DSL_wgmma_phase) std::abort();
        });
        for (std::thread &worker : workers) worker.join();
    }
}

/* Compare all actual stores with an independent dense original-buffer sum,
 * exercise extra blocks/wrong launch guards and preserve source/canary bits. */
int main(int argc, char **argv)
{
    if (argc != 6) return 2;
    VHO_DSL_wgmma_format = std::strtoul(argv[1], NULL, 10);
    const unsigned rows = std::strtoul(argv[2], NULL, 10), columns = std::strtoul(argv[3], NULL, 10), reduction = std::strtoul(argv[4], NULL, 10);
    const bool simt = std::atoi(argv[5]) != 0;
    std::vector<unsigned short> input_a(rows * reduction), input_b(reduction * columns);
    std::vector<float> output(rows * columns + 16, 123456.0f);
    unsigned short one = VHO_DSL_wgmma_format == 5 ? 0x3c00 : 0x3f80;
    unsigned shift = VHO_DSL_wgmma_format == 5 ? 5 : 2;
    for (unsigned pattern = 0; pattern < 4; ++pattern) {
        for (unsigned index = 0; index < input_a.size(); ++index) input_a[index] = pattern ?
            one + ((index * 13 + index / reduction) % 29) * (1u << shift) + (index % 3 == 0 ? 0x8000 : 0) : one;
        for (unsigned index = 0; index < input_b.size(); ++index) input_b[index] = pattern ?
            one + ((index * 7 + index / columns) % 29) * (1u << shift) + (index % 5 == 0 ? 0x8000 : 0) : one;
        if (pattern == 2) std::fill(input_a.begin(), input_a.end(), 0);
        if (pattern == 3) std::fill(input_b.begin(), input_b.end(), one | 0x8000);
        const std::vector<unsigned short> saved_a = input_a, saved_b = input_b;
        std::fill(output.begin(), output.end(), 123456.0f);
        gridDim = {simt ? (columns + 127) / 128 : columns / 64, simt ? rows : rows / 64, 1};
        blockDim = {128, 1, 1};
        for (unsigned row = 0; row <= gridDim.y; ++row)
            for (unsigned column = 0; column <= gridDim.x; ++column) {
                blockIdx = {column, row, 0}; VHO_DSL_wgmma_launch(input_a.data(), input_b.data(), output.data() + 8, simt);
            }
        if (saved_a != input_a || saved_b != input_b) return 3;
        for (unsigned index = 0; index < 8; ++index)
            if (output[index] != 123456.0f || output[rows * columns + index + 8] != 123456.0f) return 4;
        for (unsigned row = 0; row < rows; ++row)
            for (unsigned column = 0; column < columns; ++column) {
                double expected = 0;
                for (unsigned index = 0; index < reduction; ++index)
                    expected += double(VHO_DSL_wgmma_decode(input_a[row * reduction + index])) * VHO_DSL_wgmma_decode(input_b[index * columns + column]);
                if (output[row * columns + column + 8] != float(expected)) return 5;
            }
    }
    if (!simt) {
        unsigned expected = 4 * (rows / 64) * (columns / 64) * (reduction / 16) * 128;
        if (VHO_DSL_wgmma_atoms != expected || VHO_DSL_wgmma_proxies != expected) return 6;
        std::fill(output.begin(), output.end(), 123456.0f);
        const dim3 invalid[] = {{64, 1, 1}, {256, 1, 1}, {128, 2, 1}, {128, 1, 2}};
        blockIdx = {0, 0, 0};
        for (dim3 shape : invalid) { blockDim = shape; VHO_DSL_wgmma_launch(input_a.data(), input_b.data(), output.data() + 8, false); }
        blockDim = {128, 1, 1}; gridDim.z = 2;
        VHO_DSL_wgmma_launch(input_a.data(), input_b.data(), output.data() + 8, false);
        gridDim.z = 1; blockIdx = {0xffffffffu, 0xffffffffu, 0};
        VHO_DSL_wgmma_launch(input_a.data(), input_b.data(), output.data() + 8, false);
        if (VHO_DSL_wgmma_atoms != expected || VHO_DSL_wgmma_proxies != expected ||
            std::count(output.begin(), output.end(), 123456.0f) != output.size()) return 7;
    }
    std::printf("MMA4-H host geometry/protocol passed: M=%u N=%u K=%u format=%u mode=%s patterns=4 native_gpu=unknown timing=unknown\n",
        rows, columns, reduction, VHO_DSL_wgmma_format, simt ? "SIMT" : "completed_atom_emulation");
    return 0;
}
