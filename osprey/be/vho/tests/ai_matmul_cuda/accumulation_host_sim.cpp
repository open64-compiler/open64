/* Copyright (C) 2026 Open64 Project */

/* Execute only WHIRL-derived scalar reduction code on the host. This proves
 * reset/merge semantics and numerical test cases, not GPU FP8 arithmetic.
 * Design: doc/AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md. */
#include <cmath>
#include <cstdio>
#include "accumulation.whirl.cu"

/* Run exact, varying-sign and absorption-sensitive inputs against an FP64
 * reference. Return failure for unexpected source/compiler behavior. */
int main()
{
    float a[257], b[257], result;
    for (int test = 0; test != 3; ++test) {
        double reference = 0;
        for (int k = 0; k != 257; ++k) {
            a[k] = test == 0 ? 1.0f : test == 1 ? (k % 13 - 6) * 0.125f :
                k == 0 ? 16777216.0f : 1.0f;
            b[k] = test == 1 ? (k % 7 - 3) * 0.25f : 1.0f;
            reference += double(a[k]) * double(b[k]);
        }
        accumulation_case(a, b, &result);
        double error = std::fabs(double(result) - reference);
        if (!std::isfinite(result) || (test < 2 && error != 0))
            return 1;
        std::printf("case=%d result=%.17g reference=%.17g abs_error=%.17g\n",
            test, double(result), reference, error);
    }
    return 0;
}
