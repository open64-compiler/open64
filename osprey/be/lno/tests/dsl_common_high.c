/* Copyright (C) 2026 Open64 Project */
/* Ordinary C fixtures for canonical High admission and unchanged fallback.
 * Design: doc/AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md.
 * No metadata or naming convention supplies legality to the LNO checker. */

/* A complete affine signed domain and explicit array memory can be admitted. */
void high_affine(float * restrict out, const float * restrict in)
{
    int i;
    for (i = 0; i < 37; ++i) out[i] = in[i] + 1.0f;
}

/* A rectangular matrix fold retains its source scalar accumulation order. */
void high_ordered_fold(float * restrict out, const float * restrict in)
{
    int i, k;
    for (i = 0; i < 5; ++i) {
        float sum = 0.0f;
        for (k = 0; k < 37; ++k) sum = sum + in[i * 37 + k];
        out[i] = sum;
    }
}

/* Runtime bounds need an admitted symbolic-domain contract, absent in v1. */
void high_dynamic(float * restrict out, const float * restrict in, int n)
{
    int i;
    for (i = 0; i < n; ++i) out[i] = in[i];
}

/* Unsigned induction is not normalized by the admission observer. */
void high_unsigned(float * restrict out, const float * restrict in)
{
    unsigned i;
    for (i = 0; i < 37; ++i) out[i] = in[i];
}

/* Nonunit loops retain normal LNO behavior after a refusal. */
void high_stride(float * restrict out, const float * restrict in)
{
    int i;
    for (i = 0; i < 37; i += 2) out[i] = in[i];
}

/* Indirection cannot be promoted into a proved affine dependence domain. */
void high_indirect(float * restrict out, const float * restrict in, const int * restrict indices)
{
    int i;
    for (i = 0; i < 37; ++i) out[i] = in[indices[i]];
}

/* Volatile memory must not acquire ordinary scheduling eligibility. */
void high_volatile(float * restrict out, const volatile float * in)
{
    int i;
    for (i = 0; i < 37; ++i) out[i] = in[i];
}

extern float opaque_effect(float);
/* A call remains an effectful fallback rather than a missing graph edge. */
void high_call(float * restrict out, const float * restrict in)
{
    int i;
    for (i = 0; i < 37; ++i) out[i] = opaque_effect(in[i]);
}
