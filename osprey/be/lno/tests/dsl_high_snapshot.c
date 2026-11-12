/* Copyright (C) 2026 Open64 Project */
/* Native PU fixtures for owned High facts; this is source arithmetic, not
 * metadata-based legality. Design:
 * doc/AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md. */

/* A carried dependence supplies nonempty full DEPV vectors for perturbation. */
void snapshot_carried(float * restrict out, const float * restrict in)
{
    int i;
    for (i = 0; i < 37; ++i) out[i + 1] = out[i] + in[i];
}

/* Separate PU, nest and affine coefficients test independent identities. */
void snapshot_nested(float * restrict out, const float * restrict in)
{
    int i, j;
    for (i = 0; i < 7; ++i)
        for (j = 0; j < 37; ++j) out[i * 37 + j] = in[i * 37 + j] + 2.0f;
}

/* No snapshot must be published when canonical form admission refuses. */
void snapshot_dynamic(float * restrict out, const float * restrict in, int n)
{
    int i;
    for (i = 0; i < n; ++i) out[i] = in[i];
}

/* Canonical form is admissible, but extended constants are outside the owned
 * payload subset. Collector refusal must still leave ordinary LNO unchanged. */
void snapshot_extended_constant(long double * restrict out, const long double * restrict in)
{
    int i;
    for (i = 0; i < 37; ++i) out[i] = in[i] + 1.25L;
}
