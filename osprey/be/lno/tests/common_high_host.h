/* Copyright (C) 2026 Open64 Project */
/* Host compatibility for ordinary whirl2c's legacy F32 intrinsic/inf names.
 * Design: doc/AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md.
 * This supplies standard math spellings only; all algorithm statements come
 * from the mapped High WHIRL image. No GPU or schedule semantics are added. */
#ifndef common_high_host_INCLUDED
#define common_high_host_INCLUDED
#include <math.h>
#define infF INFINITY
#define F4EXP expf
#define F4LOG logf
#endif
