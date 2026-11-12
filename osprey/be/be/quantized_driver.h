/* Copyright (C) 2026 Open64 Project */
/* Driver-owned Q1 entry ABI routing, never optimizer scope enlargement.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. */
#ifndef quantized_driver_INCLUDED
#define quantized_driver_INCLUDED
#include <stdio.h>
#include "defs.h"
struct pu_info;
class WN;
/* Read-only whole-file mode admission establishes one resident active PU;
 * source and caller eligibility are checked before native construction. */
extern BOOL BE_DSL_q1_prepare_file(struct pu_info *,BOOL,FILE *);
/* Driver restores only the resident scope it established during admission. */
extern BOOL BE_DSL_q1_resident(struct pu_info *);
/* Terminal standard output is selected only for an admitted explicit ABI. */
extern BOOL BE_DSL_q1_terminal(void);
/* Construct the faithful common High body; normal phases continue afterward. */
extern WN *BE_DSL_q1_route(struct pu_info *,WN *,FILE *);
#endif
