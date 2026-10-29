/*
 * Backend-free physical PU and call ABI verification over the selected
 * PU tree and resident local symbols. No PU switching or transformation.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_PU_INTERFACE_VERIFY_INCLUDED
#define DSL_PU_INTERFACE_VERIFY_INCLUDED

#include <stdio.h>
#include "../../common/com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/* With driver-established active PU/tree/local-symbol context and consistent
 * call/value/interface/projection tables, including canonical formal rows for
 * every represented callee-side argument, check that PU's physical caller and
 * callee ABI sides, accounting for retired slots and runtime projections.
 * This does not replace full runtime/type/row or handle-initialization checks.
 * Does not activate other PUs or mutate trees/tables. FALSE reports the first
 * mismatch; NULL diagnostic is silent. Implemented in dsl_pu_interface_verify.cxx. */
extern BOOL DSL_Call_ABI_Image_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);

/* Empty formal images return TRUE even for NULL pu (legacy compatibility).
 * Otherwise require driver-established matching Current_pu, live FUNC_ENTRY,
 * local ST/string context and consistent structural/value/retirement/projection
 * tables. Compare source-ordinal-sorted nonretired rows with physical IDNAMEs,
 * substituting projected handles, and include runtime-binding count in coverage.
 * Does not activate other PUs or independently prove each runtime binding's
 * physical identity. No tree/table mutation; FALSE reports the first mismatch,
 * NULL diagnostic is silent. Implemented in dsl_pu_interface_verify.cxx. */
extern BOOL DSL_PU_Interface_Image_Validate_PU
                                (PU_Info *pu, FILE *diagnostic);

#endif /* DSL_PU_INTERFACE_VERIFY_INCLUDED */
