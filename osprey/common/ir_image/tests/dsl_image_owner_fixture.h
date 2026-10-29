/*
 * Image-owner compatibility fixture imports the services exercised by the
 * linked builder and shape tests. Keeping one include at their existing schema
 * include site preserves __LINE__-derived WHIRL source positions. Production
 * callers import individual owner headers; the persistent schema re-exports none.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 */

#ifndef DSL_IMAGE_OWNER_FIXTURE_INCLUDED
#define DSL_IMAGE_OWNER_FIXTURE_INCLUDED

#include "../../com/dsl_builder.h"
#include "../dsl_image_service.h"
#include "../../../be/com/dsl_external_tensor.h"
#include "../../../be/com/dsl_ir_lower.h"
#include "../../../be/com/dsl_ir_retype.h"
#include "../../../be/com/dsl_ir_rewrite.h"
#include "../../../be/com/dsl_program_interface.h"
#include "../../../be/com/dsl_pu_interface_verify.h"
#include "../../../be/com/dsl_runtime_interface.h"

#endif /* DSL_IMAGE_OWNER_FIXTURE_INCLUDED */
