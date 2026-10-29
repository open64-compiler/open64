/* Stable runtime identities shared by general telemetry and phase-owned plans.
 * common/com owns these lightweight labels for the retained telemetry service;
 * they neither define optimization stores nor add persisted WHIRL image rows.
 * Phase-specific construction, lookup, selection and lifetime stay with VHO.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md. */
#ifndef dsl_plan_identity_INCLUDED
#define dsl_plan_identity_INCLUDED

#include "defs.h"

typedef UINT32 DSL_OPT_PLAN_ID;
typedef UINT32 DSL_RUNTIME_VARIANT_SITE_ID;
typedef UINT32 DSL_RUNTIME_VARIANT_ID;

#endif
