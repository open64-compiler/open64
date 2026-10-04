/*
 * Copyright (C) 2026 Open64 Project
 *
 * Group complete, context-bound CKKS circuit plans into reusable PU variants.
 * This FHE policy preflight does not clone PUs or mutate mapped WHIRL.
 * Design: doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_variant_signature_INCLUDED
#define fhe_ckks_variant_signature_INCLUDED

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <vector>

#include "fhe_ckks_event_coverage.h"

typedef struct {
  VHO_FHE_CKKS_EVENT_IDENTITY event;
  /* Canonical, versioned bytes of the full executable plan for this event:
   * ordered operators/operands, TY/layout, every result state, keys, and
   * rotation requirements. A typed B formal role is encoded, not its
   * context-specific TCON bytes. The caller owns this buffer. */
  const unsigned char *plan_bytes;
  size_t plan_size;
} VHO_FHE_CKKS_SIGNATURE_EVENT;

typedef struct {
  uint32_t source_owner_pu_st;
  bool use_existing_pu;
  std::vector<unsigned char> signature_bytes;
  std::vector<uint32_t> context_callsites;
} VHO_FHE_CKKS_SIGNATURE_VARIANT;

/* Verify exact source-event coverage against independently counted static
 * and dynamic schedule totals, and identical static-event shape for
 * every context of one source PU. Group only byte-identical whole-PU plans;
 * call ordinal and bound TCON bytes never select a variant. The earliest
 * source context chooses the existing PU. Failure leaves variants untouched.
 * The producer must first serialize complete reviewed CKKS step plans; this
 * function does not claim opaque input bytes are themselves semantically
 * legal or produce an executable .B artifact. */
bool VHO_FHE_CKKS_Build_Variant_Signatures(
    const VHO_FHE_CKKS_EVENT_IDENTITY *source_events,
    size_t source_event_count,
    size_t expected_static_event_count,
    size_t expected_dynamic_event_count,
    const VHO_FHE_CKKS_SIGNATURE_EVENT *plans,
    size_t plan_count,
    std::vector<VHO_FHE_CKKS_SIGNATURE_VARIANT> *variants,
    FILE *diagnostic);

#endif /* fhe_ckks_variant_signature_INCLUDED */
