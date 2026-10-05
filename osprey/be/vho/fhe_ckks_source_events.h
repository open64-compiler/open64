/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE VHO bridge from existing managed schedule/call tables to CKKS events.
 * This is a read-only compiler-phase service, not a WHIRL image or opcode.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_source_events_INCLUDED
#define fhe_ckks_source_events_INCLUDED

#include <stdio.h>
#include <vector>

#include "defs.h"
#include "fhe_ckks_event_coverage.h"

/*
 * Collect exact source events before the first CKKS native expansion mutates
 * any PU. The caller owns output; it is replaced only after all source rows,
 * call routes, and the independently counted dynamic total validate. Nested
 * contexts without a typed full-path identity fail closed.
 */
BOOL VHO_FHE_CKKS_Collect_Source_Events(
    std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> *events,
    FILE *diagnostic);

#endif /* fhe_ckks_source_events_INCLUDED */
