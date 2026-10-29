/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/fhe exposes commit-only CKKS event insertion to the owner-PU expansion
 * transaction, which owns preflight and rollback. The existing .B event-row
 * ABI, ordering and version stay unchanged; this is not a public edit service.
 * Design: doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#ifndef dsl_ckks_event_internal_INCLUDED
#define dsl_ckks_event_internal_INCLUDED

#include "dsl_ckks_event.h"

/* Commit a copied preflighted row with the next one-based ID; never borrow
 * input storage. NULL returns invalid without mutation. No relation validation
 * occurs here: the transaction owns rollback via its saved row count. */
extern DSL_CKKS_EVENT_ID DSL_CKKS_Event_Image_Add
                                (const DSL_CKKS_EVENT_RECORD *record);

#endif /* dsl_ckks_event_internal_INCLUDED */
