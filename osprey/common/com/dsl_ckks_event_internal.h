/*
 * Copyright (C) 2026 Open64 Project
 *
 * Commit-only CKKS event image service. Producers must use the owner-PU
 * expansion transaction rather than insert rows directly.
 */

#ifndef dsl_ckks_event_internal_INCLUDED
#define dsl_ckks_event_internal_INCLUDED

#include "dsl_ckks_event.h"

extern DSL_CKKS_EVENT_ID DSL_CKKS_Event_Image_Add
                                (const DSL_CKKS_EVENT_RECORD *record);

#endif /* dsl_ckks_event_internal_INCLUDED */
