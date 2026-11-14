/* Copyright (C) 2026 Open64 Project */

/* Read-only active-PU partition-domain discovery after PREOPT/LNO access build.
 * Design: doc/AI-COMPILER-NATIVE-PARTITION-AXIS-v0.1.md.
 * Transient facts only: no WHIRL/table/image change or execution selection. */
#ifndef dsl_partition_axis_INCLUDED
#define dsl_partition_axis_INCLUDED

#include "defs.h"
#include "wn.h"

class DU_MANAGER;
struct ALIAS_MANAGER;

/* Driver-lifetime observations, valid only while the source and maps are
 * unchanged. Only status="proved" grants established I/K domains. All source
 * pointers are borrowed; consumers must not mutate through this callback. The
 * self operand preserves actual ADD operand order, including NaN-sensitive
 * source distinctions. No normalized operator or numerical binding is inferred. */
struct LNO_DSL_PARTITION_FACT {
  WN *outer;
  WN *inner;
  WN *init;
  WN *update;
  WN *output;
  WN *self;
  WN *final_use;
  WN *input;
  WN *second_input;
  WN *contributor_definition;
  UINT64 instances;
  UINT64 contributions;
  INT instance_axis;
  INT contribution_axis;
  INT self_operand;
  const char *recipe;
  const char *status;
};

typedef void (*LNO_DSL_PARTITION_VISITOR)
    (const LNO_DSL_PARTITION_FACT *fact, void *context);

/* Consume current driver-owned DU/alias and LNO access/parent maps. Scope must
 * be the active FUNC_ENTRY; no REGION or cross-PU analysis is performed. A
 * private reduction map recognizes recurrences without granting reassociation.
 * Return the number proved; optionally report every bounded candidate/refusal.
 * Facts expire at the next tree, alias, DU or access-map mutation. Trace NULL
 * suppresses diagnostics only and cannot change the analysis or input IR. */
extern UINT32 LNO_DSL_discover_partition_axes
    (WN *scope, DU_MANAGER *du, ALIAS_MANAGER *alias, FILE *trace,
     LNO_DSL_PARTITION_VISITOR visitor = NULL, void *context = NULL);

#endif
