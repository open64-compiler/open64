/*
 * Private string-store checkpoint used by a caller-owned expansion.
 * The existing strtab owner saves its table pointer and append watermark;
 * this is not a mapped-image record or a general string mutation journal.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_STRTAB_CHECKPOINT_INCLUDED
#define DSL_STRTAB_CHECKPOINT_INCLUDED

#include <stdio.h>
#include "../com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

typedef struct {
    void *table;
    STR_IDX next_index;
} DSL_CKKS_STRTAB_SAVEPOINT;

/* Capture the current string table and append index; NULL rejects. */
extern BOOL DSL_CKKS_Strtab_Save (DSL_CKKS_STRTAB_SAVEPOINT *savepoint);
/* Restore only a matching live string table suffix; stale points reject. */
extern BOOL DSL_CKKS_Strtab_Restore
                     (const DSL_CKKS_STRTAB_SAVEPOINT *savepoint);

#endif /* DSL_STRTAB_CHECKPOINT_INCLUDED */
