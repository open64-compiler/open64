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

/* Capture the current table pointer and nonzero append index into caller POD
 * storage. NULL output, absent table or zero index returns FALSE without output
 * writes. No string copies, generation token, reservation or table mutation;
 * caller must keep this exact store and its saved prefix alive until restore. */
extern BOOL DSL_CKKS_Strtab_Save (DSL_CKKS_STRTAB_SAVEPOINT *savepoint);
/* Require a nonnull point with matching table pointer and nonzero saved index
 * no greater than the current end; FALSE changes nothing. Then rewind the end
 * and rebuild interning hashes, invalidating appended STR_IDX meanings. Pointer/
 * bound checks do not detect reset/reuse of the same store or recover old bytes;
 * caller excludes those cases and restores all consumers of discarded strings.
 * No buffer shrink or wider rollback; hash-allocation failures are uncaught. */
extern BOOL DSL_CKKS_Strtab_Restore
                     (const DSL_CKKS_STRTAB_SAVEPOINT *savepoint);

#endif /* DSL_STRTAB_CHECKPOINT_INCLUDED */
