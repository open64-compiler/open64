/* Copyright (C) 2026 Open64 Project */
/* Standalone lifecycle tests for LNO's owned value contract, with no WHIRL
 * runtime. Design: doc/AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md. */
#include "../dsl_high_snapshot.h"
#include <assert.h>

/* Prove expiry/ABA/owner behavior with no live compiler contexts to consult. */
int main() {
    DSL_HIGH_EPOCH first("unit/function"), second("unit/function"), other("unit/other");
    DSL_HIGH_STAMP stamp = first.Stamp();
    assert(first.Matches(stamp));
    assert(!second.Matches(stamp)); assert(!other.Matches(stamp));
    DSL_HIGH_STAMP forged = stamp; forged.owner = "unit/other";
    assert(!first.Matches(forged));
    first.Before_mutation(); assert(!first.Matches(stamp));
    DSL_HIGH_STAMP new_stamp = first.Stamp(); assert(first.Matches(new_stamp));
    first.Close(); assert(!first.Matches(new_stamp));
    first.Before_mutation(); assert(!first.Matches(first.Stamp()));
    DSL_HIGH_SNAPSHOT empty; DSL_HIGH_BINDINGS bindings;
    assert(!empty.Is_current(second, bindings)); assert(!empty.Same_content(empty));
    DSL_HIGH_BINDINGS changed = bindings; assert(changed == bindings);
    changed.numeric = "strict"; assert(!(changed == bindings));
    puts("epoch_lifecycle=PASS"); return 0;
}
