/*
 * Verify that construction-time DSL canonicalization remains subordinate to
 * the traditional WHIRL simplifier master control.
 */

#include <stdio.h>

#include "defs.h"
#include "config.h"
#include "dsl_builder.h"

/*
 * This focused test links only the live builder control sections. The full
 * Open64 build supplies this definition through config.o.
 */
BOOL Enable_WN_Simp = TRUE;

int
main(void)
{
    DSL_Builder_Set_Canonicalization_Enabled(FALSE);
    if (DSL_Builder_Canonicalization_Enabled()) {
        fprintf(stderr, "disabled builder canonicalization became active\n");
        return 1;
    }

    DSL_Builder_Set_Canonicalization_Enabled(TRUE);
    if (!DSL_Builder_Canonicalization_Enabled()) {
        fprintf(stderr, "enabled builder canonicalization stayed inactive\n");
        return 1;
    }

    Enable_WN_Simp = FALSE;
    if (DSL_Builder_Canonicalization_Enabled()) {
        fprintf(stderr, "builder bypassed Enable_WN_Simp master control\n");
        return 1;
    }

    DSL_Builder_Set_Canonicalization_Enabled(TRUE);
    if (DSL_Builder_Canonicalization_Enabled()) {
        fprintf(stderr, "builder control re-enabled disabled wn_simp\n");
        return 1;
    }

    Enable_WN_Simp = TRUE;
    if (!DSL_Builder_Canonicalization_Enabled()) {
        fprintf(stderr, "builder restriction was not restored\n");
        return 1;
    }

    printf("DSL builder simplifier control contract passed\n");
    return 0;
}
