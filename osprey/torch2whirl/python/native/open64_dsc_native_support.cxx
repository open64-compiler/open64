/*
 * Copyright (C) 2026 Open64 Project
 */

#include <stdio.h>

#include "defs.h"
#include "srcpos.h"

class WN;

/*
 * Common WHIRL simplification code references this backend phase switch even
 * when torch2whirl is only finalizing frontend-built global tables.
 */
BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

extern "C" const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    (void) kind;
    (void) parm;

    return "<open64_dsc host parameter>";
}

extern "C" void
Signal_Cleanup(INT sig)
{
    (void) sig;
}

BOOL
IR_set_dump_order(BOOL prefix)
{
    return prefix;
}

void
fdump_tree(FILE *f, WN *wn)
{
    (void) wn;

    if (f != NULL)
        fputs("<open64_dsc tree dump unavailable>\n", f);
}

void
IR_Srcpos_Filename(SRCPOS srcpos, const char **fname, const char **dirname)
{
    (void) srcpos;

    if (fname != NULL)
        *fname = NULL;
    if (dirname != NULL)
        *dirname = NULL;
}
