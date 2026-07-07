/*
 * Copyright (C) 2026 Open64 Project
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "mempool.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "stab.h"
#include "dsl_builder.h"
#include "open64_dsc_native_bridge.h"

static BOOL Open64_DSC_Context_Initialized = FALSE;

static void
Open64_DSC_Initialize_Context(void)
{
    if (Open64_DSC_Context_Initialized)
        return;

    MEM_Initialize();
    Set_Error_Tables(Phases, host_errlist);
    Init_Error_Handler(10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    Initialize_Symbol_Tables(TRUE);

    Open64_DSC_Context_Initialized = TRUE;
}

int
Open64_DSC_Finalize_Mapped_Image(const char *path)
{
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;

    if (path == NULL || path[0] == '\0')
        return 0;

    Open64_DSC_Initialize_Context();

    request.path = path;
    request.flags = 0;

    return DSL_Builder_Finalize_Mapped_Image(&request) ? 1 : 0;
}
