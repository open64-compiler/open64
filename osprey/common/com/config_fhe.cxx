/*
 * Copyright (C) 2026 Open64 Project
 *
 * Configure the independent -FHE option group.  This file is included by
 * config.cxx after the common option infrastructure is available.
 */

#include "config_fhe.h"

BOOL VHO_FHE_Enable_Conversion = TRUE;
BOOL VHO_FHE_Enable_Conversion_Set = FALSE;
BOOL VHO_FHE_Strict_O0 = TRUE;
BOOL VHO_FHE_Strict_O0_Set = FALSE;
BOOL VHO_FHE_Dump_Before_Conversion = FALSE;
BOOL VHO_FHE_Dump_Before_Conversion_Set = FALSE;
BOOL VHO_FHE_Dump_After_Conversion = FALSE;
BOOL VHO_FHE_Dump_After_Conversion_Set = FALSE;
char *VHO_FHE_Conversion_Checkpoint_Output = NULL;

static OPTION_DESC Options_FHE[] = {
  { OVK_BOOL, OV_VISIBLE, TRUE, "convert", "convert",
    TRUE, 0, 0, &VHO_FHE_Enable_Conversion,
    &VHO_FHE_Enable_Conversion_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "strict_o0", "strict_o0",
    TRUE, 0, 0, &VHO_FHE_Strict_O0,
    &VHO_FHE_Strict_O0_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "dump_before", "dump_before",
    FALSE, 0, 0, &VHO_FHE_Dump_Before_Conversion,
    &VHO_FHE_Dump_Before_Conversion_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "dump_after", "dump_after",
    FALSE, 0, 0, &VHO_FHE_Dump_After_Conversion,
    &VHO_FHE_Dump_After_Conversion_Set },
  { OVK_NAME, OV_VISIBLE, FALSE, "checkpoint", "checkpoint",
    0, 0, 0, &VHO_FHE_Conversion_Checkpoint_Output, NULL },
  { OVK_COUNT }
};
