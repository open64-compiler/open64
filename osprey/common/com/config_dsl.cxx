/*
 * Copyright (C) 2026 Open64 Project
 *
 * Configure the independent -DSL option group.  This file is included by
 * config.cxx after the common option infrastructure is available.
 */

#include "config_dsl.h"

BOOL VHO_DSL_Enable_Canonicalization = FALSE;
BOOL VHO_DSL_Enable_Canonicalization_Set = FALSE;
BOOL VHO_DSL_Enable_Descriptor_Propagation = FALSE;
BOOL VHO_DSL_Enable_Descriptor_Propagation_Set = FALSE;
BOOL VHO_DSL_Enable_Constant_Propagation = FALSE;
BOOL VHO_DSL_Enable_Constant_Propagation_Set = FALSE;
BOOL VHO_DSL_Enable_Algebraic_Simplification = FALSE;
BOOL VHO_DSL_Enable_Algebraic_Simplification_Set = FALSE;
BOOL VHO_DSL_Enable_Dead_Result_Elimination = FALSE;
BOOL VHO_DSL_Enable_Dead_Result_Elimination_Set = FALSE;
BOOL VHO_DSL_Enable_Common_Subexpression = FALSE;
BOOL VHO_DSL_Enable_Common_Subexpression_Set = FALSE;
BOOL VHO_DSL_Enable_SSA_PRE = FALSE;
BOOL VHO_DSL_Enable_SSA_PRE_Set = FALSE;
BOOL VHO_DSL_Enable_Quantization = FALSE;
BOOL VHO_DSL_Enable_Quantization_Set = FALSE;
BOOL VHO_DSL_Enable_Fusion = FALSE;
BOOL VHO_DSL_Enable_Fusion_Set = FALSE;
BOOL VHO_DSL_Enable_Parallelization = FALSE;
BOOL VHO_DSL_Enable_Parallelization_Set = FALSE;
BOOL VHO_DSL_Enable_Implementation_Selection = FALSE;
BOOL VHO_DSL_Enable_Implementation_Selection_Set = FALSE;

static OPTION_DESC Options_DSL[] = {
  { OVK_BOOL, OV_VISIBLE, TRUE, "canon", "canon",
    FALSE, 0, 0, &VHO_DSL_Enable_Canonicalization,
    &VHO_DSL_Enable_Canonicalization_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "descriptor_prop", "desc",
    FALSE, 0, 0, &VHO_DSL_Enable_Descriptor_Propagation,
    &VHO_DSL_Enable_Descriptor_Propagation_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "const_prop", "const_prop",
    FALSE, 0, 0, &VHO_DSL_Enable_Constant_Propagation,
    &VHO_DSL_Enable_Constant_Propagation_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "algebraic", "algebraic",
    FALSE, 0, 0, &VHO_DSL_Enable_Algebraic_Simplification,
    &VHO_DSL_Enable_Algebraic_Simplification_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "dce", "dce",
    FALSE, 0, 0, &VHO_DSL_Enable_Dead_Result_Elimination,
    &VHO_DSL_Enable_Dead_Result_Elimination_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "cse", "cse",
    FALSE, 0, 0, &VHO_DSL_Enable_Common_Subexpression,
    &VHO_DSL_Enable_Common_Subexpression_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "ssa_pre", "ssa_pre",
    FALSE, 0, 0, &VHO_DSL_Enable_SSA_PRE,
    &VHO_DSL_Enable_SSA_PRE_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "quant", "quant",
    FALSE, 0, 0, &VHO_DSL_Enable_Quantization,
    &VHO_DSL_Enable_Quantization_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "fusion", "fusion",
    FALSE, 0, 0, &VHO_DSL_Enable_Fusion,
    &VHO_DSL_Enable_Fusion_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "parallel", "parallel",
    FALSE, 0, 0, &VHO_DSL_Enable_Parallelization,
    &VHO_DSL_Enable_Parallelization_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "impl_select", "impl_select",
    FALSE, 0, 0, &VHO_DSL_Enable_Implementation_Selection,
    &VHO_DSL_Enable_Implementation_Selection_Set },
  { OVK_COUNT }
};
