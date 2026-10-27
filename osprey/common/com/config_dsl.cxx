/*
 * Copyright (C) 2026 Open64 Project
 *
 * Configure the independent -DSL option group.  This file is included by
 * config.cxx after the common option infrastructure is available.
 */

#include "config_dsl.h"

BOOL VHO_DSL_Enable_WOPT = FALSE;
BOOL VHO_DSL_Enable_WOPT_Set = FALSE;
BOOL VHO_DSL_Enable_Shape_Refinement = TRUE;
BOOL VHO_DSL_Enable_Shape_Refinement_Set = FALSE;
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
BOOL VHO_DSL_Dump_After_Lowering = FALSE;
BOOL VHO_DSL_Dump_After_Lowering_Set = FALSE;
BOOL VHO_DSL_Dump_After_Shape_Refinement = FALSE;
BOOL VHO_DSL_Dump_After_Shape_Refinement_Set = FALSE;
BOOL VHO_DSL_AI_Opt_Trace = TRUE;
BOOL VHO_DSL_AI_Opt_Trace_Set = FALSE;
BOOL VHO_DSL_CUDA_TMA_Prefetch = FALSE;
BOOL VHO_DSL_CUDA_TMA_Prefetch_Set = FALSE;
BOOL VHO_DSL_CUDA_SMEM_Layout_Search = TRUE;
BOOL VHO_DSL_CUDA_SMEM_Layout_Search_Set = FALSE;
BOOL VHO_DSL_CUDA_Tile_Apply = FALSE;
BOOL VHO_DSL_CUDA_Tile_Apply_Set = FALSE;
BOOL VHO_DSL_CUDA_Legacy_Emit = TRUE;
BOOL VHO_DSL_CUDA_Legacy_Emit_Set = FALSE;
char *VHO_DSL_PU_Specialization_Checkpoint_Output = NULL;
char *VHO_DSL_CUDA_Tile_Output = NULL;
char *VHO_DSL_CUDA_Whirl_Output = NULL;
char *VHO_DSL_GEMM_Plan_Search = NULL;
char *VHO_DSL_GEMM_Cost_Model = NULL;
UINT32 VHO_DSL_GEMM_Memory_Latency = 100;

static OPTION_DESC Options_DSL[] = {
  { OVK_NAME, OV_VISIBLE, FALSE, "gemm_cost_model", "gemm_cost_model",
    0, 0, 0, &VHO_DSL_GEMM_Cost_Model, NULL },
  { OVK_UINT32, OV_VISIBLE, FALSE, "gemm_memory_latency", "gemm_memory_latency",
    100, 0, 1000000, &VHO_DSL_GEMM_Memory_Latency, NULL },
  { OVK_NAME, OV_VISIBLE, FALSE, "gemm_plan_search", "gemm_plan_search",
    0, 0, 0, &VHO_DSL_GEMM_Plan_Search, NULL },
  { OVK_BOOL, OV_VISIBLE, TRUE, "cuda_smem_layout_search", "cuda_smem_layout_search",
    FALSE, 0, 0, &VHO_DSL_CUDA_SMEM_Layout_Search,
    &VHO_DSL_CUDA_SMEM_Layout_Search_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "ai_opt_trace", "ai_opt_trace",
    FALSE, 0, 0, &VHO_DSL_AI_Opt_Trace,
    &VHO_DSL_AI_Opt_Trace_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "cuda_tma_prefetch", "cuda_tma_prefetch",
    FALSE, 0, 0, &VHO_DSL_CUDA_TMA_Prefetch,
    &VHO_DSL_CUDA_TMA_Prefetch_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "cuda_tile_apply", "cuda_tile_apply",
    FALSE, 0, 0, &VHO_DSL_CUDA_Tile_Apply,
    &VHO_DSL_CUDA_Tile_Apply_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "cuda_legacy_emit", "cuda_legacy_emit",
    FALSE, 0, 0, &VHO_DSL_CUDA_Legacy_Emit,
    &VHO_DSL_CUDA_Legacy_Emit_Set },
  { OVK_NAME, OV_VISIBLE, FALSE, "cuda_tile_output", "cuda_tile_output",
    0, 0, 0, &VHO_DSL_CUDA_Tile_Output, NULL },
  { OVK_NAME, OV_VISIBLE, FALSE, "cuda_whirl_output", "cuda_whirl_output",
    0, 0, 0, &VHO_DSL_CUDA_Whirl_Output, NULL },
  { OVK_NAME, OV_VISIBLE, FALSE, "pu_specialization_checkpoint",
    "pu_specialization_checkpoint", 0, 0, 0,
    &VHO_DSL_PU_Specialization_Checkpoint_Output, NULL },
  { OVK_BOOL, OV_VISIBLE, TRUE, "shape_refine", "shape_refine",
    FALSE, 0, 0, &VHO_DSL_Enable_Shape_Refinement,
    &VHO_DSL_Enable_Shape_Refinement_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "wopt", "wopt",
    FALSE, 0, 0, &VHO_DSL_Enable_WOPT,
    &VHO_DSL_Enable_WOPT_Set },
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
  { OVK_BOOL, OV_VISIBLE, TRUE, "dump_after_lower", "dump_after_lower",
    FALSE, 0, 0, &VHO_DSL_Dump_After_Lowering,
    &VHO_DSL_Dump_After_Lowering_Set },
  { OVK_BOOL, OV_VISIBLE, TRUE, "dump_after_shape_refine",
    "dump_after_shape_refine", FALSE, 0, 0,
    &VHO_DSL_Dump_After_Shape_Refinement,
    &VHO_DSL_Dump_After_Shape_Refinement_Set },
  { OVK_COUNT }
};
