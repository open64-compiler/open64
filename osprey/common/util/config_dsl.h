/*
 * Copyright (C) 2026 Open64 Project
 */

/* Shared common/util declarations for the independent -DSL option group.
 * Callers provide the established Open64 scalar typedefs before this header.
 * Phase-owned meaning, defaults and explicit-set flags are unchanged; these
 * are process options, not persisted IR or permission to widen phase scope.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md. */
#ifndef config_dsl_INCLUDED
#define config_dsl_INCLUDED

/* Explicit production Q1 buffer ABI; NULL/off preserves ordinary routing. */
extern char *VHO_DSL_Q1_High;
extern char *VHO_DSL_Q1_Checkpoint;
extern BOOL VHO_DSL_Enable_WOPT;
extern BOOL VHO_DSL_Enable_WOPT_Set;
extern BOOL VHO_DSL_Enable_Shape_Refinement;
extern BOOL VHO_DSL_Enable_Shape_Refinement_Set;
extern BOOL VHO_DSL_Enable_Canonicalization;
extern BOOL VHO_DSL_Enable_Canonicalization_Set;
extern BOOL VHO_DSL_Enable_Descriptor_Propagation;
extern BOOL VHO_DSL_Enable_Descriptor_Propagation_Set;
extern BOOL VHO_DSL_Enable_Constant_Propagation;
extern BOOL VHO_DSL_Enable_Constant_Propagation_Set;
extern BOOL VHO_DSL_Enable_Algebraic_Simplification;
extern BOOL VHO_DSL_Enable_Algebraic_Simplification_Set;
extern BOOL VHO_DSL_Enable_Dead_Result_Elimination;
extern BOOL VHO_DSL_Enable_Dead_Result_Elimination_Set;
extern BOOL VHO_DSL_Enable_Common_Subexpression;
extern BOOL VHO_DSL_Enable_Common_Subexpression_Set;
extern BOOL VHO_DSL_Enable_SSA_PRE;
extern BOOL VHO_DSL_Enable_SSA_PRE_Set;
extern BOOL VHO_DSL_Enable_Quantization;
extern BOOL VHO_DSL_Enable_Quantization_Set;
extern BOOL VHO_DSL_Enable_Fusion;
extern BOOL VHO_DSL_Enable_Fusion_Set;
extern BOOL VHO_DSL_Enable_Parallelization;
extern BOOL VHO_DSL_Enable_Parallelization_Set;
extern BOOL VHO_DSL_Enable_Implementation_Selection;
extern BOOL VHO_DSL_Enable_Implementation_Selection_Set;
extern BOOL VHO_DSL_Dump_After_Lowering;
extern BOOL VHO_DSL_Dump_After_Lowering_Set;
extern BOOL VHO_DSL_Dump_After_Shape_Refinement;
extern BOOL VHO_DSL_Dump_After_Shape_Refinement_Set;
extern BOOL VHO_DSL_AI_Opt_Trace;
extern BOOL VHO_DSL_AI_Opt_Trace_Set;
extern BOOL VHO_DSL_Partition_Axis;
extern BOOL VHO_DSL_Partition_Axis_Set;
extern BOOL VHO_DSL_CUDA_TMA_Prefetch;
extern char *VHO_DSL_GEMM_Plan_Search;
extern char *VHO_DSL_GEMM_Cost_Model;
extern char *VHO_DSL_GPU_Arch;
/* Exact optional device key; backend admission resolves its baseline ISA.
 * NULL preserves architecture-only behavior. No resource/profile payload.
 * Design: doc/AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md. */
extern char *VHO_DSL_GPU_Device;
/* H1b exact file bytes require a digest pin and declared target toolchain.
 * All NULL preserves H1a/architecture-only routing. Import is driver-owned.
 * Design: doc/AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md. */
extern char *VHO_DSL_GPU_Profile;
extern char *VHO_DSL_GPU_Profile_SHA256;
extern char *VHO_DSL_GPU_Profile_Toolchain;
/* Driver-only synchronous publication; phase owners copy before exploration.
 * Borrow expires at driver cleanup. Not a mutable resource-table row. */
#ifdef __cplusplus
class DSL_GPU_DEVICE_CONTEXT;
extern const DSL_GPU_DEVICE_CONTEXT *VHO_DSL_GPU_Context;
#endif
extern UINT32 VHO_DSL_GEMM_Memory_Latency;
extern BOOL VHO_DSL_GEMM_Memory_Latency_Set;
extern BOOL VHO_DSL_CUDA_TMA_Prefetch_Set;
extern BOOL VHO_DSL_CUDA_SMEM_Layout_Search;
extern BOOL VHO_DSL_CUDA_SMEM_Layout_Search_Set;
extern BOOL VHO_DSL_CUDA_Tile_Apply;
extern BOOL VHO_DSL_CUDA_Tile_Apply_Set;
extern BOOL VHO_DSL_CUDA_Legacy_Emit;
extern BOOL VHO_DSL_CUDA_Legacy_Emit_Set;
extern char *VHO_DSL_PU_Specialization_Checkpoint_Output;
extern char *VHO_DSL_CUDA_Tile_Output;
extern char *VHO_DSL_CUDA_Whirl_Output;
extern char *VHO_DSL_Normalization_Request;
extern char *VHO_DSL_Normalization_Output;
extern char *VHO_DSL_Normalization_Mode;
extern char *VHO_DSL_Normalization_Host_Entry;

/* Reserved TP_WOPT2 bit for versioned AI optimization decisions. */
#define AI_OPT_TRACE_FLAG 0x00800000

#endif /* config_dsl_INCLUDED */
