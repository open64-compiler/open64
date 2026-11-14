/* Copyright (C) 2026 Open64 Project */
/* Selected-plan phase boundaries for the standalone driver-owned MLA bridge.
 * Active-PU only; no source traversal, candidate search or binary format change.
 * Contract: doc/AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md. */
#ifndef TENSOR_SELECTED_BRIDGE_H
#define TENSOR_SELECTED_BRIDGE_H
#include "tensor_kernel_lower.h"
/* Construct the fixed algorithm with explicit abstract participant formals,
 * staging loops and typed synchronization. Failure abandons this fresh PU. */
extern void VHO_tensor_selected_worker(const VHO_TENSOR_PROGRAM &,UINT32,SRCPOS,const std::string &,const std::string &,const std::string &device_key="");
/* Verify the full persisted level key and structural boundary, without mutation.
 * A mismatch throws before downstream lowering or emission. */
extern void VHO_tensor_verify_bridge(const char *);
/* Use native WN_Lower ARRAY/SCF services on the active verified High body.
 * No analyses survive this boundary; failure abandons the unpublished worker. */
extern void VHO_tensor_selected_memory();
/* Bind abstract block/thread owners to physical GPU coordinates and entry ABI.
 * The fixed plan has already determined all work, memory and collective scopes. */
extern void VHO_tensor_selected_gpu();
/* Emit only verified Low scalar native WHIRL, not a recipe or CUDA template.
 * Unsupported operations/types/effects fail before the caller publishes text. */
extern std::string CG_tensor_selected_ptx();
#endif
