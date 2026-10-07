/*
 * Copyright (C) 2026 Open64 Project
 *
 * Build the provider-independent algebraic DAG for the pinned ANT ACE
 * composite ReLU polynomial. The executable CKKS materializer consumes this
 * recipe after adding explicit encode, rescale, relinearize, and bootstrap
 * state transitions. See doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md.
 */

#ifndef fhe_ckks_relu_recipe_INCLUDED
#define fhe_ckks_relu_recipe_INCLUDED

#include <stdio.h>

#include <vector>

#include "defs.h"

typedef enum {
  VHO_FHE_RELU_VALUE_INVALID = 0,
  VHO_FHE_RELU_VALUE_ORIGINAL_INPUT = 1,
  VHO_FHE_RELU_VALUE_NORMALIZED_INPUT = 2,
  VHO_FHE_RELU_VALUE_PRIOR_STEP = 3
} VHO_FHE_RELU_VALUE_KIND;

typedef struct {
  VHO_FHE_RELU_VALUE_KIND kind;
  UINT32 step_index;
} VHO_FHE_RELU_VALUE_REF;

typedef enum {
  VHO_FHE_RELU_RECIPE_INVALID = 0,
  VHO_FHE_RELU_RECIPE_ADD = 1,
  VHO_FHE_RELU_RECIPE_MUL = 2,
  VHO_FHE_RELU_RECIPE_MUL_SCALAR = 3,
  VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT = 4
} VHO_FHE_RELU_RECIPE_OPERATOR;

typedef struct {
  VHO_FHE_RELU_RECIPE_OPERATOR operation;
  VHO_FHE_RELU_VALUE_REF left;
  VHO_FHE_RELU_VALUE_REF right;
  double scalar;
  UINT32 stage_ordinal;
  UINT32 algebraic_depth;
} VHO_FHE_RELU_RECIPE_STEP;

typedef struct {
  UINT32 stage_ordinal;
  UINT32 degree;
  UINT32 first_step;
  UINT32 step_count;
  VHO_FHE_RELU_VALUE_REF result;
  UINT32 input_depth;
  UINT32 output_depth;
} VHO_FHE_RELU_RECIPE_STAGE;

typedef struct {
  std::vector<VHO_FHE_RELU_RECIPE_STEP> steps;
  std::vector<VHO_FHE_RELU_RECIPE_STAGE> stages;
  VHO_FHE_RELU_VALUE_REF result;
} VHO_FHE_RELU_RECIPE;

/*
 * Build the exact algebraic decomposition used by ANT ACE for the pinned
 * degree-7, degree-15, degree-13 Chebyshev composition. Coefficients use
 * direct T0..Tdegree order. The original input and its normalized copy are
 * distinct roots because ACE reconstructs 0.5*x*sign(x/B)+0.5*x.
 *
 * This routine is read-only with respect to WHIRL and replaces the output
 * only after every stage and reference validates.
 */
extern BOOL VHO_FHE_CKKS_Build_Ace_Relu_Recipe(
    const std::vector<std::vector<double> > &coefficients,
    VHO_FHE_RELU_RECIPE *recipe, FILE *diagnostic);

#endif /* fhe_ckks_relu_recipe_INCLUDED */
