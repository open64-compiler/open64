/*
 * Copyright (C) 2026 Open64 Project
 *
 * Clear algebra and structural checks for the ANT ACE-compatible ReLU recipe.
 * Design: doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md.
 */

#include <assert.h>
#include <math.h>

#include <vector>

#include "fhe_ckks_relu_recipe.h"

/* Resolve one recipe reference against clear inputs and prior step results. */
static double
Value(const VHO_FHE_RELU_VALUE_REF &reference, double original,
      double normalized, const std::vector<double> &results)
{
  switch (reference.kind) {
  case VHO_FHE_RELU_VALUE_ORIGINAL_INPUT:
    return original;
  case VHO_FHE_RELU_VALUE_NORMALIZED_INPUT:
    return normalized;
  case VHO_FHE_RELU_VALUE_PRIOR_STEP:
    assert(reference.step_index < results.size());
    return results[reference.step_index];
  default:
    assert(false);
    return 0.0;
  }
}

/* Execute the algebraic recipe without CKKS rounding or scale effects. */
static double
Evaluate_Recipe(const VHO_FHE_RELU_RECIPE &recipe, double original,
                double normalized)
{
  std::vector<double> results;
  for (UINT32 i = 0; i < recipe.steps.size(); ++i) {
    const VHO_FHE_RELU_RECIPE_STEP &step = recipe.steps[i];
    double value = 0.0;
    if (step.operation == VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT) {
      value = step.scalar;
    } else {
      double left = Value(step.left, original, normalized, results);
      if (step.operation == VHO_FHE_RELU_RECIPE_MUL_SCALAR)
        value = left * step.scalar;
      else {
        double right = Value(step.right, original, normalized, results);
        value = step.operation == VHO_FHE_RELU_RECIPE_ADD ?
            left + right : left * right;
      }
    }
    results.push_back(value);
  }
  return Value(recipe.result, original, normalized, results);
}

/* Evaluate one Chebyshev coefficient vector directly with T0..Tdegree order. */
static double
Evaluate_Chebyshev(const std::vector<double> &coefficients, double input)
{
  std::vector<double> terms(coefficients.size(), 0.0);
  terms[0] = 1.0;
  if (terms.size() > 1)
    terms[1] = input;
  for (UINT32 i = 2; i < terms.size(); ++i)
    terms[i] = 2.0 * input * terms[i - 1] - terms[i - 2];
  double result = 0.0;
  for (UINT32 i = 0; i < terms.size(); ++i)
    result += coefficients[i] * terms[i];
  return result;
}

/* Return the exact pinned ACE degree-7, degree-15, degree-13 coefficients. */
static std::vector<std::vector<double> >
Coefficients()
{
  std::vector<std::vector<double> > result;
  result.push_back(std::vector<double>{
      0.0, 1.277209679957775013e+00, 0.0,
      -4.369818210105346212e-01, 0.0, 2.781705762612975419e-01,
      0.0, -9.522998581241576277e-01});
  result.push_back(std::vector<double>{
      0.0, 1.336811809725395372e+00, 0.0,
      -3.314086854871873267e-01, 0.0, 2.739009935511804161e-01,
      0.0, -2.096678512577555831e-01, 0.0,
      6.827141455300124451e-02, 0.0, -1.036056317926726048e-02,
      0.0, 7.381161118162535544e-04, 0.0,
      -2.000350671563594715e-05});
  result.push_back(std::vector<double>{
      0.0, 1.229917329338358289e+00, 0.0,
      -3.099894039867301943e-01, 0.0, 1.047929208484282559e-01,
      0.0, -3.040264421328875422e-02, 0.0,
      6.507995190210730772e-03, 0.0, -8.815509689332230855e-04,
      0.0, 5.555595810150389487e-05});
  return result;
}

/* Check the ported decomposition against direct polynomial composition. */
int
main()
{
  const std::vector<std::vector<double> > coefficients = Coefficients();
  VHO_FHE_RELU_RECIPE recipe;
  assert(VHO_FHE_CKKS_Build_Ace_Relu_Recipe(
      coefficients, &recipe, stderr));
  assert(recipe.stages.size() == 3);
  assert(recipe.stages[0].degree == 7);
  assert(recipe.stages[1].degree == 15);
  assert(recipe.stages[2].degree == 13);
  assert(recipe.steps.size() == 94);
  assert(recipe.stages[0].first_step == 0 &&
         recipe.stages[0].step_count == 18);
  assert(recipe.stages[1].first_step == 18 &&
         recipe.stages[1].step_count == 35);
  assert(recipe.stages[2].first_step == 53 &&
         recipe.stages[2].step_count == 39);
  assert(recipe.result.step_index == 93);
  assert(recipe.result.kind == VHO_FHE_RELU_VALUE_PRIOR_STEP);
  for (UINT32 i = 0; i < recipe.steps.size(); ++i) {
    assert(recipe.steps[i].operation >= VHO_FHE_RELU_RECIPE_ADD);
    assert(recipe.steps[i].operation <=
           VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT);
    assert(recipe.steps[i].stage_ordinal <= 3);
  }

  static const double inputs[] = {
    -3.0, -1.0, -0.25, 0.0, 0.25, 1.0, 3.0
  };
  static const double bound = 3.5;
  for (UINT32 sample = 0; sample < sizeof(inputs) / sizeof(inputs[0]); ++sample) {
    double argument = inputs[sample] / bound;
    for (UINT32 stage = 0; stage < 3; ++stage)
      argument = Evaluate_Chebyshev(coefficients[stage], argument);
    double expected = 0.5 * inputs[sample] * argument +
                      0.5 * inputs[sample];
    double actual = Evaluate_Recipe(recipe, inputs[sample],
                                    inputs[sample] / bound);
    assert(fabs(actual - expected) < 1.0E-11);
  }

  std::vector<std::vector<double> > malformed = coefficients;
  malformed[1].pop_back();
  VHO_FHE_RELU_RECIPE unchanged = recipe;
  assert(!VHO_FHE_CKKS_Build_Ace_Relu_Recipe(
      malformed, &unchanged, NULL));
  assert(unchanged.steps.size() == recipe.steps.size());
  fprintf(stdout, "steps=%u stage_steps=%u,%u,%u depths=%u,%u,%u total=%u\n",
          (UINT32)recipe.steps.size(), recipe.stages[0].step_count,
          recipe.stages[1].step_count, recipe.stages[2].step_count,
          recipe.stages[0].output_depth - recipe.stages[0].input_depth,
          recipe.stages[1].output_depth - recipe.stages[1].input_depth,
          recipe.stages[2].output_depth - recipe.stages[2].input_depth,
          recipe.steps[recipe.result.step_index].algebraic_depth);
  return 0;
}
