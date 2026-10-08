/*
 * Copyright (C) 2026 Open64 Project
 *
 * ANT ACE-compatible Chebyshev decomposition for executable ReLU planning.
 * This is an FHE semantic recipe, not a WHIRL mutation service. See
 * doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md.
 */

#include "fhe_ckks_relu_recipe.h"

#include <math.h>

#include <map>
#include <utility>

namespace {

static const double VHO_FHE_RELU_SMALL_COEFFICIENT = 1.0E-30;

struct POLY_NODE {
  POLY_NODE(const std::vector<double> &values, UINT32 actual)
      : coefficients(values), actual_degree(actual), divisor_power(0),
        quotient(NULL), remainder(NULL) {}

  ~POLY_NODE()
  {
    delete quotient;
    delete remainder;
  }

  std::vector<double> coefficients;
  UINT32 actual_degree;
  UINT32 divisor_power;
  POLY_NODE *quotient;
  POLY_NODE *remainder;

private:
  POLY_NODE(const POLY_NODE &);
  POLY_NODE &operator=(const POLY_NODE &);
};

/* Report one stable recipe diagnostic without exposing ACE implementation IR. */
static BOOL
Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-RELU-001: %s\n", message);
  return FALSE;
}

/* Return the greatest power of two not greater than a positive integer. */
static UINT32
Floor_Power_Of_Two(UINT32 value)
{
  UINT32 result = 1;
  while (result <= value / 2)
    result <<= 1;
  return result;
}

/* Return the least power of two not less than a positive integer. */
static UINT32
Ceiling_Power_Of_Two(UINT32 value)
{
  UINT32 result = 1;
  while (result < value)
    result <<= 1;
  return result;
}

/* Reproduce ACE's low-degree precomputation threshold for one stage. */
static UINT32
Precompute_Degree(UINT32 degree)
{
  UINT32 ceiling = Ceiling_Power_Of_Two(degree + 1);
  UINT32 logarithm = 0;
  for (UINT32 value = ceiling; value > 1; value >>= 1)
    ++logarithm;
  return (UINT32)1 << (logarithm / 2);
}

/* Reproduce ACE's full-decomposition threshold for one stage. */
static UINT32
Full_Decomposition_Degree(UINT32 degree)
{
  UINT32 ceiling = Ceiling_Power_Of_Two(degree + 1);
  UINT32 logarithm = 0;
  for (UINT32 value = ceiling; value > 1; value >>= 1)
    ++logarithm;
  UINT32 low_power = (UINT32)1 << (logarithm / 2);
  return ceiling - low_power / 2;
}

/*
 * Split a Chebyshev polynomial as q*T_(2^k)+r using the identity applied by
 * ACE. The recursion shape, not allocation order, defines the recipe.
 */
static void
Decompose(POLY_NODE *node, UINT32 full_degree, UINT32 precompute_degree)
{
  UINT32 degree = (UINT32)node->coefficients.size() - 1;
  if (degree <= 1 ||
      (degree < precompute_degree && node->actual_degree < full_degree))
    return;

  UINT32 divisor_degree = Floor_Power_Of_Two(degree);
  UINT32 divisor_power = 0;
  for (UINT32 value = divisor_degree; value > 1; value >>= 1)
    ++divisor_power;
  node->divisor_power = divisor_power;

  std::vector<double> quotient(
      node->coefficients.begin() + divisor_degree,
      node->coefficients.end());
  std::vector<double> remainder(
      node->coefficients.begin(),
      node->coefficients.begin() + divisor_degree);
  for (UINT32 term = 1; term < quotient.size(); ++term)
    remainder[divisor_degree - term] -= quotient[term];
  for (UINT32 term = 1; term < quotient.size(); ++term)
    quotient[term] *= 2.0;

  UINT32 remainder_actual = node->actual_degree -
      ((UINT32)node->coefficients.size() - (UINT32)remainder.size());
  node->remainder = new POLY_NODE(remainder, remainder_actual);
  node->quotient = new POLY_NODE(quotient, node->actual_degree);
  Decompose(node->remainder, full_degree, precompute_degree);
  Decompose(node->quotient, full_degree, precompute_degree);
}

class RECIPE_BUILDER {
public:
  explicit RECIPE_BUILDER(VHO_FHE_RELU_RECIPE *recipe)
      : _recipe(recipe), _stage(0) {}

  /* Append an addition and derive its algebraic ciphertext depth. */
  VHO_FHE_RELU_VALUE_REF Add(VHO_FHE_RELU_VALUE_REF left,
                             VHO_FHE_RELU_VALUE_REF right)
  {
    return Append(VHO_FHE_RELU_RECIPE_ADD, left, right, 0.0,
                  Maximum_Depth(left, right));
  }

  /* Append a ciphertext product and account for one multiplicative layer. */
  VHO_FHE_RELU_VALUE_REF Multiply(VHO_FHE_RELU_VALUE_REF left,
                                  VHO_FHE_RELU_VALUE_REF right)
  {
    return Append(VHO_FHE_RELU_RECIPE_MUL, left, right, 0.0,
                  Maximum_Depth(left, right) + 1);
  }

  /* Append one scalar coefficient use; CKKS repair is added by materializer. */
  VHO_FHE_RELU_VALUE_REF Multiply_Scalar(VHO_FHE_RELU_VALUE_REF value,
                                         double scalar)
  {
    return Append(VHO_FHE_RELU_RECIPE_MUL_SCALAR, value,
                  Invalid_Reference(), scalar, Depth(value) + 1);
  }

  /* Build one ACE polynomial stage and return its final value. */
  VHO_FHE_RELU_VALUE_REF Build_Stage(
      const std::vector<double> &coefficients,
      VHO_FHE_RELU_VALUE_REF argument,
      VHO_FHE_RELU_VALUE_REF original, BOOL outermost)
  {
    _precomputed.clear();
    _powers_of_two.clear();
    const UINT32 degree = (UINT32)coefficients.size() - 1;
    const UINT32 precompute = Precompute_Degree(degree);
    _precomputed[1] = argument;
    for (UINT32 item = 2; item < precompute; ++item) {
      UINT32 first_degree = item / 2;
      UINT32 second_degree = item % 2 == 0 ? first_degree : first_degree + 1;
      VHO_FHE_RELU_VALUE_REF product = Multiply(
          _precomputed[first_degree], _precomputed[second_degree]);
      VHO_FHE_RELU_VALUE_REF doubled = Add(product, product);
      if (item % 2 == 0)
        _precomputed[item] = Constant_Minus_One(doubled);
      else
        _precomputed[item] =
            Add(doubled, Multiply_Scalar(argument, -1.0));
    }

    for (UINT32 item = 1; item <= degree; item <<= 1) {
      std::map<UINT32, VHO_FHE_RELU_VALUE_REF>::const_iterator known =
          _precomputed.find(item);
      if (known != _precomputed.end()) {
        _powers_of_two.push_back(known->second);
        continue;
      }
      VHO_FHE_RELU_VALUE_REF prior = _powers_of_two.back();
      VHO_FHE_RELU_VALUE_REF product = Multiply(prior, prior);
      VHO_FHE_RELU_VALUE_REF doubled = Add(product, product);
      _powers_of_two.push_back(Constant_Minus_One(doubled));
    }

    POLY_NODE root(coefficients, degree);
    Decompose(&root, Full_Decomposition_Degree(degree), precompute);
    return Build_Node(root, original, outermost);
  }

  void Set_Stage(UINT32 stage) { _stage = stage; }

  UINT32 Depth(VHO_FHE_RELU_VALUE_REF value) const
  {
    if (value.kind == VHO_FHE_RELU_VALUE_ORIGINAL_INPUT ||
        value.kind == VHO_FHE_RELU_VALUE_NORMALIZED_INPUT)
      return 0;
    return value.kind == VHO_FHE_RELU_VALUE_PRIOR_STEP &&
           value.step_index < _recipe->steps.size() ?
        _recipe->steps[value.step_index].algebraic_depth : ~(UINT32)0;
  }

private:
  /* Build a constant-minus-one adjustment as ACE's doubled value plus -1. */
  VHO_FHE_RELU_VALUE_REF Constant_Minus_One(
      VHO_FHE_RELU_VALUE_REF doubled)
  {
    return Append(VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT,
                  doubled, Invalid_Reference(), -1.0, 0,
                  &doubled);
  }

  /* Recursively emit q*T_(2^k)+r for one decomposed polynomial node. */
  VHO_FHE_RELU_VALUE_REF Build_Node(
      const POLY_NODE &node, VHO_FHE_RELU_VALUE_REF original,
      BOOL outermost)
  {
    if (node.quotient == NULL) {
      VHO_FHE_RELU_VALUE_REF sum = Invalid_Reference();
      for (UINT32 degree = 0; degree < node.coefficients.size(); ++degree) {
        double coefficient = node.coefficients[degree];
        if (fabs(coefficient) < VHO_FHE_RELU_SMALL_COEFFICIENT)
          continue;
        if (outermost)
          coefficient *= 0.5;
        VHO_FHE_RELU_VALUE_REF item;
        if (outermost) {
          /*
           * ACE multiplies the refreshed original input by the encoded
           * coefficient before multiplying by T_degree. This ordering is
           * semantically significant for CKKS depth: (x*c)*T_degree avoids
           * the extra layer introduced by (T_degree*c)*x and also makes the
           * outer T0 term part of 0.5*x*sign(x/B).
           */
          item = Multiply_Scalar(original, coefficient);
          if (degree > 0)
            item = Multiply(_precomputed[degree], item);
        } else if (degree == 0) {
          item = Scalar_Constant(coefficient);
        } else {
          item = Multiply_Scalar(_precomputed[degree], coefficient);
        }
        sum = sum.kind == VHO_FHE_RELU_VALUE_INVALID ? item : Add(item, sum);
      }
      return sum;
    }
    VHO_FHE_RELU_VALUE_REF quotient =
        Build_Node(*node.quotient, original, outermost);
    VHO_FHE_RELU_VALUE_REF result = quotient.kind ==
        VHO_FHE_RELU_VALUE_INVALID ? Invalid_Reference() :
        Multiply(_powers_of_two[node.divisor_power], quotient);
    VHO_FHE_RELU_VALUE_REF remainder =
        Build_Node(*node.remainder, original, outermost);
    return result.kind == VHO_FHE_RELU_VALUE_INVALID ? remainder :
        remainder.kind == VHO_FHE_RELU_VALUE_INVALID ? result :
        Add(result, remainder);
  }

  /* Represent a standalone scalar as a scalar step with no tensor input. */
  VHO_FHE_RELU_VALUE_REF Scalar_Constant(double scalar)
  {
    return Append(VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT,
                  Invalid_Reference(), Invalid_Reference(), scalar, 0);
  }

  /* Append one recipe step, optionally adding a scalar to an existing value. */
  VHO_FHE_RELU_VALUE_REF Append(
      VHO_FHE_RELU_RECIPE_OPERATOR operation,
      VHO_FHE_RELU_VALUE_REF left, VHO_FHE_RELU_VALUE_REF right,
      double scalar, UINT32 depth,
      const VHO_FHE_RELU_VALUE_REF *add_to = NULL)
  {
    VHO_FHE_RELU_RECIPE_STEP step = {
      operation, left, right, scalar, _stage, depth
    };
    _recipe->steps.push_back(step);
    VHO_FHE_RELU_VALUE_REF result = {
      VHO_FHE_RELU_VALUE_PRIOR_STEP,
      (UINT32)_recipe->steps.size() - 1
    };
    return add_to == NULL ? result : Add(*add_to, result);
  }

  static VHO_FHE_RELU_VALUE_REF Invalid_Reference()
  {
    VHO_FHE_RELU_VALUE_REF invalid = { VHO_FHE_RELU_VALUE_INVALID, 0 };
    return invalid;
  }

  UINT32 Maximum_Depth(VHO_FHE_RELU_VALUE_REF left,
                       VHO_FHE_RELU_VALUE_REF right) const
  {
    UINT32 left_depth = Depth(left);
    UINT32 right_depth = Depth(right);
    return left_depth > right_depth ? left_depth : right_depth;
  }

  VHO_FHE_RELU_RECIPE *_recipe;
  UINT32 _stage;
  std::map<UINT32, VHO_FHE_RELU_VALUE_REF> _precomputed;
  std::vector<VHO_FHE_RELU_VALUE_REF> _powers_of_two;
};

}  // namespace

/* Build the pinned three-stage ACE algebraic recipe without mutating WHIRL. */
BOOL
VHO_FHE_CKKS_Build_Ace_Relu_Recipe(
    const std::vector<std::vector<double> > &coefficients,
    VHO_FHE_RELU_RECIPE *recipe, FILE *diagnostic)
{
  static const UINT32 expected_degrees[3] = { 7, 15, 13 };
  if (recipe == NULL || coefficients.size() != 3)
    return Report(diagnostic, "expected three coefficient stages");
  for (UINT32 stage = 0; stage < 3; ++stage) {
    if (coefficients[stage].size() != expected_degrees[stage] + 1)
      return Report(diagnostic, "coefficient degree does not match profile");
  }

  VHO_FHE_RELU_RECIPE candidate;
  RECIPE_BUILDER builder(&candidate);
  VHO_FHE_RELU_VALUE_REF original = {
    VHO_FHE_RELU_VALUE_ORIGINAL_INPUT, 0
  };
  VHO_FHE_RELU_VALUE_REF argument = {
    VHO_FHE_RELU_VALUE_NORMALIZED_INPUT, 0
  };
  for (UINT32 stage = 0; stage < 3; ++stage) {
    builder.Set_Stage(stage);
    UINT32 first = (UINT32)candidate.steps.size();
    UINT32 input_depth = builder.Depth(argument);
    argument = builder.Build_Stage(
        coefficients[stage], argument, original, stage == 2);
    if (argument.kind != VHO_FHE_RELU_VALUE_PRIOR_STEP)
      return Report(diagnostic, "polynomial stage produced no value");
    VHO_FHE_RELU_RECIPE_STAGE summary = {
      stage, expected_degrees[stage], first,
      (UINT32)candidate.steps.size() - first, argument,
      input_depth, builder.Depth(argument)
    };
    candidate.stages.push_back(summary);
  }
  builder.Set_Stage(3);
  VHO_FHE_RELU_VALUE_REF half = builder.Multiply_Scalar(original, 0.5);
  candidate.result = builder.Add(argument, half);
  if (candidate.result.kind != VHO_FHE_RELU_VALUE_PRIOR_STEP)
    return Report(diagnostic, "ReLU reconstruction produced no value");
  *recipe = candidate;
  return TRUE;
}
