/* Copyright (C) 2026 Open64 Project */

/* LNO-owned static rectangular partition-axis discovery using existing
 * REDUCTION_MANAGER, ACCESS_ARRAY, PREOPT DU and alias services.
 * Design: doc/AI-COMPILER-NATIVE-PARTITION-AXIS-v0.1.md.
 * This observes ordered sum/squared-sum regions; it does not infer LayerNorm,
 * mutate WHIRL, authorize reassociation or select a physical work partition. */
#include <string.h>
#include "dsl_partition_axis.h"
#include "pu_info.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "targ_const.h"
#include "lnopt_main.h"
#include "lwn_util.h"
#include "access_vector.h"
#include "reduc.h"
#include "opt_du.h"
#include "opt_alias_interface.h"

/* Require exact local scalar identity; names and tensor annotations are unused. */
static BOOL
LNO_DSL_same_scalar(WN *reference, WN *definition)
{
  return reference != NULL && definition != NULL &&
      WN_operator(reference) == OPR_LDID &&
      WN_operator(definition) == OPR_STID &&
      WN_st_idx(reference) == WN_st_idx(definition) &&
      WN_offset(reference) == WN_offset(definition) &&
      WN_rtype(reference) == MTYPE_F4 && WN_desc(definition) == MTYPE_F4;
}

/* Reuse canonical loop queries, not TripCount (which allocates/folds trees).
 * Zero-based I4 ascending unit loops make cardinality/overflow checks exact. */
static BOOL
LNO_DSL_static_domain(WN *loop, UINT64 *count)
{
  if (loop == NULL || WN_operator(loop) != OPR_DO_LOOP) return FALSE;
  OPCODE comparison;
  BOOL increasing;
  WN *iv = WN_LOOP_InductionVariable(loop);
  WN *lower = WN_LOOP_LowerBound(loop);
  WN *upper = WN_LOOP_UpperBound(loop, &comparison);
  WN *step = WN_LOOP_Increment(loop, &increasing);
  if (iv == NULL || lower == NULL || upper == NULL || step == NULL ||
      WN_operator(lower) != OPR_INTCONST || WN_const_val(lower) != 0 ||
      WN_operator(upper) != OPR_INTCONST || WN_operator(step) != OPR_INTCONST ||
      !increasing || WN_const_val(step) != 1 ||
      WN_desc(WN_start(loop)) != MTYPE_I4 ||
      OPCODE_desc(comparison) != MTYPE_I4) return FALSE;
  INT64 extent = WN_const_val(upper);
  if (OPCODE_operator(comparison) == OPR_LE) {
    if (extent < 0 || extent >= 2147483646LL) return FALSE;
    ++extent;
  } else if (OPCODE_operator(comparison) != OPR_LT) return FALSE;
  if (extent <= 0 || extent >= 2147483647LL) return FALSE;
  *count = (UINT64)extent;
  return TRUE;
}

/* The bounded region contains only init, contribution loop and final store.
 * Comments are nonsemantic; any branch/call/extra store remains a refusal. */
static WN *
LNO_DSL_next_statement(WN *statement)
{
  while (statement != NULL && WN_operator(statement) == OPR_COMMENT)
    statement = WN_next(statement);
  return statement;
}

/* Prove the reaching definitions and nonescaping uses from PREOPT's chains;
 * incomplete information is not repaired by structural resemblance. */
static BOOL
LNO_DSL_recurrence_du(const LNO_DSL_PARTITION_FACT &fact, DU_MANAGER *du)
{
  WN *uses[2] = { fact.self, fact.final_use };
  WN *defs[2] = { fact.init, fact.update };
  for (INT i = 0; i < 2; ++i) {
    DEF_LIST *list = du->Ud_Get_Def(uses[i]);
    if (list == NULL || list->Incomplete() || list->Is_Empty()) return FALSE;
    DEF_LIST_ITER iter(list);
    BOOL update_seen = FALSE;
    for (DU_NODE *node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
      if (node->Wn() != fact.init && node->Wn() != fact.update) return FALSE;
      update_seen = update_seen || node->Wn() == fact.update;
    }
    if (!update_seen) return FALSE;
  }
  for (INT i = 0; i < 2; ++i) {
    USE_LIST *list = du->Du_Get_Use(defs[i]);
    if (list == NULL || list->Incomplete() || list->Is_Empty()) return FALSE;
    USE_LIST_ITER iter(list);
    for (DU_NODE *node = iter.First(); !iter.Is_Empty(); node = iter.Next())
      if (node->Wn() != fact.self && node->Wn() != fact.final_use) return FALSE;
  }
  return TRUE;
}

/* Follow exactly one pure local contributor temporary through existing DU.
 * This observes a source edge, not a substitution/canonicalization rewrite. */
static BOOL
LNO_DSL_contributor_du(WN *use, WN *definition, DU_MANAGER *du)
{
  if (!LNO_DSL_same_scalar(use, definition) ||
      TY_is_volatile(WN_ty(definition)) ||
      ST_addr_saved(WN_st(definition)) || ST_addr_passed(WN_st(definition)) ||
      (ST_class(WN_st(definition)) != CLASS_PREG &&
       ST_sclass(WN_st(definition)) != SCLASS_AUTO)) return FALSE;
  DEF_LIST *defs = du->Ud_Get_Def(use);
  USE_LIST *uses = du->Du_Get_Use(definition);
  if (defs == NULL || uses == NULL || defs->Incomplete() || uses->Incomplete())
    return FALSE;
  DEF_LIST_ITER d_iter(defs);
  USE_LIST_ITER u_iter(uses);
  DU_NODE *d = d_iter.First(), *u = u_iter.First();
  return !d_iter.Is_Empty() && !u_iter.Is_Empty() &&
      d->Wn() == definition && u->Wn() == use &&
      d_iter.Next() == NULL && u_iter.Next() == NULL;
}

/* Read an exact loop-to-array-dimension relation from LNO's access vectors.
 * Reject offsets, symbolic/nonlinear terms, skew and outer-loop dependence. */
static BOOL
LNO_DSL_axis_vector(ACCESS_VECTOR *vector, UINT depth)
{
  if (vector == NULL || vector->Too_Messy || vector->Const_Offset != 0 ||
      vector->Contains_Lin_Symb() || vector->Contains_Non_Lin_Symb() ||
      vector->Loop_Coeff(depth) != 1) return FALSE;
  for (UINT i = 0; i < vector->Nest_Depth(); ++i)
    if (i != depth && vector->Loop_Coeff(i) != 0) return FALSE;
  return TRUE;
}

/* Validate address shape and dimension extents without inferring contiguity
 * or allocation capacity from the selected reduction width. */
static BOOL
LNO_DSL_array_shape(WN *array, INT rank)
{
  if (array == NULL || WN_operator(array) != OPR_ARRAY ||
      WN_num_dim(array) != rank || WN_element_size(array) != 4) return FALSE;
  for (INT i = 0; i < rank; ++i)
    if (WN_operator(WN_array_dim(array, i)) != OPR_INTCONST ||
        WN_const_val(WN_array_dim(array, i)) <= 0) return FALSE;
  return TRUE;
}

/* Match the two distinct logical dimensions, including X[K,I]. The scalar
 * scopes define I/K; access vectors establish their logical tensor axes. */
static BOOL
LNO_DSL_access_axes(LNO_DSL_PARTITION_FACT *fact)
{
  WN *array = WN_kid0(fact->input);
  WN *result = WN_kid1(fact->output);
  if (!LNO_DSL_array_shape(array, 2) || !LNO_DSL_array_shape(result, 1))
    return FALSE;
  ACCESS_ARRAY *access = (ACCESS_ARRAY *)WN_MAP_Get(LNO_Info_Map, array);
  ACCESS_ARRAY *out = (ACCESS_ARRAY *)WN_MAP_Get(LNO_Info_Map, result);
  if (access == NULL || out == NULL || access->Too_Messy || out->Too_Messy ||
      access->Num_Vec() != 2 || out->Num_Vec() != 1) return FALSE;
  UINT instance_depth = Get_Do_Loop_Info(fact->outer)->Depth;
  UINT contribution_depth = Get_Do_Loop_Info(fact->inner)->Depth;
  if (!LNO_DSL_axis_vector(out->Dim(0), instance_depth) ||
      (UINT64)WN_const_val(WN_array_dim(result, 0)) != fact->instances)
    return FALSE;
  for (INT i = 0; i < 2; ++i) {
    INT k = 1 - i;
    if (LNO_DSL_axis_vector(access->Dim(i), instance_depth) &&
        LNO_DSL_axis_vector(access->Dim(k), contribution_depth) &&
        (UINT64)WN_const_val(WN_array_dim(array, i)) == fact->instances &&
        (UINT64)WN_const_val(WN_array_dim(array, k)) == fact->contributions) {
      fact->instance_axis = i;
      fact->contribution_axis = k;
      return TRUE;
    }
  }
  return FALSE;
}

/* Observe one complete ordered recurrence. A refusal retains no partial
 * parallelization permission and never changes the input tree or FP options. */
static LNO_DSL_PARTITION_FACT
LNO_DSL_partition_fact(WN *outer, REDUCTION_MANAGER *reductions,
                       DU_MANAGER *du, ALIAS_MANAGER *alias)
{
  LNO_DSL_PARTITION_FACT f;
  memset(&f, 0, sizeof(f));
  f.outer = outer;
  f.instance_axis = f.contribution_axis = -1;
  f.recipe = "unknown";
  f.status = "region_unsupported";
  WN *body = WN_do_body(outer);
  f.init = LNO_DSL_next_statement(WN_first(body));
  f.inner = f.init == NULL ? NULL : LNO_DSL_next_statement(WN_next(f.init));
  f.output = f.inner == NULL ? NULL : LNO_DSL_next_statement(WN_next(f.inner));
  if (f.init == NULL || WN_operator(f.init) != OPR_STID ||
      f.inner == NULL || WN_operator(f.inner) != OPR_DO_LOOP ||
      f.output == NULL || WN_operator(f.output) != OPR_ISTORE ||
      LNO_DSL_next_statement(WN_next(f.output)) != NULL) return f;
  f.status = "domain_unproved";
  if (!LNO_DSL_static_domain(outer, &f.instances) ||
      !LNO_DSL_static_domain(f.inner, &f.contributions)) return f;
  if (WN_st_idx(WN_index(outer)) == WN_st_idx(WN_index(f.inner)) &&
      WN_offset(WN_index(outer)) == WN_offset(WN_index(f.inner))) return f;
  f.status = "recurrence_unproved";
  f.update = LNO_DSL_next_statement(WN_first(WN_do_body(f.inner)));
  if (f.update != NULL && WN_operator(f.update) == OPR_STID) {
    WN *next = LNO_DSL_next_statement(WN_next(f.update));
    if (next != NULL && WN_operator(next) == OPR_STID) {
      f.contributor_definition = f.update;
      f.update = next;
    }
  }
  if (f.update == NULL || WN_operator(f.update) != OPR_STID ||
      LNO_DSL_next_statement(WN_next(f.update)) != NULL ||
      reductions->Which_Reduction(f.update) != RED_ADD ||
      WN_st_idx(f.init) != WN_st_idx(f.update) ||
      WN_offset(f.init) != WN_offset(f.update) ||
      WN_desc(f.init) != MTYPE_F4 || WN_desc(f.update) != MTYPE_F4 ||
      WN_operator(WN_kid0(f.update)) != OPR_ADD ||
      WN_rtype(WN_kid0(f.update)) != MTYPE_F4 ||
      TY_is_volatile(WN_ty(f.init)) ||
      (ST_class(WN_st(f.init)) != CLASS_PREG &&
       ST_sclass(WN_st(f.init)) != SCLASS_AUTO) ||
      ST_addr_saved(WN_st(f.init)) || ST_addr_passed(WN_st(f.init))) return f;
  WN *identity = WN_kid0(f.init);
  if (WN_operator(identity) != OPR_CONST || WN_rtype(identity) != MTYPE_F4 ||
      !Targ_Is_Zero(STC_val(WN_st(identity))) ||
      Targ_Is_Neg_Zero(STC_val(WN_st(identity)))) return f;
  WN *add = WN_kid0(f.update);
  f.self_operand = LNO_DSL_same_scalar(WN_kid0(add), f.update) ? 0 : 1;
  f.self = WN_kid(add, f.self_operand);
  WN *contribution = WN_kid(add, 1 - f.self_operand);
  f.final_use = WN_kid0(f.output);
  if (!LNO_DSL_same_scalar(f.self, f.update) ||
      !LNO_DSL_same_scalar(f.final_use, f.update) ||
      WN_desc(f.output) != MTYPE_F4) return f;
  if (f.contributor_definition != NULL) {
    f.status = "def_use_unproved";
    if (!LNO_DSL_contributor_du(contribution, f.contributor_definition, du)) return f;
    contribution = WN_kid0(f.contributor_definition);
    f.status = "recurrence_unproved";
  }
  if (WN_operator(contribution) == OPR_MPY &&
      WN_rtype(contribution) == MTYPE_F4) {
    f.input = WN_kid0(contribution);
    f.second_input = WN_kid1(contribution);
    f.recipe = "ordered_squared_sum";
  } else {
    f.input = contribution;
    f.recipe = "ordered_sum";
  }
  if (WN_operator(f.input) != OPR_ILOAD || WN_rtype(f.input) != MTYPE_F4 ||
      WN_desc(f.input) != MTYPE_F4 || WN_offset(f.input) != 0 ||
      WN_offset(f.output) != 0 ||
      WN_Is_Volatile_Mem(f.input) || WN_Is_Volatile_Mem(f.output) ||
      (f.second_input != NULL &&
       WN_Simp_Compare_Trees(f.input, f.second_input) != 0))
    return f;
  f.status = "def_use_unproved";
  if (!LNO_DSL_recurrence_du(f, du)) return f;
  f.status = "access_unproved";
  if (!LNO_DSL_access_axes(&f)) return f;
  f.status = "alias_unproved";
  if (!Valid_alias(alias, f.input) || !Valid_alias(alias, f.output) ||
      Aliased(alias, f.input, f.output) != NOT_ALIASED) return f;
  f.status = "proved";
  return f;
}

/* Traverse only this active PU, reporting each outer nest once and source
 * line last. Refused facts do not leak axes/cardinalities as proved values. */
static UINT32
LNO_DSL_partition_walk(WN *tree, REDUCTION_MANAGER *reductions,
                       DU_MANAGER *du, ALIAS_MANAGER *alias, FILE *trace,
                       UINT32 *regions, LNO_DSL_PARTITION_VISITOR visitor,
                       void *context)
{
  UINT32 proved = 0;
  if (tree == NULL) return 0;
  if (WN_operator(tree) == OPR_DO_LOOP) {
    BOOL nested = FALSE;
    for (WN *stmt = WN_first(WN_do_body(tree)); stmt != NULL; stmt = WN_next(stmt))
      nested = nested || WN_operator(stmt) == OPR_DO_LOOP;
    if (nested) {
      LNO_DSL_PARTITION_FACT f = LNO_DSL_partition_fact(tree, reductions, du, alias);
      BOOL valid = strcmp(f.status, "proved") == 0;
      ++*regions;
      if (visitor != NULL) visitor(&f, context);
      if (trace != NULL) {
        fprintf(trace, "AI_PARTITION 1 FACT lno_discovery %u %u %s ",
            (UINT32)PU_Info_proc_sym(Current_PU_Info), *regions, f.status);
        if (valid) fprintf(trace, "%u %lld %u %lld %d %d %llu %llu %s %d ordered ",
            (UINT32)WN_st_idx(WN_index(f.outer)),
            (long long)WN_offset(WN_index(f.outer)),
            (UINT32)WN_st_idx(WN_index(f.inner)),
            (long long)WN_offset(WN_index(f.inner)),
            f.instance_axis, f.contribution_axis,
            (unsigned long long)f.instances, (unsigned long long)f.contributions,
            f.recipe, f.self_operand);
        else fprintf(trace, "- - - - - - - - %s - unknown ", f.recipe);
        SRCPOS position = WN_Get_Linenum(tree);
        fprintf(trace, "%u\n", (UINT32)SRCPOS_linenum(position));
      }
      return valid ? 1 : 0;
    }
  }
  if (WN_operator(tree) == OPR_BLOCK) {
    for (WN *stmt = WN_first(tree); stmt != NULL; stmt = WN_next(stmt))
      proved += LNO_DSL_partition_walk(stmt, reductions, du, alias, trace, regions,
                                       visitor, context);
  } else {
    for (INT i = 0; i < WN_kid_count(tree); ++i)
      proved += LNO_DSL_partition_walk(WN_kid(tree, i), reductions, du, alias, trace,
                                       regions, visitor, context);
  }
  return proved;
}

/* Analyze under driver-owned LNO lifetimes. The private reduction map is
 * deliberately independent of reassociation-enabled global red_manager. */
UINT32
LNO_DSL_discover_partition_axes(WN *scope, DU_MANAGER *du,
                               ALIAS_MANAGER *alias, FILE *trace,
                               LNO_DSL_PARTITION_VISITOR visitor, void *context)
{
  if (scope == NULL || du == NULL || alias == NULL ||
      WN_operator(scope) != OPR_FUNC_ENTRY || Current_PU_Info == NULL ||
      PU_Info_tree_ptr(Current_PU_Info) != scope) return 0;
  REDUCTION_MANAGER reductions(&LNO_default_pool);
  reductions.Build(scope, TRUE, FALSE);
  if (trace != NULL) fprintf(trace,
      "AI_PARTITION 1 HEADER phase pu region status instance_iv instance_offset "
      "contribution_iv contribution_offset instance_axis contribution_axis instances "
      "contributions recipe self_operand order source_line\n");
  UINT32 regions = 0;
  UINT32 proved = LNO_DSL_partition_walk(scope, &reductions, du, alias, trace,
                                         &regions, visitor, context);
  if (trace != NULL) fprintf(trace, "AI_PARTITION 1 SUMMARY %u %u %u %u\n",
      (UINT32)PU_Info_proc_sym(Current_PU_Info), regions, proved, regions - proved);
  return proved;
}
