/* Copyright (C) 2026 Open64 Project */
/* Test-only live-fact perturbations at the actual LNO snapshot boundary.
 * Design: doc/AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md.
 * Included only with LNO_DSL_HIGH_SNAPSHOT_TEST; restore before legacy LNO. */

/* Fail the test component immediately; emit individually reviewable evidence. */
static void DSL_high_test_require(bool result, const char *name) {
    FmtAssert(result, ("High snapshot test failed: %s", name));
    fprintf(TFile, "AI_HIGH_SNAPSHOT_TEST PASS %s\n", name);
}
/* Find real admitted nodes without constructing mock analyses or new scope. */
static void DSL_high_test_nodes(WN *node, WN *&loop, WN *&memory, WN *&use, DU_MANAGER *du) {
    if (WN_operator(node) == OPR_DO_LOOP && !loop) loop = node;
    if ((WN_operator(node) == OPR_ILOAD || WN_operator(node) == OPR_ISTORE) && !memory) memory = node;
    if (WN_operator(node) == OPR_LDID && !use) {
        DEF_LIST *list = du->Ud_Get_Def(node);
        if (list && !list->Incomplete()) use = node;
    }
    if (WN_operator(node) == OPR_BLOCK) {
        for (WN *statement = WN_first(node); statement; statement = WN_next(statement))
            DSL_high_test_nodes(statement, loop, memory, use, du);
    } else for (INT i = 0; i < WN_kid_count(node); ++i)
        if (WN_kid(node, i)) DSL_high_test_nodes(WN_kid(node, i), loop, memory, use, du);
}
/* Exercise real source/analysis changes and restore each before assertion or
 * continuation. Revalidation uses complete keys, not only the mutation token. */
static void DSL_high_native_test(WN *scope, DU_MANAGER *du, ALIAS_MANAGER *alias,
    ARRAY_DIRECTED_GRAPH16 *graph, const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_BINDINGS &bindings, const DSL_HIGH_SNAPSHOT &snapshot) {
    std::string reason;
    DSL_high_test_require(snapshot.Source_key().find_first_of("\n\r") == std::string::npos &&
        snapshot.Source_key().find('\0') == std::string::npos &&
        snapshot.Facts_key().find_first_of("\n\r") == std::string::npos &&
        snapshot.Facts_key().find('\0') == std::string::npos, "numeric_type_codes_are_text");
    DSL_high_test_require(LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason), "unchanged_live_facts");
    DSL_HIGH_SNAPSHOT repeat;
    bool captured = LNO_DSL_capture_high(scope, du, alias, graph, epoch, bindings, repeat, reason);
    DSL_high_test_require(captured && snapshot.Same_content(repeat) && snapshot.Identity() == repeat.Identity(), "repeat_identity_and_content");
    WN *loop = NULL, *memory = NULL, *use = NULL;
    DSL_high_test_nodes(WN_func_body(scope), loop, memory, use, du);
    DSL_high_test_require(loop && memory && use, "live_test_coverage");

    OPCODE comparison; WN *upper = WN_LOOP_UpperBound(loop, &comparison);
    INT64 old_bound = WN_const_val(upper); WN_const_val(upper) = old_bound + 1;
    bool rejected = !LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason);
    WN_const_val(upper) = old_bound;
    DSL_high_test_require(rejected, "unannounced_loop_bound_edit");

    WN *address = WN_operator(memory) == OPR_ILOAD ? WN_kid0(memory) : WN_kid1(memory);
    ACCESS_VECTOR *access = ((ACCESS_ARRAY *)WN_MAP_Get(LNO_Info_Map, address))->Dim(0);
    INT64 old_offset = access->Const_Offset; ++access->Const_Offset;
    rejected = !LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason);
    access->Const_Offset = old_offset;
    DSL_high_test_require(rejected, "unannounced_affine_access_edit");

    EINDEX16 edge = graph->Get_Edge();
    while (edge && (!graph->Depv_Array(edge)->Num_Vec() || !graph->Depv_Array(edge)->Num_Dim())) edge = graph->Get_Next_Edge(edge);
    if (edge) {
        DEPV_ARRAY *vectors = graph->Depv_Array(edge);
        DEP &component = DEPV_Dep(vectors->Depv(0), 0); DEP old_component = component;
        component = DEP_IsDistance(component) ? DEP_SetDirection(DIR_STAR) : DEP_SetDistance(0);
        rejected = !LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason);
        component = old_component;
        DSL_high_test_require(rejected, "unannounced_dependence_edit");
    } else fprintf(TFile, "AI_HIGH_SNAPSHOT_TEST SKIP unannounced_dependence_edit reason=empty_graph_edges\n");

    DEF_LIST *definitions = du->Ud_Get_Def(use); definitions->Set_Incomplete();
    rejected = !LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason);
    definitions->Reset_Incomplete();
    DSL_high_test_require(rejected, "unannounced_DU_completeness_edit");

    ALIAS_CONTEXT old_context = alias->Pu_context(); alias->Set_pu_context(old_context ^ 1);
    rejected = !LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason);
    alias->Set_pu_context(old_context);
    DSL_high_test_require(rejected, "unannounced_alias_context_edit");

    IDTYPE old_id = alias->Id(memory); alias->Cross_dso_set_id(memory, 0);
    rejected = !LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason);
    alias->Cross_dso_set_id(memory, old_id);
    DSL_high_test_require(rejected, "unannounced_alias_answer_edit");

    IEEE_LEVEL old_ieee = IEEE_Arithmetic;
    IEEE_Arithmetic = (IEEE_LEVEL)(old_ieee == 0 ? 1 : 0);
    DSL_HIGH_BINDINGS changed = LNO_DSL_high_bindings(bindings.unit.c_str());
    rejected = !LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, changed, snapshot, reason);
    IEEE_Arithmetic = old_ieee;
    DSL_high_test_require(rejected, "effective_numerical_control_edit");
    changed = bindings; changed.target += "/changed";
    DSL_high_test_require(!LNO_DSL_revalidate_high(NULL, NULL, NULL, NULL, epoch, changed, snapshot, reason), "target_rebind_before_native_access");
    changed = bindings; changed.manifest = "different-source-manifest";
    DSL_high_test_require(!LNO_DSL_revalidate_high(NULL, NULL, NULL, NULL, epoch, changed, snapshot, reason), "manifest_rebind_before_native_access");
    changed = bindings; changed.model = "different-cost-model";
    DSL_high_test_require(!LNO_DSL_revalidate_high(NULL, NULL, NULL, NULL, epoch, changed, snapshot, reason), "model_rebind_before_native_access");

    /* Reposition an existing comment: allocating a new one would change the
     * global string table/map allocation sequence even after deleting the WN. */
    WN *body = WN_func_body(scope), *comment = WN_first(body);
    while (comment && WN_operator(comment) != OPR_COMMENT) comment = WN_next(comment);
    if (comment) {
        WN *old_next = WN_next(comment);
        bool was_first = comment == WN_first(body);
        WN_EXTRACT_FromBlock(body, comment);
        WN_INSERT_BlockBefore(body, was_first ? NULL : WN_first(body), comment);
        captured = LNO_DSL_capture_high(scope, du, alias, graph, epoch, bindings, repeat, reason);
        WN_EXTRACT_FromBlock(body, comment); WN_INSERT_BlockBefore(body, old_next, comment);
        DSL_high_test_require(captured && snapshot.Same_content(repeat), "comment_does_not_renumber_identity");
    } else fprintf(TFile, "AI_HIGH_SNAPSHOT_TEST SKIP comment_does_not_renumber_identity reason=no_comment\n");

    SRCPOS old_source = WN_linenum(loop); WN_Set_Linenum(loop, old_source + 1);
    captured = LNO_DSL_capture_high(scope, du, alias, graph, epoch, bindings, repeat, reason);
    WN_Set_Linenum(loop, old_source);
    DSL_high_test_require(captured && snapshot.Same_content(repeat), "source_position_does_not_renumber_identity");

    DSL_HIGH_EPOCH another_activation(epoch.Stamp().owner);
    DSL_high_test_require(!LNO_DSL_revalidate_high(NULL, NULL, NULL, NULL, another_activation, bindings, snapshot, reason), "same_owner_new_activation_rejected");
    DSL_HIGH_EPOCH wrong_owner("different-pu");
    DSL_high_test_require(!LNO_DSL_revalidate_high(NULL, NULL, NULL, NULL, wrong_owner, bindings, snapshot, reason), "cross_PU_rejected");

    DSL_HIGH_EPOCH mutation_epoch(epoch.Stamp().owner);
    captured = LNO_DSL_capture_high(scope, du, alias, graph, mutation_epoch, bindings, repeat, reason);
    mutation_epoch.Before_mutation();
    DSL_high_test_require(captured && !LNO_DSL_revalidate_high(NULL, NULL, NULL, NULL, mutation_epoch, bindings, repeat, reason), "mutation_epoch_before_native_access");
    captured = LNO_DSL_capture_high(scope, du, alias, graph, mutation_epoch, bindings, repeat, reason);
    mutation_epoch.Close();
    DSL_high_test_require(captured && !LNO_DSL_revalidate_high(NULL, NULL, NULL, NULL, mutation_epoch, bindings, repeat, reason), "closed_epoch_before_native_access");

    DSL_HIGH_BINDINGS absent = bindings; absent.model.clear(); repeat = snapshot;
    bool refused = !LNO_DSL_capture_high(scope, du, alias, graph, epoch, absent, repeat, reason);
    DSL_high_test_require(refused && !repeat.Is_current(epoch, bindings) && repeat.Loops().empty(), "refusal_clears_previous_output");
    DSL_high_test_require(LNO_DSL_revalidate_high(scope, du, alias, graph, epoch, bindings, snapshot, reason), "all_live_edits_restored");
}
