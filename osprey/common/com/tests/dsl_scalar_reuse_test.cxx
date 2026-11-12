/* Native scalar-operation reuse fixture, owned by the common/com tests.
 * Exercise the real WN engine and fail-closed tensor/FP boundaries without
 * a parallel arithmetic evaluator. Retained core-WHIRL before/after images
 * demonstrate eligible rewrites; tensor wrappers keep their existing .B IDs.
 * Design: doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_simp.h"
#include "wn_util.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "glob.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_opt.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "dwarf_DST_mem.h"
#include "srcpos.h"
#include "const.h"
#include "dsl_builder.h"
#include "dsl_simp.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

/* Satisfy the linked compiler's signal hook; this standalone fixture owns no
 * driver-managed output that needs signal cleanup. */
void
Signal_Cleanup (INT sig)
{
}

/* Satisfy the diagnostic formatter hook; this fixture emits no host-specific
 * error parameters and returns borrowed empty text. */
const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
    return "";
}

/* Initialize compiler pools, errors, target controls, tables and DST before
 * any builder or simplifier API runs; the process owns their test lifetime. */
static void
DSL_scalar_test_initialize (void)
{
    MEM_Initialize();
    Set_Error_Tables(Phases, host_errlist);
    Init_Error_Handler(10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure();
    Init_Controls_Tbl();
    ABI_Name = "n64";
    Configure();
    IR_reader_init();
    Initialize_Symbol_Tables(TRUE);
    DST_Init(NULL, 0);
}

/* Build an exact binary32 CONST through the existing target parser and table.
 * The caller supplies eight hexadecimal digits and retains the returned WN. */
static WN *
DSL_scalar_test_constant (const char *bits)
{
    TCON scalar = Targ_Hexfptoc(MTYPE_F4, bits);
    return WN_CreateConst(OPR_CONST, MTYPE_F4, MTYPE_V,
                          New_Const_Sym(Enter_tcon(scalar), MTYPE_To_TY(MTYPE_F4)));
}

/* Create a caller-PU-local scalar value slot with the fixture source position;
 * no tensor identity or optimization fact is attached to this ordinary ST. */
static ST *
DSL_scalar_test_symbol (const char *name, TYPE_ID mtype, SRCPOS position)
{
    ST *symbol = New_ST(CURRENT_SYMTAB);
    ST_Init(symbol, Save_Str(name), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
            MTYPE_To_TY(mtype));
    Set_ST_Srcpos(*symbol, position);
    return symbol;
}

/* Retain a conventional expression, optionally replacing disposable leaves
 * through the real bridge. The before form is built without construction-time
 * WN simplification; ownership transfers once into an ordinary STID. */
static BOOL
DSL_scalar_test_append (PU_Info *pu, const char *name,
                         DSL_SIMP_SCALAR_CANDIDATE *candidate,
                         SRCPOS position, BOOL apply,
                         DSL_SIMP_STATUS expected_status)
{
    DSL_SCALAR_OPERATION operation;
    if (!DSL_scalar_operation(candidate->dsl_operator, candidate->version,
                               candidate->element_mtype, &operation))
        return FALSE;
    WN *expression = NULL;
    if (apply) {
        DSL_SIMP_SCALAR_RESULT result;
        DSL_SIMP_STATUS status = DSL_simp_scalar(candidate, &result);
        if (status != expected_status ||
            (status < DSL_SIMP_REJECT_DISABLED &&
             result.projected_opcode != operation.opcode))
            return FALSE;
        expression = result.engine_result;
    }
    if (expression == NULL) {
        expression = WN_Create(OPCODE_operator(operation.opcode),
            OPCODE_rtype(operation.opcode), OPCODE_desc(operation.opcode),
            operation.arity);
        for (UINT32 index = 0; index < operation.arity; ++index)
            WN_kid(expression, index) = candidate->projected_kid[index];
    }
    TYPE_ID mtype = OPCODE_rtype(operation.opcode);
    ST *symbol = DSL_scalar_test_symbol(name, mtype, position);
    WN *statement = WN_Stid(mtype, 0, symbol, ST_type(symbol), expression);
    WN_Set_Linenum(statement, position);
    WN_INSERT_BlockLast(WN_func_body(PU_Info_tree_ptr(pu)), statement);
    return TRUE;
}

/* Check fail-closed admission without transferring the supplied trees or
 * changing compiler controls. The caller retains and destroys every leaf. */
static BOOL
DSL_scalar_test_rejection (DSL_SIMP_SCALAR_CANDIDATE *candidate,
                           DSL_SIMP_STATUS expected)
{
    IEEE_LEVEL ieee = IEEE_Arithmetic;
    ROUNDOFF roundoff = Roundoff_Level;
    BOOL reassociate = Enable_Cfold_Reassociate;
    BOOL aggressive = Enable_Cfold_Aggressive;
    BOOL div_split = Div_Split_Allowed;
    BOOL comparisons = Force_IEEE_Comparisons;
    BOOL enabled = Enable_WN_Simp;
    DSL_SIMP_SCALAR_RESULT result;
    return DSL_simp_scalar(candidate, &result) == expected &&
           result.engine_result == NULL && IEEE_Arithmetic == ieee &&
           Roundoff_Level == roundoff && Enable_Cfold_Reassociate == reassociate &&
           Enable_Cfold_Aggressive == aggressive && Div_Split_Allowed == div_split &&
           Force_IEEE_Comparisons == comparisons && Enable_WN_Simp == enabled;
}

/* Validate mapping, real engine dispatch, disabled/malformed admission, FP
 * bit preservation. Tensor broadcasting is checked independently by native
 * shape/preflight tests. Produce one ordinary-WHIRL image when
 * requested; only the APPLY variant simplifies its scalar expression leaves.
 * This is focused infrastructure evidence, not tensor lowering or execution. */
static int
DSL_scalar_reuse_contract (void)
{
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    PU_Info *pu = DSL_Builder_Create_Minimal_PU("scalar_operation_reuse");
    UINT32 file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    USRCPOS source;
    USRCPOS_clear(source);
    USRCPOS_filenum(source) = file_id;
    USRCPOS_linenum(source) = __LINE__;
    USRCPOS_column(source) = 1;
    SRCPOS position = USRCPOS_srcpos(source);
    Set_ST_Srcpos(St_Table[PU_Info_proc_sym(pu)], position);
    WN_Set_Linenum(PU_Info_tree_ptr(pu), position);
    BOOL apply = getenv("OPEN64_DSL_SCALAR_REUSE_APPLY") != NULL;
    BOOL disabled_image = getenv("OPEN64_DSL_SCALAR_REUSE_DISABLED_IMAGE") != NULL;
    const DSL_OPERATOR operations[] = {
        OPR_DSLSUB, OPR_DSLGREATER, OPR_DSLLOGICALNOT, OPR_DSLSELECT
    };
    const OPERATOR expected[] = { OPR_SUB, OPR_GT, OPR_LNOT, OPR_SELECT };
    const char *names[] = { "sub_result", "greater_result", "not_result", "select_result" };
    const TYPE_ID mtypes[] = { MTYPE_I4, MTYPE_I8, MTYPE_U4, MTYPE_U8, MTYPE_F4, MTYPE_F8 };
    for (UINT32 index = 0; index < 6; ++index) {
        DSL_SCALAR_OPERATION operation;
        if (!DSL_scalar_operation(OPR_DSLGREATER, 1, mtypes[index], &operation) ||
            OPCODE_operator(operation.opcode) != OPR_GT ||
            OPCODE_rtype(operation.opcode) != MTYPE_I4 ||
            OPCODE_desc(operation.opcode) != mtypes[index])
            return 1;
    }
    DSL_SCALAR_OPERATION double_intrinsic;
    if (!DSL_scalar_operation(OPR_DSLEXP, 1, MTYPE_F8, &double_intrinsic) ||
        double_intrinsic.intrinsic != INTRN_F8EXP ||
        !DSL_scalar_operation(OPR_DSLLOG, 1, MTYPE_F8, &double_intrinsic) ||
        double_intrinsic.intrinsic != INTRN_F8LOG ||
        !DSL_scalar_operation(OPR_DSLISFINITE, 1, MTYPE_F8, &double_intrinsic) ||
        double_intrinsic.intrinsic != INTRN_F8FINITE)
        return 1;
    for (UINT32 index = 0; index < 4; ++index) {
        DSL_SIMP_SCALAR_CANDIDATE candidate;
        memset(&candidate, 0, sizeof(candidate));
        candidate.dsl_operator = operations[index];
        candidate.version = 1;
        candidate.element_mtype = MTYPE_I4;
        DSL_SCALAR_OPERATION operation;
        if (!DSL_scalar_operation(operations[index], 1, MTYPE_I4, &operation) ||
            OPCODE_operator(operation.opcode) != expected[index])
            return 1;
        candidate.kid_count = operation.arity;
        candidate.projected_kid[0] = WN_Intconst(MTYPE_I4, index < 2 ? 7 : 1);
        if (operation.arity > 1)
            candidate.projected_kid[1] = WN_Intconst(MTYPE_I4, index == 0 ? 0 : 3);
        if (operation.arity > 2)
            candidate.projected_kid[2] = WN_Intconst(MTYPE_I4, 9);
        BOOL saved_enabled = Enable_WN_Simp;
        if (disabled_image)
            Enable_WN_Simp = FALSE;
        BOOL appended = DSL_scalar_test_append(pu, names[index], &candidate, position,
            apply, disabled_image ? DSL_SIMP_REJECT_DISABLED : DSL_SIMP_ENGINE_REWRITE);
        Enable_WN_Simp = saved_enabled;
        if (!appended)
            return 1;
        if (apply && !disabled_image) {
            const INT64 values[] = { 7, 1, 0, 3 };
            WN *value = WN_kid0(WN_last(WN_func_body(PU_Info_tree_ptr(pu))));
            if (WN_operator(value) != OPR_INTCONST ||
                WN_const_val(value) != values[index])
                return 1;
        }
    }

    IEEE_LEVEL saved_ieee = IEEE_Arithmetic;
    ROUNDOFF saved_roundoff = Roundoff_Level;
    BOOL saved_reassociate = Enable_Cfold_Reassociate;
    BOOL saved_aggressive = Enable_Cfold_Aggressive;
    BOOL saved_div_split = Div_Split_Allowed;
    BOOL saved_comparisons = Force_IEEE_Comparisons;
    IEEE_Arithmetic = IEEE_ACCURATE;
    Roundoff_Level = ROUNDOFF_NONE;
    Enable_Cfold_Reassociate = FALSE;
    Enable_Cfold_Aggressive = FALSE;
    Div_Split_Allowed = FALSE;
    Force_IEEE_Comparisons = TRUE;
    ST *runtime_value = DSL_scalar_test_symbol("runtime_float", MTYPE_F4, position);
    for (UINT32 index = 0; index < 2; ++index) {
        DSL_SIMP_SCALAR_CANDIDATE candidate;
        memset(&candidate, 0, sizeof(candidate));
        candidate.dsl_operator = operations[index];
        candidate.version = 1;
        candidate.element_mtype = MTYPE_F4;
        candidate.kid_count = 2;
        candidate.projected_kid[0] = WN_Ldid(MTYPE_F4, 0, runtime_value, ST_type(runtime_value));
        candidate.projected_kid[1] = WN_Ldid(MTYPE_F4, 0, runtime_value, ST_type(runtime_value));
        if (!DSL_scalar_test_append(pu, index == 0 ? "strict_sub" : "strict_gt",
                                    &candidate, position, apply, DSL_SIMP_UNCHANGED))
            return 1;
    }

    /* Retain exact strict-FP counterexamples in both mapped images. A scalar
     * identity must not silently change signed zero or erase NaN arithmetic. */
    for (UINT32 index = 0; index < 3; ++index) {
        DSL_SIMP_SCALAR_CANDIDATE candidate;
        memset(&candidate, 0, sizeof(candidate));
        candidate.dsl_operator = OPR_DSLSUB;
        candidate.version = 1;
        candidate.element_mtype = MTYPE_F4;
        candidate.kid_count = 2;
        WN *value = WN_Ldid(MTYPE_F4, 0, runtime_value, ST_type(runtime_value));
        if (index == 0) {
            candidate.projected_kid[0] = DSL_scalar_test_constant("00000000");
            candidate.projected_kid[1] = value;
        } else if (index == 1) {
            candidate.projected_kid[0] = value;
            candidate.projected_kid[1] = DSL_scalar_test_constant("80000000");
        } else {
            candidate.projected_kid[0] = WN_Create(OPR_NEG, MTYPE_F4, MTYPE_V, 1);
            WN_kid0(candidate.projected_kid[0]) = value;
            candidate.projected_kid[1] = WN_Ldid
                (MTYPE_F4, 0, runtime_value, ST_type(runtime_value));
        }
        const char *names[] = { "strict_zero_sub", "strict_sub_negative_zero", "strict_nested_sub" };
        if (!DSL_scalar_test_append(pu, names[index], &candidate, position, apply,
                index == 2 ? DSL_SIMP_REJECT_PROJECTION : DSL_SIMP_REJECT_NUMERIC_POLICY))
            return 1;
    }

    /* Even genuine nonconstant leaves cannot enter an engine configuration
     * that can commute FP operands or introduce reciprocal multiplication. */
    DSL_SIMP_SCALAR_CANDIDATE policy;
    memset(&policy, 0, sizeof(policy));
    policy.dsl_operator = OPR_DSLGREATER;
    policy.version = 1;
    policy.element_mtype = MTYPE_F4;
    policy.kid_count = 2;
    policy.projected_kid[0] = WN_Ldid(MTYPE_F4, 0, runtime_value, ST_type(runtime_value));
    policy.projected_kid[1] = WN_Ldid(MTYPE_F4, 0, runtime_value, ST_type(runtime_value));
    Enable_Cfold_Aggressive = TRUE;
    if (!DSL_scalar_test_rejection(&policy, DSL_SIMP_REJECT_NUMERIC_POLICY))
        return 1;
    Enable_Cfold_Aggressive = FALSE;
    policy.dsl_operator = OPR_DSLDIV;
    Div_Split_Allowed = TRUE;
    if (!DSL_scalar_test_rejection(&policy, DSL_SIMP_REJECT_NUMERIC_POLICY))
        return 1;
    Div_Split_Allowed = FALSE;
    WN_DELETE_Tree(policy.projected_kid[0]);
    WN_DELETE_Tree(policy.projected_kid[1]);

    const char *patterns[] = { "80000000", "7f800000", "ff800000", "7fc01234", "7f801234" };
    for (UINT32 index = 0; index < 10; ++index) {
        DSL_SIMP_SCALAR_CANDIDATE candidate;
        memset(&candidate, 0, sizeof(candidate));
        candidate.dsl_operator = OPR_DSLSELECT;
        candidate.version = 1;
        candidate.element_mtype = MTYPE_F4;
        candidate.kid_count = 3;
        BOOL condition = index < 5;
        candidate.projected_kid[0] = WN_Intconst(MTYPE_I4, condition);
        candidate.projected_kid[1] = DSL_scalar_test_constant
            (condition ? patterns[index % 5] : "00000000");
        candidate.projected_kid[2] = DSL_scalar_test_constant
            (condition ? "00000000" : patterns[index % 5]);
        WN *selected = candidate.projected_kid[condition ? 1 : 2];
        DSL_SIMP_SCALAR_RESULT result;
        if (DSL_simp_scalar(&candidate, &result) != DSL_SIMP_ENGINE_REWRITE ||
            result.engine_result != selected)
            return 1;
        WN_DELETE_Tree(result.engine_result);
    }

    DSL_SIMP_SCALAR_CANDIDATE rejected;
    memset(&rejected, 0, sizeof(rejected));
    rejected.dsl_operator = OPR_DSLSUB;
    rejected.version = 1;
    rejected.element_mtype = MTYPE_F4;
    rejected.kid_count = 2;
    rejected.projected_kid[0] = DSL_scalar_test_constant("00000000");
    rejected.projected_kid[1] = WN_LdidPreg(MTYPE_F4, 31);
    DSL_SIMP_SCALAR_RESULT result;
    if (DSL_simp_scalar(&rejected, &result) != DSL_SIMP_REJECT_NUMERIC_POLICY)
        return 1;
    WN_DELETE_Tree(rejected.projected_kid[0]);
    rejected.projected_kid[0] = WN_LdidPreg(MTYPE_F4, 32);
    if (DSL_simp_scalar(&rejected, &result) != DSL_SIMP_REJECT_NUMERIC_POLICY)
        return 1;
    WN_DELETE_Tree(rejected.projected_kid[0]);
    WN_DELETE_Tree(rejected.projected_kid[1]);

    const DSL_OPERATOR intrinsic_wrappers[] = { OPR_DSLEXP, OPR_DSLLOG, OPR_DSLISFINITE };
    const INTRINSIC intrinsics[] = { INTRN_F4EXP, INTRN_F4LOG, INTRN_F4FINITE };
    for (UINT32 index = 0; index < 3; ++index) {
        DSL_SCALAR_OPERATION operation;
        if (!DSL_scalar_operation(intrinsic_wrappers[index], 1, MTYPE_F4, &operation) ||
            operation.intrinsic != intrinsics[index] || !operation.requires_fp_binding)
            return 1;
        rejected.dsl_operator = intrinsic_wrappers[index];
        rejected.kid_count = 1;
        rejected.projected_kid[0] = DSL_scalar_test_constant("3f800000");
        if (DSL_simp_scalar(&rejected, &result) != DSL_SIMP_REJECT_NUMERIC_POLICY)
            return 1;
        WN_DELETE_Tree(rejected.projected_kid[0]);
    }
    rejected.dsl_operator = OPR_DSLLOGICALNOT;
    rejected.element_mtype = MTYPE_I4;
    rejected.projected_kid[0] = WN_Intconst(MTYPE_I4, 1);
    Enable_WN_Simp = FALSE;
    DSL_SIMP_STATUS disabled = DSL_simp_scalar(&rejected, &result);
    Enable_WN_Simp = TRUE;
    if (disabled != DSL_SIMP_REJECT_DISABLED)
        return 1;
    WN_DELETE_Tree(rejected.projected_kid[0]);

    /* Malformed schemas, noncanonical predicates and shared trees must fail
     * before the consuming engine is called. */
    rejected.dsl_operator = OPR_DSLLOGICALNOT;
    rejected.version = 1;
    rejected.element_mtype = MTYPE_I4;
    rejected.kid_count = 1;
    rejected.projected_kid[0] = WN_Intconst(MTYPE_I4, 2);
    if (!DSL_scalar_test_rejection(&rejected, DSL_SIMP_REJECT_PROJECTION))
        return 1;
    rejected.kid_count = 2;
    if (!DSL_scalar_test_rejection(&rejected, DSL_SIMP_REJECT_MALFORMED))
        return 1;
    rejected.kid_count = 1;
    rejected.version = 2;
    if (!DSL_scalar_test_rejection(&rejected, DSL_SIMP_REJECT_UNSUPPORTED_OPERATOR))
        return 1;
    rejected.version = 1;
    rejected.element_mtype = MTYPE_U1;
    if (!DSL_scalar_test_rejection(&rejected, DSL_SIMP_REJECT_PROJECTION))
        return 1;
    WN_DELETE_Tree(rejected.projected_kid[0]);
    rejected.dsl_operator = OPR_DSLSUB;
    rejected.element_mtype = MTYPE_I4;
    rejected.kid_count = 2;
    rejected.projected_kid[0] = WN_Intconst(MTYPE_I4, 7);
    rejected.projected_kid[1] = rejected.projected_kid[0];
    if (!DSL_scalar_test_rejection(&rejected, DSL_SIMP_REJECT_PROJECTION))
        return 1;
    WN_DELETE_Tree(rejected.projected_kid[0]);
    rejected.dsl_operator = OPR_DSLSELECT;
    rejected.element_mtype = MTYPE_F4;
    rejected.kid_count = 3;
    ST *condition = DSL_scalar_test_symbol("runtime_condition", MTYPE_I4, position);
    rejected.projected_kid[0] = WN_Ldid(MTYPE_I4, 0, condition, ST_type(condition));
    rejected.projected_kid[1] = DSL_scalar_test_constant("7fc01234");
    rejected.projected_kid[2] = DSL_scalar_test_constant("00000000");
    if (!DSL_scalar_test_rejection(&rejected, DSL_SIMP_REJECT_NUMERIC_POLICY))
        return 1;
    for (UINT32 index = 0; index < 3; ++index)
        WN_DELETE_Tree(rejected.projected_kid[index]);

    IEEE_Arithmetic = saved_ieee;
    Roundoff_Level = saved_roundoff;
    Enable_Cfold_Reassociate = saved_reassociate;
    Force_IEEE_Comparisons = saved_comparisons;
    Enable_Cfold_Aggressive = saved_aggressive;
    Div_Split_Allowed = saved_div_split;
    WN *return_stmt = WN_CreateReturn();
    WN_Set_Linenum(return_stmt, position);
    WN_INSERT_BlockLast(WN_func_body(PU_Info_tree_ptr(pu)), return_stmt);
    const char *path = getenv("OPEN64_DSL_SCALAR_REUSE_ARTIFACT");
    if (path != NULL) {
        DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
        request.path = path;
        request.flags = 0;
        if (!DSL_Builder_Finalize_Mapped_Image(&request))
            return 1;
    }
    printf("scalar WHIRL reuse passed: SUB GT LNOT SELECT; strict FP unchanged; "
           "NaN/infinity/signed-zero SELECT bits retained; unresolved intrinsics rejected\n");
    return 0;
}

/* Execute the focused production-API contract in its own compiler process.
 * A nonzero status leaves diagnostics/artifacts with the invoking harness. */
int
main (void)
{
    DSL_scalar_test_initialize();
    return DSL_scalar_reuse_contract();
}
