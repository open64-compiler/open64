/* common/com direct tensor elementwise representation regression. Exercise
 * actual builder, copy/equality, scalar-engine exclusion and mapped-boundary
 * APIs in one caller-owned PU. No independent arithmetic evaluator or source
 * numerical binding. Frozen .B compatibility is checked by the harness.
 * Design: doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md. */
#ifndef DSL_TENSOR_ELEMENTWISE_FIXTURE_INCLUDED
#define DSL_TENSOR_ELEMENTWISE_FIXTURE_INCLUDED

#include <unistd.h>
#include <sys/wait.h>
#include "dsl_gatekeeper.h"
#include "ir_bwrite.h"
#include "strtab.h"

/* Report the exact failed fixture boundary; the process retains its staging
 * diagnostics and leaves production negative-case behavior unchanged. */
static int
DSL_tensor_test_failure (UINT32 line)
{
    fprintf(stderr, "direct tensor fixture failed at line %u\n", line);
    return 1;
}

/* Construct a canonical static logical tensor through the public builder;
 * table/pool ownership stays with the initialized fixture compiler context. */
static TY_IDX
DSL_tensor_test_type (const char *name, const char *dtype, TYPE_ID mtype,
                      const char *shape)
{
    DSL_BUILDER_TENSOR_TYPE_CORE core;
    memset(&core, 0, sizeof(core));
    core.kind = "tensor";
    core.dtype = dtype;
    core.rank = 2;
    core.logical_shape = shape;
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core = core;
    descriptor.representation.layout = "logical";
    return DSL_Builder_Intern_Tensor_Type(name, MTYPE_To_TY(mtype), &descriptor);
}

/* Isolate terminal table finalization in a child, preserving the parent PU
 * for before/after checks. The child verifies expression bytes and kids remain
 * untouched by successful writing; malformed-output preflight is tested below. */
static BOOL
DSL_tensor_test_snapshot (const char *directory, const char *stem,
                          WN **statements, UINT32 count)
{
    if (directory == NULL)
        return TRUE;
    fflush(NULL);
    pid_t child = fork();
    if (child < 0)
        return FALSE;
    if (child == 0) {
        WN saved[8];
        WN *expressions[8];
        WN *third[8];
        if (count > 8)
            _exit(2);
        for (UINT32 index = 0; index < count; ++index) {
            expressions[index] = WN_kid0(statements[index]);
            memcpy(&saved[index], expressions[index], sizeof(WN));
            third[index] = WN_kid_count(expressions[index]) > 2 ?
                WN_kid2(expressions[index]) : NULL;
        }
        char path[4096];
        snprintf(path, sizeof(path), "%s/%s.B", directory, stem);
        DSL_BUILDER_MAPPED_IMAGE_REQUEST request = { path, 0 };
        if (!DSL_Builder_Finalize_Mapped_Image(&request))
            _exit(1);
        /* Check the producer itself, not just its fork-isolated parent. */
        for (UINT32 index = 0; index < count; ++index)
            if (memcmp(&saved[index], expressions[index], sizeof(WN)) != 0 ||
                (third[index] != NULL && WN_kid2(expressions[index]) != third[index]))
                _exit(2);
        _exit(0);
    }
    int status = 0;
    return waitpid(child, &status, 0) == child && WIFEXITED(status) &&
           WEXITSTATUS(status) == 0;
}

/* Exercise generic publication, bypassing the builder finalizer. A child may
 * create only a diagnostic staging file; acceptance would publish it and fail
 * this test. Parent tables and the deliberately corrupt expression stay intact. */
static BOOL
DSL_tensor_test_writer_rejects (PU_Info *pu, const char *directory,
                                const char *stem)
{
    if (directory == NULL)
        return TRUE;
    char pending[4096];
    char published[4096];
    snprintf(pending, sizeof(pending), "%s/%s.pending", directory, stem);
    snprintf(published, sizeof(published), "%s/%s.B", directory, stem);
    fflush(NULL);
    pid_t child = fork();
    if (child < 0)
        return FALSE;
    if (child == 0) {
        Irb_File_Name = pending;
        if (Open_Output_Info(Irb_File_Name) == NULL)
            _exit(3);
        Write_PU_Info(pu);
        Write_Global_Info(pu);
        Close_Output_Info();
        (void)rename(pending, published);
        _exit(0);
    }
    int status = 0;
    return waitpid(child, &status, 0) == child && WIFEXITED(status) &&
           WEXITSTATUS(status) != 0 && WEXITSTATUS(status) != 3 &&
           access(published, F_OK) != 0;
}

/* Validate live ordinary tensor WNs and mandatory contracts without executing
 * source arithmetic. Reject malformed semantics and preserve both SELECT arms'
 * expansion. Retain an unchanged mapped-wire before/after family when asked. */
static int
DSL_tensor_elementwise_contract (void)
{
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    PU_Info *pu = DSL_Builder_Create_Minimal_PU("direct_tensor_elementwise");
    if (pu == NULL)
        return DSL_tensor_test_failure(__LINE__);
    DSL_BUILDER_SOURCE_POSITION source;
    memset(&source, 0, sizeof(source));
    source.file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    source.line = __LINE__;
    source.column = 1;
    USRCPOS position;
    USRCPOS_clear(position);
    USRCPOS_filenum(position) = source.file_id;
    USRCPOS_linenum(position) = source.line;
    USRCPOS_column(position) = source.column;
    Set_ST_Srcpos(St_Table[PU_Info_proc_sym(pu)], USRCPOS_srcpos(position));
    WN_Set_Linenum(PU_Info_tree_ptr(pu), USRCPOS_srcpos(position));
    TY_IDX column = DSL_tensor_test_type("column", "float32", MTYPE_F4, "[2,1]");
    TY_IDX row = DSL_tensor_test_type("row", "float32", MTYPE_F4, "[1,3]");
    TY_IDX output = DSL_tensor_test_type("output", "float32", MTYPE_F4, "[2,3]");
    TY_IDX mask_ty = DSL_tensor_test_type("mask", "bool", MTYPE_U1, "[2,1]");
    TY_IDX predicate = DSL_tensor_test_type("predicate", "bool", MTYPE_U1, "[2,3]");
    DSL_BUILDER_VALUE x = DSL_Builder_Declare_PU_Formal(pu, "x", 0, column, &source);
    DSL_BUILDER_VALUE y = DSL_Builder_Declare_PU_Formal(pu, "y", 1, row, &source);
    DSL_BUILDER_VALUE mask = DSL_Builder_Declare_PU_Formal(pu, "mask", 2, mask_ty, &source);
    if (x == NULL || y == NULL || mask == NULL)
        return DSL_tensor_test_failure(__LINE__);
    const DSL_OPERATOR logical[] = { OPR_DSLADD, OPR_DSLSUB, OPR_DSLMUL,
        OPR_DSLDIV, OPR_DSLGREATER, OPR_DSLLOGICALNOT, OPR_DSLSELECT };
    const OPERATOR ordinary[] = { OPR_ADD, OPR_SUB, OPR_MPY, OPR_DIV,
        OPR_GT, OPR_LNOT, OPR_SELECT, OPR_REM };
    const char *names[] = { "sum", "difference", "product", "quotient",
        "greater", "inverted", "selected" };
    WN *statements[8];
    for (UINT32 index = 0; index < 7; ++index) {
        DSL_BUILDER_VALUE kids[] = { x, y, y };
        UINT32 count = 2;
        TY_IDX ty = index == 4 ? predicate : output;
        DSL_BUILDER_OPERATOR_ATTRIBUTE attr = { "attr.broadcast_rule", "numpy" };
        if (index == 5) {
            kids[0] = mask;
            count = 1;
            ty = mask_ty;
        } else if (index == 6) {
            kids[0] = mask;
            kids[1] = y;
            count = 3;
        }
        DSL_OPERATOR_INFO info;
        if (!DSL_Operator_Get_Info_Version(logical[index], 1, &info))
            return DSL_tensor_test_failure(__LINE__);
        DSL_OPCODE_ID id = DSL_Opcode_Find(DSL_Domain_Find("common"), info.stable_name, 1);
        WN *stmt = DSL_Builder_Create_Operator_With_Result(id, 1, kids, count,
            index == 5 ? NULL : &attr, index == 5 ? 0 : 1, names[index], ty);
        if (stmt == NULL || !DSL_Builder_Set_Value_Source_Position(stmt, &source) ||
            !DSL_Builder_Append_PU_Value(pu, stmt))
            return DSL_tensor_test_failure(__LINE__);
        statements[index] = stmt;
        WN *expression = WN_kid0(stmt);
        if (WN_operator(expression) != ordinary[index] || WN_ty(expression) != ty ||
            !DSL_opcode_is_tensor_elementwise(WN_opcode(expression)) ||
            !OPCODE_is_not_executable(WN_opcode(expression)))
            return DSL_tensor_test_failure(__LINE__);
        WN *copy = WN_COPY_Tree(expression);
        if (!WN_Equiv(copy, expression) || WN_ty(copy) != ty ||
            WN_Simp_Compare_Trees(copy, expression) == 0)
            return DSL_tensor_test_failure(__LINE__);
        WN_set_ty(copy, ty == output ? predicate : output);
        if (WN_Equiv(copy, expression))
            return DSL_tensor_test_failure(__LINE__);
        WN_DELETE_Tree(copy);
        BOOL enabled = Enable_WN_Simp;
        Enable_WN_Simp = TRUE;
        WN *rewrite = count == 1 ? WN_SimplifyExp1(WN_opcode(expression), WN_kid0(expression)) :
            count == 2 ? WN_SimplifyExp2(WN_opcode(expression), WN_kid0(expression), WN_kid1(expression)) :
            WN_SimplifyExp3(WN_opcode(expression), WN_kid0(expression), WN_kid1(expression), WN_kid2(expression));
        if (rewrite != NULL || WN_Simplify_Tree(expression) != expression ||
            WN_Simplify_Rebuild_Expr_Tree(expression) != expression)
            return DSL_tensor_test_failure(__LINE__);
        Enable_WN_Simp = enabled;
    }
    TY_IDX integer_ty = DSL_tensor_test_type("integer", "int32", MTYPE_I4, "[2,3]");
    DSL_BUILDER_VALUE integer_kids[] = {
        DSL_Builder_Declare_PU_Formal(pu, "dividend", 3, integer_ty, &source),
        DSL_Builder_Declare_PU_Formal(pu, "divisor", 4, integer_ty, &source)
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE rem_attr = { "attr.broadcast_rule", "numpy" };
    DSL_OPCODE_ID rem_id = DSL_Opcode_Find(DSL_Domain_Find("common"), "common.rem", 1);
    statements[7] = DSL_Builder_Create_Operator_With_Result(rem_id, 1,
        integer_kids, 2, &rem_attr, 1, "remainder", integer_ty);
    if (statements[7] == NULL || WN_operator(WN_kid0(statements[7])) != OPR_REM ||
        !DSL_Builder_Set_Value_Source_Position(statements[7], &source) ||
        !DSL_Builder_Append_PU_Value(pu, statements[7]))
        return DSL_tensor_test_failure(__LINE__);
    DSL_GATEKEEPER_RESULT checked;
    if (!DSL_Gatekeeper_Verify_PU(pu, stderr, &checked))
        return DSL_tensor_test_failure(__LINE__);
    /* Whole-tensor predicates cannot use scalar bit truncation or operand
     * permutation rules, even when their mathematical operator is familiar. */
    BOOL saved_enabled = Enable_WN_Simp;
    for (UINT32 enabled = 0; enabled < 2; ++enabled) {
        Enable_WN_Simp = enabled;
        for (UINT32 index = 4; index <= 5; ++index) {
            WN *kid = WN_kid0(statements[index]);
            if (WN_SimplifyCvtl(OPC_I4CVTL, 8, kid) != NULL ||
                WN_operator(kid) != ordinary[index])
                return DSL_tensor_test_failure(__LINE__);
        }
    }
    Enable_WN_Simp = TRUE;
    WN *scalar = WN_Intconst(MTYPE_I4, 7);
    WN *folded = WN_SimplifyCvtl(OPC_I4CVTL, 8, scalar);
    if (folded == NULL || WN_operator(folded) != OPR_INTCONST ||
        WN_const_val(folded) != 7)
        return DSL_tensor_test_failure(__LINE__);
    WN_DELETE_Tree(folded);
    Enable_WN_Simp = saved_enabled;
    for (UINT32 index = 0; index < 8; ++index)
        if (OPCODE_commutative_op(WN_opcode(WN_kid0(statements[index]))) != 0)
            return DSL_tensor_test_failure(__LINE__);
    if (OPCODE_commutative_op(OPC_I4F4GT) != OPC_I4F4LT)
        return DSL_tensor_test_failure(__LINE__);
    WN *test = WN_kid0(statements[1]);
    const char *directory = getenv("OPEN64_DSL_DIRECT_TENSOR_ARTIFACT_DIR");
    TY_IDX saved_ty = WN_ty(test);
    WN_set_ty(test, row);
    if (DSL_Gatekeeper_Verify_PU(pu, NULL, &checked))
        return DSL_tensor_test_failure(__LINE__);
    if (!DSL_tensor_test_writer_rejects(pu, directory, "bad-type"))
        return DSL_tensor_test_failure(__LINE__);
    if (directory != NULL) {
        char rejected_path[4096];
        snprintf(rejected_path, sizeof(rejected_path), "%s/rejected.B", directory);
        DSL_BUILDER_MAPPED_IMAGE_REQUEST rejected = { rejected_path, 0 };
        if (DSL_Builder_Finalize_Mapped_Image(&rejected) || access(rejected_path, F_OK) == 0)
            return DSL_tensor_test_failure(__LINE__);
    }
    WN_set_ty(test, saved_ty);
    OPCODE saved_opcode = WN_opcode(test);
    WN_set_opcode(test, OPCODE_make_op(OPR_ADD, MTYPE_M, MTYPE_V));
    if (!DSL_tensor_test_writer_rejects(pu, directory, "bad-operator"))
        return DSL_tensor_test_failure(__LINE__);
    WN_set_opcode(test, saved_opcode);
    WN_OFFSET saved_record = WN_offset(test);
    WN_OFFSET corrupt[] = { 0, (WN_OFFSET)STR_Table_Size(),
        (WN_OFFSET)0x7fffffffU, (WN_OFFSET)0x80000000U, (WN_OFFSET)0xffffffffU };
    DSL_WHIRL_NODE_RECORD decoded;
    for (UINT32 index = 0; index < sizeof(corrupt) / sizeof(corrupt[0]); ++index) {
        WN_offset(test) = corrupt[index];
        if (DSL_WN_Decode_Opcode_Record(test, &decoded) ||
            DSL_Gatekeeper_Verify_PU(pu, NULL, &checked) || !DSL_WN_Is_Native(test) ||
            WN_SimplifyExp2(WN_opcode(test), WN_kid0(test), WN_kid1(test)) != NULL)
            return DSL_tensor_test_failure(__LINE__);
    }
    STR_IDX tail = STR_Table_Size() - 1;
    char *last = Index_To_Str(tail);
    char saved_last = *last;
    *last = 'x';
    WN_offset(test) = (WN_OFFSET)tail;
    BOOL invalid_record = DSL_WN_Decode_Opcode_Record(test, &decoded);
    *last = saved_last;
    if (invalid_record)
        return DSL_tensor_test_failure(__LINE__);
    WN_offset(test) = saved_record;
    WN *ret = WN_CreateReturn();
    WN_Set_Linenum(ret, WN_Get_Linenum(statements[0]));
    WN_INSERT_BlockLast(WN_func_body(PU_Info_tree_ptr(pu)), ret);
    if (!DSL_tensor_test_snapshot(directory, "tensor.before", statements, 8))
        return DSL_tensor_test_failure(__LINE__);
    /* Actual whole-tree simplification is a no-op: no scalar identities may
     * erase the required tensor domain, FP permissions or eager operands. */
    if (WN_Simplify_Tree(PU_Info_tree_ptr(pu)) != PU_Info_tree_ptr(pu) ||
        !DSL_tensor_test_snapshot(directory, "tensor.after", statements, 8))
        return DSL_tensor_test_failure(__LINE__);
    for (UINT32 index = 0; index < 8; ++index)
        if (WN_operator(WN_kid0(statements[index])) != ordinary[index])
            return DSL_tensor_test_failure(__LINE__);
    puts("direct tensor operators passed: ADD SUB MPY DIV REM GT LNOT SELECT; type/record/clone/FP/broadcast/writer purity preserved");
    return 0;
}

#endif
