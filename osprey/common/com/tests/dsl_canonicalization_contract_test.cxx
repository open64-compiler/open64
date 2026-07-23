/*
 * Runtime contract checks for DSL algebraic canonicalization policy.
 */

#include <stdio.h>
#include <string.h>

#include "dsl_domain.h"
#include "dsl_opcode.h"

int
main(void)
{
    DSL_ALGEBRAIC_INFO info;
    DSL_ALGEBRAIC_RELATION_INFO relation;
    DSL_OPERATOR swap_equivalent;

    DSL_Domain_Registry_Reset();
    DSL_Opcode_Registry_Reset();
    if (DSL_Opcode_Register_Common_Substrate() == 0 ||
        DSL_Operator_Find("common.mul", strlen("common.mul"), 1) !=
            OPR_DSLMUL ||
        strcmp(DSL_OPERATOR_name(OPR_DSLMUL), "OPR_DSLMUL") != 0 ||
        !DSL_Operator_Get_Algebraic_Info(OPR_DSLADD, 1, &info) ||
        info.identity != DSL_ALGEBRAIC_IDENTITY_ZERO ||
        !DSL_Operator_Get_Swap_Equivalent
             (OPR_DSLADD, 1, &swap_equivalent) ||
        swap_equivalent != OPR_DSLADD ||
        !DSL_Operator_Get_Algebraic_Info(OPR_DSLMUL, 1, &info) ||
        info.identity != DSL_ALGEBRAIC_IDENTITY_ONE ||
        !DSL_Algebraic_Relation_Get
             (OPR_DSLADD, 1, OPR_DSLMUL, 1,
              DSL_ALGEBRAIC_RELATION_FACTOR, &relation) ||
        relation.operand_mask != DSL_ALGEBRAIC_OPERAND_ALL) {
        fprintf(stderr, "DSL algebraic registry contract changed\n");
        return 1;
    }

    if (!DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLADD, 1, TRUE, TRUE, FALSE, FALSE, "zeta", "alpha") ||
        DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLADD, 1, TRUE, TRUE, FALSE, FALSE, "alpha", "zeta") ||
        !DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLADD, 1, TRUE, TRUE, TRUE, FALSE, "zero", "alpha") ||
        DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLADD, 1, TRUE, TRUE, FALSE, TRUE, "alpha", "zero") ||
        DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLADD, 1, FALSE, TRUE, FALSE, FALSE, "zeta", "alpha") ||
        DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLADD, 1, TRUE, FALSE, FALSE, FALSE, "zeta", "alpha") ||
        DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLMATMUL, 1, TRUE, TRUE, FALSE, FALSE,
              "zeta", "alpha") ||
        DSL_Algebraic_Should_Swap_Binary_Operands
             (OPR_DSLADD, 2, TRUE, TRUE, FALSE, FALSE, "zeta", "alpha")) {
        fprintf(stderr, "DSL canonical operand-order policy changed\n");
        return 1;
    }

    printf("DSL canonicalization contract passed\n");
    return 0;
}
