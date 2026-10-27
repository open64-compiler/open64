"""Fail-closed tests for the independent MMA3 artifact reader."""
import unittest

from typed_simt_report import audit, contract


VALUES = "9 12801 6 5 5 1 1 1 1 0 7 1 1 7 2 3 5 7 32 1 27"
ACTION = "loads_u2=2 widen_f4=2 product_f4=1 add_f4=1 store_f4=1 order=increasing_k contraction=none timing=unknown 27"
TRACE = f"AI_SIMT_BASELINE 1 CONTRACT {VALUES}\nAI_SIMT_BASELINE 1 ACTION 9 serial_reference {ACTION}\n" + \
        f"AI_SIMT_BASELINE 1 CONTRACT {VALUES}\nAI_SIMT_BASELINE 1 ACTION 9 direct_simt {ACTION}\n"
IDENTITY = "MMA3_TYPED_SIMT_V1 id=9 owner=12801 contract=6 source=5 input=5 product=1 partial=1 outer=1 output=1 permissions=0 max_span=7 evidence=1:1 acceptance=7 generation=2 M=3 N=5 K=7 threads=32 disjoint=1 execution=2"
SOURCE = f"""/* {IDENTITY} */
_UINT16 * input_a; _UINT16 * input_b; _IEEE32 * output;
volatile _IEEE32 rounded_product; volatile _IEEE32 accumulator;
row = blockIdx.y; column = threadIdx.x;
if ((row < 3LL) && (column < 5LL)) {{
accumulator = 0.0F;
for (reduction_index=0; reduction_index<7; reduction_index=reduction_index+1) {{
a_value=__uint_as_float(raw_a); b_value=__uint_as_float(raw_b);
rounded_product=a_value*b_value; accumulator=accumulator+rounded_product;
}} }}
"""
DUMP = f"LOC 1 27\nCOMMENT {IDENTITY}\n U4U2ILOAD\n U4U2ILOAD\n F4TAS\n F4TAS\n F4MPY\n F4ADD\n F4ISTORE\n RETURN\n"


class TypedSIMTReportTests(unittest.TestCase):
    def test_distinct_plan_and_numerical_ids_are_preserved(self):
        record = contract(TRACE)
        self.assertEqual((record["id"], record["contract"]), (9, 6))
        self.assertEqual(audit(SOURCE, DUMP, record, "simt")["F4ADD"], 1)

    def test_malformed_contract_and_boundary_rows_reject(self):
        for broken in (TRACE.replace("1 CONTRACT", "2 CONTRACT"), TRACE + TRACE.splitlines()[0] + "\n",
                       "\n".join(TRACE.splitlines()[1:]), TRACE.replace("direct_simt", "native_mma"),
                       TRACE.replace("timing=unknown", "timing=0"), TRACE.replace("contraction=none", "contraction=fma"),
                       TRACE.replace("6 5 5", "6 5 6"), TRACE.replace("1 0 7", "1 2 7")):
            with self.assertRaises(ValueError):
                contract(broken)

    def test_precision_storage_order_and_widening_corruption_reject(self):
        for old, new in (("volatile", ""), ("_UINT16 * input_b", "_IEEE32 * input_b"),
                         ("__uint_as_float", "numeric_cast"), ("a_value*b_value", "fma(a_value,b_value,accumulator)"),
                         ("reduction_index+1", "reduction_index+2"), ("column < 5LL", "column < 6LL"),
                         ("generation=2", "generation=3"), ("evidence=1:1", "evidence=1:2")):
            with self.assertRaises(ValueError):
                audit(SOURCE.replace(old, new), DUMP, contract(TRACE), "simt")

    def test_actual_ir_operations_and_identity_required(self):
        for old, new in (("F4MPY", "F4MADD"), ("F4TAS", "F4U4CVT"), ("U4U2ILOAD", "F4F4ILOAD"),
                         ("LOC 1", "LOC 0"), ("contract=6", "contract=7")):
            with self.assertRaises(ValueError):
                audit(SOURCE, DUMP.replace(old, new), contract(TRACE), "simt")


if __name__ == "__main__":
    unittest.main()
