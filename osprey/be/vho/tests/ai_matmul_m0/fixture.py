"""Emit the fixed-shape F32 common.matmul PU for the m0 baseline."""

import sys
import inspect
from pathlib import Path

from open64_dsc import WhirlExportOptions, load_builder, save_as_whirl
from open64_dsc.module import WhirlModule, WhirlProgramUnitRecord


def main() -> int:
    if len(sys.argv) != 2:
        raise SystemExit("usage: fixture.py OUTPUT.B")
    output = Path(sys.argv[1])
    builder = load_builder("native")
    builder.begin_program()
    pu = builder.minimal_program_unit("ai_matmul_m0")
    builder.set_pu_source_identity(
        pu, "ai_matmul_m0", "ai.matmul.m0", str(Path(__file__).resolve()), 1
    )
    source_id = builder.register_source_file(pu, str(Path(__file__).resolve()))

    lhs_line = inspect.currentframe().f_lineno + 1
    lhs = builder.tensor_constant("lhs", "float32", 2, "[128,128]", "splat", "1.0")
    rhs_line = inspect.currentframe().f_lineno + 1
    rhs = builder.tensor_constant("rhs", "float32", 2, "[128,128]", "splat", "1.0")
    matmul_line = inspect.currentframe().f_lineno + 1
    result = builder.common_matmul(lhs, rhs)
    for value, line in ((lhs, lhs_line), (rhs, rhs_line),
                        (result, matmul_line)):
        builder.set_value_source_position(value, source_id, line)
        builder.append_program_unit_value(pu, value)
    builder.return_pu_values(pu, [])

    module = WhirlModule(
        options=WhirlExportOptions(
            backend="native", model_name="AIMatmulM0", verify=False
        ),
        model_name="AIMatmulM0",
        input_count=0,
        entry_function=WhirlProgramUnitRecord(
            "ai_matmul_m0", pu.value,
            ["common.tensor_const", "common.tensor_const", "common.matmul"],
        ),
        graph_source="ai-matmul-m0-fixed-f32",
        operators=["common.tensor_const", "common.matmul"],
    )
    save_as_whirl(module, str(output))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
