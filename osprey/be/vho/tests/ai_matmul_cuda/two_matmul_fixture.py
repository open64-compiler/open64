"""Emit two eligible matmul PUs to test program-level device publication."""

import sys
from pathlib import Path

from open64_dsc import WhirlExportOptions, load_builder, save_as_whirl
from open64_dsc.module import WhirlModule, WhirlProgramUnitRecord


def main() -> int:
    if len(sys.argv) != 2:
        raise SystemExit("usage: two_matmul_fixture.py OUTPUT.B")
    builder = load_builder("native")
    builder.begin_program()
    first_pu = None
    for ordinal in range(2):
        name = f"ai_matmul_duplicate_{ordinal}"
        pu = builder.minimal_program_unit(name)
        if first_pu is None:
            first_pu = pu
        source = str(Path(__file__).resolve())
        builder.set_pu_source_identity(pu, name, "ai.matmul.duplicate",
                                       source, 1)
        source_id = builder.register_source_file(pu, source)
        lhs = builder.tensor_constant("lhs", "float32", 2, "[128,128]",
                                      "splat", "1.0")
        rhs = builder.tensor_constant("rhs", "float32", 2, "[128,128]",
                                      "splat", "1.0")
        result = builder.common_matmul(lhs, rhs)
        for value in (lhs, rhs, result):
            builder.set_value_source_position(value, source_id, 1)
            builder.append_program_unit_value(pu, value)
        builder.return_pu_values(pu, [])
    module = WhirlModule(
        options=WhirlExportOptions(backend="native", model_name="TwoMatmuls",
                                   verify=False),
        model_name="TwoMatmuls", input_count=0,
        entry_function=WhirlProgramUnitRecord(
            "ai_matmul_duplicate_0", first_pu.value,
            ["common.tensor_const", "common.tensor_const", "common.matmul"]),
        graph_source="ai-matmul-two-pu-fixed-f32",
        operators=["common.tensor_const", "common.matmul"],
    )
    save_as_whirl(module, sys.argv[1])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
