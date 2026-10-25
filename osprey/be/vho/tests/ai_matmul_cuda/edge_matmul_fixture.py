"""Emit an irregular F32 matmul PU to exercise all three tile edges."""

import sys
from pathlib import Path

from open64_dsc import WhirlExportOptions, load_builder, save_as_whirl
from open64_dsc.module import WhirlModule, WhirlProgramUnitRecord


def main() -> int:
    if len(sys.argv) not in (2, 5):
        raise SystemExit("usage: edge_matmul_fixture.py OUTPUT.B [M N K]")
    m, n, k = (130, 133, 135) if len(sys.argv) == 2 else \
        tuple(map(int, sys.argv[2:]))
    if min(m, n, k) <= 0:
        raise SystemExit("matmul dimensions must be positive")
    builder = load_builder("native")
    builder.begin_program()
    pu = builder.minimal_program_unit("ai_matmul_edges")
    source = str(Path(__file__).resolve())
    builder.set_pu_source_identity(pu, "ai_matmul_edges", "ai.matmul.edges",
                                   source, 1)
    source_id = builder.register_source_file(pu, source)
    lhs = builder.tensor_constant("lhs", "float32", 2, f"[{m},{k}]",
                                  "splat", "1.0")
    rhs = builder.tensor_constant("rhs", "float32", 2, f"[{k},{n}]",
                                  "splat", "1.0")
    result = builder.common_matmul(lhs, rhs)
    for value in (lhs, rhs, result):
        builder.set_value_source_position(value, source_id, 1)
        builder.append_program_unit_value(pu, value)
    builder.return_pu_values(pu, [])
    module = WhirlModule(
        options=WhirlExportOptions(backend="native", model_name="EdgeMatmul",
                                   verify=False),
        model_name="EdgeMatmul", input_count=0,
        entry_function=WhirlProgramUnitRecord(
            "ai_matmul_edges", pu.value,
            ["common.tensor_const", "common.tensor_const", "common.matmul"]),
        graph_source="ai-matmul-irregular-f32",
        operators=["common.tensor_const", "common.matmul"],
    )
    save_as_whirl(module, sys.argv[1])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
