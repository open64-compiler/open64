"""Check generated-C execution against the immutable SYNC-5 schedule.

See doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-H. The trace is emitted by
executing unchanged whirl2c output; this tool does not infer a new schedule.
"""

from collections import Counter
import json
import re
import sys


FAMILIES = {
    10: [1],
    7: [2],
    5: [3, 4, 5, 5, 5, 6],
    13: [7],
    6: [8],
    8: [9],
}


def validate(trace_path: str, schedule_path: str, source_path: str) -> None:
    """Match execution and the root's ordered resource roles to the plan."""
    with open(trace_path, encoding="utf-8") as stream:
        lines = [line.strip() for line in stream]
    with open(schedule_path, encoding="utf-8") as stream:
        schedule = json.load(stream)

    assert schedule["schema"] == "open64.fhe.sync5.runtime-schedule.v1"
    assert schedule["static_evaluations"] == 87
    assert schedule["dynamic_evaluations"] == 147
    assert len(schedule["records"]) == 32
    assert len(lines) == 295
    assert lines[-1] == "SUMMARY selects=147 evals=147 output=set"
    observed = Counter()
    for sequence in range(147):
        selected = lines[2 * sequence].split()
        evaluated = lines[2 * sequence + 1].split()
        assert len(selected) == 5 and len(evaluated) == 5
        assert selected[0] == "SELECT" and evaluated[0] == "EVAL"
        assert int(selected[1]) == int(evaluated[1]) == sequence + 1
        assert selected[4] == evaluated[2]
        observed[(int(selected[2]), int(selected[3]))] += 1

    expected = Counter()
    next_ordinal = 1
    for record in schedule["records"]:
        kinds = FAMILIES[record["operator"]]
        assert record["first_ordinal"] == next_ordinal
        assert record["static_count"] == len(kinds)
        assert record["dynamic_count"] == len(kinds) * record["multiplicity"]
        for index, kind in enumerate(kinds):
            expected[(next_ordinal + index, kind)] += record["multiplicity"]
        next_ordinal += len(kinds)
    assert next_ordinal == 88
    assert observed == expected, (observed - expected, expected - observed)

    with open(source_path, encoding="utf-8") as stream:
        generated_c = stream.read()
    root = re.search(r"static void SecureResNet20\(([^\n]*)\)", generated_c)
    assert root is not None
    formals = [name.strip() for name in root.group(1).split(",")]
    assert len(formals) == 50
    assert formals[0] == "__dsl_runtime_input0_1"
    assert formals[1] == "__dsl_runtime_model_result_267"
    assert sum("folded_weight" in name for name in formals) == 21
    assert sum("folded_bias" in name for name in formals) == 21
    assert formals[42] == "__dsl_input_entry_stem_conv_folded_bias"
    assert formals[43] == "__dsl_input_entry_stem_conv_folded_weight"
    assert formals[44] == "__dsl_input_fc_bias"
    assert formals[45] == "__dsl_input_fc_weight"
    assert formals[46] == "__dsl_input_fhe_model"
    for index in range(3):
        assert formals[47 + index] == (
            "__dsl_input_fhe_relu_coefficient_stage" + str(index)
        )
    assert "OPR_DSL" not in generated_c


if __name__ == "__main__":
    if len(sys.argv) != 4:
        raise SystemExit("usage: schedule_check.py TRACE SCHEDULE GENERATED_C")
    validate(sys.argv[1], sys.argv[2], sys.argv[3])
