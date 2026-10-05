#!/usr/bin/env python3
"""Measure the forbidden direct stride-two Conv mapping cost for C2 review.

This is a shape-only estimator, not executable CKKS lowering. Design:
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
"""

import argparse
import json
from pathlib import Path


def direct_cost(input_channels, output_channels, input_width, kernel):
    """Count valid per-position contributions and distinct signed offsets."""
    output_width = input_width // 2
    padding = (kernel - 1) // 2
    rotations = set()
    masks = 0
    max_offsets_per_term = 0
    for oc in range(output_channels):
        for ci in range(input_channels):
            for ky in range(kernel):
                for kx in range(kernel):
                    term_offsets = set()
                    for oy in range(output_width):
                        for ox in range(output_width):
                            iy = 2 * oy + ky - padding
                            ix = 2 * ox + kx - padding
                            if not (0 <= iy < input_width and
                                    0 <= ix < input_width):
                                continue
                            source = ci * input_width**2 + iy * input_width + ix
                            dest = oc * output_width**2 + oy * output_width + ox
                            offset = source - dest
                            term_offsets.add(offset)
                            rotations.add(offset)
                            masks += 1
                    max_offsets_per_term = max(max_offsets_per_term,
                                               len(term_offsets))
    return {
        "input_channels": input_channels,
        "output_channels": output_channels,
        "input_width": input_width,
        "output_width": output_width,
        "kernel": kernel,
        "term_count": input_channels * output_channels * kernel**2,
        "direct_position_masks": masks,
        "distinct_nonzero_signed_offsets": len(rotations - {0}),
        "max_offsets_per_term": max_offsets_per_term,
    }


def counterexample():
    """Expose why one constant rotation fails for a 4x4-to-2x2 1x1 Conv."""
    offsets = []
    for oy in range(2):
        for ox in range(2):
            source = (2 * oy) * 4 + 2 * ox
            dest = oy * 2 + ox
            offsets.append(source - dest)
    assert offsets == [0, 1, 6, 7]
    return offsets


def main():
    """Write deterministic review evidence for the two captured projections."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    rows = [direct_cost(*shape, kernel)
            for shape in ((16, 32, 32), (32, 64, 16))
            for kernel in (1, 3)]
    assert [(r["direct_position_masks"],
             r["distinct_nonzero_signed_offsets"])
            for r in rows] == [
                (131072, 23551), (1131008, 24049),
                (131072, 12031), (1083392, 12153)]
    evidence = {
        "schema": "open64.fhe.sync6.conv-stride-cost.v1",
        "status": "shape_only_unoptimized_direct_cost_not_executable_plan",
        "counterexample_rotations": counterexample(),
        "cases": rows,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(evidence, indent=2) + "\n",
                           encoding="utf-8")
    print(args.output)


if __name__ == "__main__":
    main()
