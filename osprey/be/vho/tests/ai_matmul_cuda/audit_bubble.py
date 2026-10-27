"""Independent process reopening of retained bubble event/interval evidence.

Design: doc/AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md, replay slice. This checks
schedule feasibility and totals, not validity of synthetic hardware timings.
"""

import argparse
import gzip
import json
from pathlib import Path

from gemm_bubble import Event, Graph, audit_timeline


def audit(path):
    """Reopen serialized evidence and verify every dependency and resource interval."""
    data = json.loads(gzip.decompress(path.read_bytes()))
    graph = Graph()
    graph.events = [Event(**dict(row, deps=tuple(tuple(d) for d in row["deps"]),
                               demands=tuple(tuple(d) for d in row["demands"])))
                    for row in data["graph"]]
    audit_timeline(graph, data["result"])


def main():
    """Audit a complete retained event directory without rerunning the compiler."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("events", type=Path)
    args = parser.parse_args()
    paths = sorted(args.events.rglob("*.json.gz"))
    if not paths:
        parser.error("no event evidence found")
    for path in paths:
        audit(path)
    print("PASS: reopened %d event graphs and audited interval schedules" % len(paths))


if __name__ == "__main__":
    main()
