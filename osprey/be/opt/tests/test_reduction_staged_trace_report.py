"""Adversarial staged trace and independent visibility/order checks."""
import random
import unittest
from reduction_staged_trace_report import COLUMNS, HEADER, SLICE_HEADER, audit, events_valid, expected_status, slice_witness


def row():
    """A small contract fixture; real producer integration is tested separately."""
    r = dict.fromkeys(COLUMNS, 0)
    r.update(node=2, input=1, result=3, rows=3, width=17, version=2, arch=1, chunk_request=8,
             slots=1, scratch_object=401, space=2, offset=16, stride=36, allocation=1168,
             row_budget=32, cta_budget=65536, other=128, rows_cta=32, ctas=1, threads=32,
             copy=1, visibility=1, tail=1, e0=2, e1=4, e2=8, e3=16, n4=1,
             chunk=8, chunks=3, last_count=1, last_active=3, row_bytes=32, scratch_end=1164,
             reserved=2432, hw_budget=65536, headroom=1024, quantum=128, max_threads=1024,
             obligations=255, source_line=90)
    return r


def text(r=None):
    """One baseline row and complete exact-count slice coverage."""
    r = row() if r is None else r
    result = HEADER + '\nAI_REDUCE_STAGE 1 BIND ' + ' '.join(str(r[k]) for k in COLUMNS) + '\n' + SLICE_HEADER + '\n'
    for owner in range(3):
        for g in range(3):
            result += 'AI_REDUCE_SLICE 1 PLAN 2 %d %d %d %d %d %d %d 90\n' % (
                owner, g, g * 8, min(8, 17 - g * 8), 16 + 4 * (owner * 17 + g * 8),
                16 + owner * 36, 32 + owner * 4)
    return result


class StagedTraceTest(unittest.TestCase):
    """Do not let status flags substitute for slice, resource or event checks."""

    def test_valid_and_host_witness(self):
        """Poison tails and compare the full ordered fold with the integer oracle."""
        rows, slices = audit(text())
        self.assertEqual(len(slice_witness(rows[0], slices)), 3)

    def test_contract_mutations(self):
        """Reject unproved success for each supported gate and derived field."""
        for key, value in dict(base_status=12, chunk_request=0, copy=2, visibility=2, slots=2, space=1,
                               stride=28, rows_cta=0, threads=31, allocation=1163,
                               row_budget=31, cta_budget=2431, other=(1 << 64) - 1,
                               e1=0, e4=1, n4=0, n3=1, tail=2, chunks=4,
                               last_count=8, last_active=32, reserved=1, obligations=15,
                               selected=1).items():
            bad = row(); bad[key] = value
            with self.subTest(key=key), self.assertRaises(AssertionError):
                audit(text(bad))

    def test_slice_mutations(self):
        """Reject missing/duplicate/reordered, overlapping or padded tail slices."""
        for before, after in [('0 2 16 1', '0 2 16 8'), ('0 1 8 8', '0 1 7 8'),
                              ('0 0 0 8 16 16', '0 0 0 8 16 20'),
                              ('0 0 0 8', '3 0 0 8'), ('0 1 8 8', '0 0 0 8')]:
            with self.subTest(before=before), self.assertRaises(AssertionError):
                audit(text().replace(before, after))
        with self.assertRaises(AssertionError):
            audit('\n'.join(text().splitlines()[:-1]))

    def test_statuses(self):
        """Differentiate missing visibility, premature reuse and unsupported tails."""
        for key, value, expected in [('base_status', 2, 1), ('chunk_request', 0, 2),
                                     ('copy', 2, 3), ('slots', 2, 4), ('rows_cta', 0, 5),
                                     ('row_budget', 31, 6), ('e2', 0, 7), ('n4', 0, 8), ('tail', 2, 9)]:
            r = row(); r[key] = value
            self.assertEqual(expected_status(r), expected)

    def test_event_graph_reference(self):
        """Compare closure with independently ranked total-order legality."""
        rng = random.Random(20261028)
        graphs = [[2, 4, 8, 16, 0], [18, 4, 8, 16, 0]]
        graphs += [[rng.randrange(64) for _ in range(5)] for _ in range(2048)]
        for edges in graphs:
            reference = all(not edge & ~sum(1 << j for j in range(i + 1, 5)) and
                            (i == 4 or edge & (1 << (i + 1))) for i, edge in enumerate(edges))
            self.assertEqual(events_valid(edges), bool(reference))


if __name__ == '__main__':
    unittest.main()
