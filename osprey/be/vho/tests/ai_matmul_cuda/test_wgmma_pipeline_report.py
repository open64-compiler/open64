"""Protocol order and audit invalidation checks without a CUDA device."""
import unittest

from wgmma_pipeline_report import atom, protocol_events, verify_protocol


class PipelineProtocolTest(unittest.TestCase):
    def test_one_two_many_and_odd(self):
        for tma in (False, True):
            for steps in range(1, 66):
                self.assertTrue(verify_protocol(protocol_events(steps, tma), tma))

    def test_missing_events_fail_closed(self):
        for tma in (False, True):
            events = protocol_events(5, tma)
            for index in range(len(events)):
                with self.subTest(tma=tma, removed=events[index]):
                    with self.assertRaises(ValueError):
                        verify_protocol(events[:index] + events[index + 1:], tma)

    def test_tma_ready_is_not_compute_done(self):
        events = protocol_events(2, True)
        for event in ("fragment_read", "reuse_join"):
            with self.assertRaises(ValueError):
                verify_protocol(events[:4] + [(event, -1)] + events[4:], True)

    def test_reader_lifetime_and_queue(self):
        for tma in (False, True):
            events = protocol_events(2, tma)
            index = events.index(("mma_issue", 0)) + 1
            with self.assertRaises(ValueError):
                verify_protocol(events[:index] + [("pack", 0)] + events[index:], tma)
            with self.assertRaises(ValueError):
                verify_protocol(events, tma, group_limit=1)

    def test_both_formats_actual_wait_before_store(self):
        for dtype in ("f16", "bf16"):
            for groups in (1, 2):
                text = atom(dtype, groups)
                self.assertEqual(text.count("wgmma.mma_async"), groups)
                self.assertEqual(text.count("wgmma.commit_group"), groups)
                self.assertLess(text.index("wait_group.sync.aligned0;"), text.index("st.f32"))


if __name__ == "__main__":
    unittest.main()
