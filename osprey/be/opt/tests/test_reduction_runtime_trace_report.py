"""Adversarial replay checks for the runtime trace, independent of GPU/MMA1."""
import unittest
from reduction_runtime_trace_report import HEADER, BIND_HEADER, audit


def sample():
    """One real-shape graph with a fully joined and released invocation."""
    text = HEADER + '\n' + BIND_HEADER + '\nAI_REDUCE_RUN 1 BIND 1 101 1 2 3 0 204 0 12 2 4 24 32 0 0 124\n'
    rows = [('admit', 0, 3), ('ready', 1, 3), ('launch', 3, 3), ('complete', 7, 3),
            ('consume', 15, 3), ('input_release', 31, 2), ('output_release', 63, 0), ('close', 63, 0)]
    for seq, (action, done, leases) in enumerate(rows, 1):
        text += 'AI_REDUCE_RUN 1 EVENT %d 1 %s 0 %d %d 124\n' % (seq, action, done, leases)
    return text


class RuntimeTraceTest(unittest.TestCase):
    """Mutated traces must not earn the successful enforcement report."""

    def test_valid(self):
        """Accept exact event masks and terminal lease release."""
        self.assertEqual(audit(sample())['closed'], 1)

    def test_mutations(self):
        """Reject forged masks, early release, graph gaps and missing close."""
        for before, after in [('ready 0 1 3', 'ready 0 3 3'),
                              ('consume 0 15 3', 'output_release 0 47 1'),
                              ('24 32 0 0', '8 32 0 0'),
                              ('8 1 close', '9 1 close'),
                              ('63 0 124', '63 0 125')]:
            with self.subTest(before=before), self.assertRaises(AssertionError):
                audit(sample().replace(before, after))
        with self.assertRaises(AssertionError):
            audit('\n'.join(sample().splitlines()[:-1]))

    def test_failed_launch_needs_drain_and_never_publishes(self):
        """Accept drain/abort only; do not let a failure manufacture completion."""
        text = '\n'.join(sample().splitlines()[:3]) + '\n'
        rows = [('admit', 0, 0, 3), ('ready', 0, 1, 3), ('launch', 5, 1, 3),
                ('abort', 4, 1, 3), ('complete', 5, 1, 3), ('complete', 0, 5, 3),
                ('consume', 6, 5, 3), ('abort', 0, 5, 0), ('close', 0, 5, 0)]
        for seq, (action, status, done, leases) in enumerate(rows, 1):
            text += 'AI_REDUCE_RUN 1 EVENT %d 1 %s %d %d %d 124\n' % (seq, action, status, done, leases)
        self.assertEqual(audit(text)['backend_failures'], 2)
        for before, after in [('abort 4 1 3', 'abort 0 1 0'),
                              ('consume 6 5 3', 'consume 0 13 3'),
                              ('complete 5 1 3', 'complete 5 5 3')]:
            with self.subTest(before=before), self.assertRaises(AssertionError):
                audit(text.replace(before, after))


if __name__ == '__main__':
    unittest.main()
