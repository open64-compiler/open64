"""Synthetic execution-auditor mutations, not runtime/GPU execution evidence.
Design: doc/AI-COMPILER-LAYERNORM-EXECUTION-CONSUMER-v0.1.md.
"""
import unittest
from normalization_execution_trace_report import audit
from normalization_guard_trace_report import FIELDS
from test_normalization_guard_trace_report import export_fixture


def fixture():
    """One explicit host-X/host-Y story with real guard schema and fake event rows."""
    g,_,_,_=export_fixture(3);line=g['source_line'];rows=[];sequence=0;runtime_sequence=0
    expected=dict(guard_words=[g[k] for k in FIELDS]);rows.append(f'AI_NORM_EXEC_TEST 1 SCENARIO 1 {line}')
    def event(action,status=0,a=0,b=0):
        """Append a positional synthetic consumer observation."""
        nonlocal sequence
        sequence+=1;rows.append(f'AI_NORM_EXEC 1 EVENT 1 {sequence} {action} {status} {a} {b} {line}')
    def run(action,leases=15):
        """Append a synthetic successful base-runtime event."""
        nonlocal runtime_sequence
        runtime_sequence+=1;rows.append(f'AI_NORM_RUN 1 EVENT {runtime_sequence} 10 {action} 0 0 {leases} 1 {line}')
    event('open');event('allocate',0,0,g['range0_capacity']);event('owned_root',0,0,90)
    rows.append('AI_NORM_RUN 1 BIND 10 83 '+' '.join(map(str,expected['guard_words']))+' '+str(line))
    event('admit',0,10);run('begin_ready');event('h2d',0,0,g['range0_bytes']);run('ready')
    run('begin_launch');run('launch');run('begin_drain');run('drain');run('begin_publish')
    event('d2h',0,3,g['range3_bytes']);run('publish');run('close',0)
    event('close',0,10);event('retire',0,0,90);event('release',0,0);event('idle');event('destroy')
    rows+=['AI_NORM_EXEC_TEST 1 CHECK synthetic 1 '+str(line),'AI_NORM_EXEC_TEST 1 SUMMARY 1 1 0 host_mock_sentinel_only '+str(line)]
    return '\n'.join(rows)+'\n',expected


class ExecutionTraceTests(unittest.TestCase):
    """Raw event reconstruction detects tampering independently of CHECK names."""
    def test_complete(self):
        """A closed, ordered synthetic movement/ownership chain is accepted."""
        text,expected=fixture();result=audit(text,expected,scenarios=1)
        self.assertEqual((result['checks'],result['tickets'],result['h2d_attempts'],result['d2h_attempts']),(1,1,1,1))
        self.assertFalse(result['GPU'])

    def test_missing_admission_or_drain(self):
        """Successful-looking copies cannot bypass admission or device completion."""
        text,expected=fixture()
        changed=text.replace(' admit 0 10 0 ',' admit 2 0 0 ')
        with self.assertRaises(ValueError):audit(changed,expected,scenarios=1)
        changed=text.replace(' drain 0 0 15 ',' drain 5 0 15 ')
        with self.assertRaises(ValueError):audit(changed,expected,scenarios=1)

    def test_early_close_or_free(self):
        """Live leases and missing retirement must remain visible at close/release."""
        text,expected=fixture()
        with self.assertRaises(ValueError):audit(text.replace(' close 0 0 0 1 ',' close 0 0 15 1 '),expected,scenarios=1)
        with self.assertRaises(ValueError):audit(text.replace(' retire 0 0 90 ',' retire 4 0 90 '),expected,scenarios=1)

    def test_extent_and_source_identity(self):
        """Native guard, valid copy size and source cross-reference are exact."""
        text,expected=fixture();g=dict(zip(FIELDS,expected['guard_words']))
        with self.assertRaises(ValueError):audit(text.replace(f" h2d 0 0 {g['range0_bytes']} ",f" h2d 0 0 {g['range0_bytes']+4} "),expected,scenarios=1)
        changed=dict(expected);changed['guard_words']=list(expected['guard_words']);changed['guard_words'][2]=1
        with self.assertRaises(ValueError):audit(text,changed,scenarios=1)

    def test_census_sequence_and_terminal(self):
        """Dropped scenarios, malformed sequencing and post-summary records refuse."""
        text,expected=fixture()
        with self.assertRaises(ValueError):audit(text,expected)
        with self.assertRaises(ValueError):audit(text.replace('1 1 open','1 2 open'),expected,scenarios=1)
        with self.assertRaises(ValueError):audit(text+'AI_NORM_EXEC_TEST 1 CHECK late 1 211\n',expected,scenarios=1)


if __name__=='__main__':unittest.main()
