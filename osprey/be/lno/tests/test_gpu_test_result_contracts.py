"""Independent reporting arithmetic and evidence-classification checks."""
import unittest
import gpu_test_results as results
import mla_q7_profile as counters

class Contracts(unittest.TestCase):
    def test_raw_statistics(self):
        value=results.stats([102,98,100])
        self.assertEqual(value['median'],100);self.assertEqual(value['mad'],2)
        self.assertAlmostEqual(value['p95'],101.8)
    def test_ambiguous_or_nonfinite_json_is_refused(self):
        for text in ('{"a":1,"a":2}','{"a":NaN}','{"a":1e999}'):
            with self.assertRaises(ValueError):results.loads(text)
    def test_ratio_needs_correct_same_session_unprofiled_wall_samples(self):
        case=dict(correctness=dict(baseline_pass=True,candidate_pass=True),timing={
            'baseline':dict(wall=results.timing([100,100,100],'whole-operation')),
            'candidate':dict(wall=results.timing([75,75,75],'whole-operation'))})
        self.assertEqual(results.ratio(case,True)['value'],.75)
        self.assertIsNone(results.ratio(case,False)['value'])
        case['timing']['candidate']['wall']['profiled']=True
        self.assertIsNone(results.ratio(case,True)['value'])
    def test_unknown_is_distinct_from_measured_zero(self):
        self.assertEqual(counters.metric()['evidence'],'unknown')
        self.assertEqual(counters.metric(0,evidence='measured')['value'],0)
        with self.assertRaises(ValueError):counters.metric(0,evidence='unknown')
    def test_missing_counters_do_not_invent_causal_wait(self):
        groups=counters.kernel_groups(dict(counters={}))
        self.assertIsNone(groups['compute_wait']['exposed_data_wait_pct']['value'])
        self.assertTrue(all(x['value'] is None for x in groups['data_origin'].values()))
    def test_counter_units_and_duplicate_rows_are_checked(self):
        header='ID,Kernel Name,Metric Name,Metric Unit,Metric Value\n'
        row='0,K,gpu__time_duration.sum,ns,1000\n'
        self.assertEqual(counters.parse_ncu(header+row)[0][0]['counters']['duration_ns']['value'],1000)
        for text in (header+row+row,header+row.replace(',ns,',',byte,')):
            with self.assertRaises(ValueError):counters.parse_ncu(text)
