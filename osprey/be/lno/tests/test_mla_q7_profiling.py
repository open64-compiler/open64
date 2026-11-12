"""Synthetic counters and process-boundary tests; no GPU qualification claims."""
import argparse
import copy
import csv
import hashlib
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import mla_q7_profile as profile
import profile_mla_q7 as collector
import run_mla_q7
import validate_mla_q7_results as validator
import test_mla_q7_validation as validation_tests


def csv_fixture(missing=False):
    stream=io.StringIO();writer=csv.writer(stream)
    writer.writerow(['ID','Kernel Name','Metric Name','Metric Unit','Metric Value'])
    for ident,kernel in enumerate(('Prepare','Attention','Finalize')):
        values=dict(duration_ns=1000,issue_active_pct=60,long_scoreboard_pct=75,short_scoreboard_pct=12,
                    eligible_warps=1.5,tensor_active_pct=30,fp32_active_pct=20,hbm_read_bytes=1000,
                    hbm_write_bytes=500,hbm_utilization_pct=50,l2_bytes=2000,l2_utilization_pct=20,
                    nvlink_tx_bytes=0,nvlink_rx_bytes=0,pcie_read_bytes=0,pcie_write_bytes=0)
        for key,(name,unit) in profile.COUNTERS.items():
            writer.writerow([ident,kernel,name,unit,'n/a' if missing and key=='long_scoreboard_pct' else values[key]])
    return stream.getvalue()


class CounterTests(unittest.TestCase):
    def test_recompute_units_denominators_and_no_false_wait_attribution(self):
        result=profile.summarize_ncu(csv_fixture());groups=result['kernels'][1]['groups']
        self.assertEqual(groups['compute_wait']['issue_inactive_pct']['value'],40)
        self.assertEqual(groups['data_paths']['hbm_bytes_per_second']['value'],1.5e9)
        self.assertEqual(groups['compute_wait']['memory_dependency_stall_pct']['evidence'],'proxy')
        self.assertIn('active warp cycles',groups['compute_wait']['memory_dependency_stall_pct']['denominator'])
        for value in groups['data_origin'].values():
            self.assertIsNone(value['value']);self.assertEqual(value['evidence'],'unknown')
        self.assertIsNone(groups['compute_wait']['exposed_data_wait_pct']['value'])
        self.assertIsNone(groups['data_paths']['time_bandwidth_saturated_pct']['value'])
        # Observed zero traffic is measured zero, unlike unknown causal wait.
        self.assertEqual(groups['data_paths']['nvlink_tx_bytes']['value'],0)
        self.assertEqual(groups['data_paths']['nvlink_tx_bytes']['evidence'],'measured')

    def test_unavailable_and_zero_duration_never_become_zero_rate(self):
        result=profile.summarize_ncu(csv_fixture(missing=True));m=result['kernels'][0]['groups']['compute_wait']['memory_dependency_stall_pct']
        self.assertIsNone(m['value']);self.assertEqual(m['evidence'],'unknown')
        launch=profile.parse_ncu(csv_fixture())[0][0];launch['counters']['duration_ns']['value']=0
        self.assertIsNone(profile.kernel_groups(launch)['data_paths']['hbm_bytes_per_second']['value'])

    def test_malformed_duplicate_scaled_and_nonfinite_counters_refuse(self):
        raw=csv_fixture()
        for changed in (raw+raw.splitlines()[1]+'\n',raw.replace('1000','-1',1),raw.replace('1000','inf',1),
                        raw.replace('nsecond','usecond',1),raw.replace(',60',',101',1),raw.replace('1000','1,23',1)):
            with self.subTest(value=changed[:100]),self.assertRaises(ValueError):profile.summarize_ncu(changed)
        with self.assertRaises(ValueError):profile.parse_ncu('tool failed; no CSV')

    def test_capabilities_are_exact_not_guessed(self):
        name=profile.COUNTERS['duration_ns'][0]
        self.assertEqual(profile.available_metrics(name+'\n'+name+'.per_second'),[name])
        self.assertEqual(profile.available_metrics(name+'.per_second'),[])

    def test_network_is_ambient_and_reset_is_unknown(self):
        a=dict(monotonic_ns=100,interfaces=dict(eth0=dict(rx_bytes=10,tx_bytes=20)))
        b=dict(monotonic_ns=1000000100,interfaces=dict(eth0=dict(rx_bytes=110,tx_bytes=10)))
        rows=profile.network_rates(a,b)
        self.assertEqual(rows[0]['measurement']['value'],100)
        self.assertIn('all processes',rows[0]['measurement']['scope'])
        self.assertIsNone(rows[1]['measurement']['value'])
        with self.assertRaises(ValueError):profile.network_rates(a,a)

    def test_missing_tools_preserve_unknowns_and_original_child_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);packet=root/'packet';packet.mkdir()
            (packet/'packet.json').write_text(json.dumps(dict(cases=[dict(id='fixed')])) )
            # Real child process; synthetic protocol fixture, not GPU evidence.
            (packet/'run_mla_q7.py').write_text('raise SystemExit(77)\n')
            args=argparse.Namespace(packet=packet,output=root/'out',expected_sha256='0'*64,
                case=None,device=0,timeout=10,ncu='/__no_ncu__',nsys='/__no_nsys__',timeline=False)
            with patch.object(collector,'authenticate',return_value=2),patch.object(collector.shutil,'which',return_value=None):
                self.assertEqual(collector.collect(args),0)
            report=json.loads((root/'out/report.json').read_text())
            self.assertEqual(report['status'],'INCOMPLETE');self.assertEqual(report['cases'][0]['observation']['returncode'],77)
            self.assertNotIn('summary',report['cases'][0]);self.assertFalse(report['benchmark_samples_emitted'])

    def test_profile_capture_option_rejected_in_timing_lane_before_driver(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(run_mla_q7,'DRIVER') as driver,patch('sys.stdout',new=io.StringIO()):
                result=run_mla_q7.main(['--packet',directory,'--expected-sha256','0'*64,'--output',directory+'/result',
                    '--mode','performance','--capture-profile-range'])
            self.assertEqual(result,1);driver.assert_not_called()

    def test_profile_brackets_one_request_and_never_creates_timing_events(self):
        with tempfile.TemporaryDirectory() as directory:
            args=argparse.Namespace(mode='profile',library=None,packet=Path(directory),case=['fixed'],prefix=256,
                                    device=0,capture_profile_range=True)
            with patch.object(run_mla_q7,'DRIVER') as driver,patch.object(run_mla_q7,'API'),patch.object(run_mla_q7,'SESSION') as session,\
                 patch.object(run_mla_q7,'capture_environment'),patch.object(run_mla_q7,'network_snapshot') as snapshot,\
                 patch('sys.stdout',new=io.StringIO()):
                driver.return_value.info=dict(test_only=True)
                session.return_value.run.return_value=(1,{})
                session.return_value.verify.return_value=[]
                result=run_mla_q7.execute(args,Path(directory),dict(cases=[dict(id='fixed')]))
                self.assertEqual([x.args for x in driver.return_value.profile.call_args_list],[(True,),(False,)])
                driver.return_value.event.assert_not_called()
                self.assertEqual(session.return_value.run.call_count,2)
                self.assertTrue(all(not call.kwargs.get('timed',False) for call in session.return_value.run.call_args_list))
                self.assertEqual(snapshot.call_count,2);self.assertFalse(result['timing_samples_emitted'])
                self.assertNotIn('host_begin_submit_complete_us',result['cases'][0])

    def test_performance_wall_and_event_batches_are_separate(self):
        with tempfile.TemporaryDirectory() as directory:
            args=argparse.Namespace(mode='performance',library=None,packet=Path(directory),case=['fixed'],prefix=256,
                                    device=0,capture_profile_range=False,warmup=0,iterations=2)
            with patch.object(run_mla_q7,'DRIVER') as driver,patch.object(run_mla_q7,'API'),patch.object(run_mla_q7,'SESSION') as session,\
                 patch.object(run_mla_q7,'capture_environment'),patch('sys.stdout',new=io.StringIO()):
                driver.return_value.info=dict(test_only=True)
                session.return_value.run.return_value=(1,dict(host_begin_submit_complete_us=10.,submission_stream_span_ms=.01))
                session.return_value.verify.return_value=[]
                result=run_mla_q7.execute(args,Path(directory),dict(cases=[dict(id='fixed')]))
                self.assertEqual([call.kwargs.get('timed',False) for call in session.return_value.run.call_args_list],
                                 [False,False,False,True,True,False])
                self.assertFalse(result['wall_timing_cuda_events']);self.assertTrue(result['device_timing_separate_batch'])
                driver.return_value.profile.assert_not_called()


class ProfilingValidationTests(unittest.TestCase):
    # Reuse protocol construction helpers, not its test cases.
    fixture=validation_tests.ValidationTests.fixture
    stats=staticmethod(validation_tests.ValidationTests.stats)
    archive=validation_tests.ValidationTests.archive
    review=validation_tests.ValidationTests.review
    rewrite=validation_tests.ValidationTests.rewrite

    def with_profile(self,root):
        source,identity,files,base=self.fixture(root);raw=csv_fixture();entries=[];rows=[]
        folder='results/04-profiling'
        for case in json.loads((source/'packet.json').read_text())['cases']:
            name=case['id'];label=name+'-ncu';csvpath=name+'/metrics.csv';execution=name+'/execution/report.json'
            summary=profile.summarize_ncu(raw)
            row=dict(id=name,status='COLLECTED',ncu=dict(command=label,returncode=0,csv=csvpath,execution=execution,
                native=name+'/counters.ncu-rep',csv_sha256=hashlib.sha256(raw.encode()).hexdigest(),summary=summary),timeline=None)
            child=copy.deepcopy(json.loads(files[base+'/correctness/report.json']))
            child.update(mode='profile',instrumented=True,timing_samples_emitted=False,profile_range_requests=1,
                search_invocations=0,capture_profile_range=True,all_fixed_cases=False,
                cases=[x for x in child['cases'] if x['id']==name and x['prefix_bytes']==256])
            files[folder+'/'+execution]=json.dumps(child).encode();files[folder+'/'+csvpath]=raw.encode()
            files[folder+'/'+name+'/counters.ncu-rep']=b'synthetic native report fixture; not executed'
            a=dict(monotonic_ns=1,interfaces=dict(eth0=dict(rx_bytes=0,tx_bytes=0)))
            b=dict(monotonic_ns=1000000001,interfaces=dict(eth0=dict(rx_bytes=100,tx_bytes=50)))
            for label2,obj in [('network-before',a),('network-after',b)]:files[folder+'/'+name+'/execution/'+label2+'.json']=json.dumps(obj).encode()
            row['ambient_network']=profile.network_rates(a,b);rows.append(row)
            argv=['ncu','--profile-from-start','off','--target-processes','all','--replay-mode','kernel','--cache-control','all',
                '--clock-control','none','--page','raw','--print-units','base','--print-metric-name','name','--print-kernel-base','function','--csv','--print-fp',
                '--metrics',','.join(name for name,unit in profile.COUNTERS.values()),'python3','run_mla_q7.py',
                '--mode','profile','--case',case['id'],'--prefix','256','--expected-sha256',identity,'--capture-profile-range']
            entries.append(dict(label=label,argv=argv,returncode=0))
        for label in ('ncu-version','ncu-query'):entries.append(dict(label=label,argv=['ncu','--version' if label=='ncu-version' else '--query-metrics'],returncode=0))
        files[folder+'/ncu-version.stdout']=b'synthetic ncu version fixture'
        files[folder+'/ncu-query.stdout']='\n'.join(name for name,unit in profile.COUNTERS.values()).encode()
        report=dict(schema=profile.SCHEMA,status='COLLECTED',packet_sha256=identity,separate_process=True,
            benchmark_samples_emitted=False,search_invocations=0,requested_metrics=[name for name,unit in profile.COUNTERS.values()],
            cases=rows,commands=entries)
        files[folder+'/report.json']=json.dumps(report).encode()
        return source,identity,files,base,folder

    def test_valid_profile_is_recomputed_separately_from_timing(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,identity,files,base,folder=self.with_profile(root)
            code,result=self.review(root,source,identity,files)
            self.assertEqual(code,0,result['findings']);self.assertEqual(len(result['performance']),2)
            self.assertEqual(result['sections']['profiling']['status'],'MEASURED_WITH_UNKNOWNS')
            case=result['profiling'][0]['cases'][0]
            self.assertIsNone(case['kernels'][0]['groups']['data_origin']['local_gpu_exposed_wait_pct']['value'])
            text=(root/'review/summary.md').read_text();self.assertIn('unknown / unknown / unknown',text)

    def test_forged_rates_causal_wait_and_evidence_labels_refuse(self):
        for label in ('rate','causality','label','unit','wrong-kernel','extra-kernel','network'):
            with self.subTest(label=label),tempfile.TemporaryDirectory() as directory:
                root=Path(directory);source,identity,files,base,folder=self.with_profile(root)
                def change(value):
                    row=value['cases'][0];groups=row['ncu']['summary']['kernels'][0]['groups']
                    if label=='rate':groups['data_paths']['hbm_bytes_per_second']['value']=42
                    elif label=='causality':groups['data_origin']['peer_gpu_exposed_wait_pct'].update(value=20,evidence='measured')
                    elif label=='label':groups['compute_wait']['memory_dependency_stall_pct']['evidence']='measured'
                    elif label=='unit':groups['data_paths']['hbm_read_bytes']['unit']='GB'
                    elif label=='network':row['ambient_network'][0]['measurement']['value']=999
                    else:
                        name=folder+'/'+row['ncu']['csv'];raw=files[name].decode()
                        raw=raw.replace('Prepare','Other') if label=='wrong-kernel' else raw+raw.splitlines()[1].replace('0,Prepare','3,Prepare')+'\n'
                        files[name]=raw.encode();row['ncu']['csv_sha256']=hashlib.sha256(raw.encode()).hexdigest()
                        row['ncu']['summary']=profile.summarize_ncu(raw)
                self.rewrite(files,folder+'/report.json',change)
                code,result=self.review(root,source,identity,files);self.assertEqual(code,1,result['findings'])
                self.assertIn('PROFILE_EVIDENCE_DIFFERS',{x['code'] for x in result['findings']})

    def test_missing_csv_keeps_numerical_results_and_flags_diagnostic_gap(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);source,identity,files,base,folder=self.with_profile(root)
            # Collection itself must honestly say incomplete when CSV is absent.
            self.rewrite(files,folder+'/report.json',lambda v:v.update(status='INCOMPLETE'))
            for name in list(files):
                if name.endswith('/metrics.csv'):del files[name]
            code,result=self.review(root,source,identity,files)
            self.assertEqual(code,0,result['findings']);self.assertEqual(result['sections']['profiling']['status'],'UNAVAILABLE')
            self.assertEqual(len(result['performance']),2)
            self.assertIn('PROFILE_CSV_MISSING',{x['code'] for x in result['findings']})

    def test_profile_cannot_claim_timing_or_search_isolation_falsely(self):
        for field,value in [('benchmark_samples_emitted',True),('search_invocations',1),('separate_process',False)]:
            with self.subTest(field=field),tempfile.TemporaryDirectory() as directory:
                root=Path(directory);source,identity,files,base,folder=self.with_profile(root)
                self.rewrite(files,folder+'/report.json',lambda v:v.update({field:value}))
                code,result=self.review(root,source,identity,files);self.assertEqual(code,1)
                self.assertIn('PROFILE_ISOLATION',{x['code'] for x in result['findings']})


if __name__=='__main__':unittest.main()
