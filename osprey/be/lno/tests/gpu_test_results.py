"""Shared, dependency-free GPU result contracts and workload adapters.

This module owns reporting arithmetic, never GPU execution or compiler policy.
Workload-specific oracles and Q3/race checks remain with their existing owners.
Controlling contract: doc/GPU-TEST-SHARED-RESULTS-v1.0.md.
"""
import hashlib
import json
import math
import statistics

SCHEMA = 'open64.gpu-test.results.v1'
PROFILES = ('gemm.dense.bf16-fp32.bf16.v1', 'mla.decode.paged.bf16-fp32.v1',
            'mla.quantized.functional-cache.q3.v1',
            'h20-gemm.selected-f32.v1','h20-mla.selected-f32.v1',
            'h20-layernorm.selected-f32.v1','h20-reduction.selected-f32.v1')
SCAFFOLD_WALL = 'enqueue-plus-stream-completion.v1'
SCAFFOLD_DEVICE = 'cuda-event-stream-span.v1'
Q7_WALL = 'q3-begin-submit-complete.separate-wall-batch.v1'
Q7_LEGACY_WALL = 'q3-begin-submit-complete.with-event-recording.v1'
Q7_DEVICE = 'q3-submission-stream-span.with-control-transfers.v1'


def loads(data):
    """Reject ambiguous/nonfinite input before interpreting any evidence."""
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError('duplicate JSON key: '+key)
            result[key] = value
        return result
    def invalid(value):
        raise ValueError('nonfinite JSON constant: '+value)
    def integer(value):
        if len(value)>128:raise ValueError('JSON integer exceeds protocol budget')
        return int(value)
    value = json.loads(data, object_pairs_hook=unique, parse_constant=invalid,parse_int=integer)
    def finite(node):
        if isinstance(node, float) and not math.isfinite(node):
            raise ValueError('nonfinite JSON number')
        if isinstance(node, dict):
            for child in node.values():finite(child)
        elif isinstance(node, list):
            for child in node:finite(child)
    finite(value)
    return value


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(',', ':'), allow_nan=False)


def numeric(value):
    if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value) or value < 0:
        raise ValueError('expected finite nonnegative number')
    return value


def stats(samples):
    if not isinstance(samples, list) or not samples:
        raise ValueError('raw samples required')
    for value in samples:numeric(value)
    ordered = sorted(samples); median = statistics.median(ordered)
    p = (len(ordered)-1)*.95; i = int(p)
    return dict(count=len(samples), median=median,
                mad=statistics.median([abs(x-median) for x in samples]),
                p95=ordered[i]+(ordered[min(i+1,len(ordered)-1)]-ordered[i])*(p-i))


def observation(value=None, unit='ratio', evidence='unknown', reason=None):
    return dict(value=value, unit=unit, evidence=evidence, reason=reason)


def timing(samples, scope):
    if not samples:
        return dict(unit='us', scope=scope, profiled=False, evidence='unknown', samples=[], summary=None,
                    reason='No authenticated raw unprofiled samples available.')
    return dict(unit='us', scope=scope, profiled=False, evidence='measured', samples=list(samples),
                summary=stats(samples), reason=None)


def ratio(case, same_session):
    b = case['timing']['baseline']['wall']; c = case['timing']['candidate']['wall']
    correct = case['correctness']
    if correct.get('baseline_pass') is not True or correct.get('candidate_pass') is not True:
        return observation(reason='Both implementations must pass the same workload correctness contract.')
    if not same_session:
        return observation(reason='No same-device, same-session baseline binding.')
    if b['scope'] != c['scope'] or b['profiled'] or c['profiled']:
        return observation(reason='Timing boundaries or instrumentation differ.')
    if not b['summary'] or not c['summary'] or b['summary']['median'] <= 0:
        return observation(reason='A matched GPU baseline and raw wall samples are unavailable.')
    return observation(c['summary']['median']/b['summary']['median'], evidence='derived')


def from_scaffold(native):
    """Preserve v1 payload while giving raw wall/event batches one shared form."""
    identity = dict(candidate_id=native['submission_id'], baseline_id=native['baseline_id'],
        corpus_sha256=native['corpus_sha256'], session_id=native.get('session_id'),
        same_device_and_session=(native.get('environment') or {}).get('same_device_and_session') is True)
    result = dict(schema=SCHEMA, producer='testing-scaffold', workload_id=native['workload_id'],
        suite_id=native['suite_id'], identity=identity, status=native['status'], environment=native.get('environment'),
        policy=native['policy'], decision=native['decision'], reason=native.get('reason'), cases=[],
        search_invocations=0, extensions=dict(scaffold_v1=native))
    for item in native['cases']:
        record = dict(case_id=item['case_id'], status=item['status'], correctness=item['correctness'],
            useful_flops=observation(item['useful_flops'], 'FLOP', 'declared', native['policy']['flops']),
            timing={}, diagnostics=item.get('diagnostics',dict(status='not_requested')))
        record['diagnostics']=dict(record['diagnostics'],method='ncu_pm_sampling',
            evidence='proxy' if record['diagnostics'].get('status')=='collected' else 'unknown',
            interpretation='Per-kernel aggregate coactivity; not exposed waiting or exact transfer/compute overlap.',
            benchmark_timing_eligible=False)
        for role in ('baseline','candidate'):
            raw = item.get(role) or {}
            record['timing'][role] = dict(wall=timing(raw.get('wall_samples_us'),SCAFFOLD_WALL),
                                         device=timing(raw.get('device_samples_us'),SCAFFOLD_DEVICE))
        record['wall_time_ratio'] = ratio(record,identity['same_device_and_session'])
        record['effective_tflops'] = {}
        for role in ('baseline','candidate'):
            measured = record['timing'][role]['device']['summary']
            record['effective_tflops'][role] = observation(item['useful_flops']/(measured['median']*1e6),
                'TFLOP/second','derived','Useful work / median device-event duration; not executed hardware FLOPs.') if measured and measured['median']>0 else observation(
                unit='TFLOP/second',reason='Device-event samples unavailable.')
        result['cases'].append(record)
    return result


def from_q7(packet, identity, files):
    """Adapt snapshotted native evidence, without replacing the Q7 deep audit.

    files maps archive-relative names to JSON bytes. Unavailable baselines and
    a missing reviewed full-operation FLOP formula remain explicit unknowns.
    """
    def obj(path):
        raw=files.get(path)
        return loads(raw) if raw is not None else None
    roots=[]
    for path in sorted(files):
        if path.startswith('results/') and path.count('/')==2 and path.endswith('/report.json'):
            root=path.rsplit('/',1)[0]; value=obj(path)
            if isinstance(value,dict) and (value.get('mode')=='all' or root+'/correctness/report.json' in files):roots.append(root)
    root=roots[0] if len(roots)==1 else None
    parent=obj(root+'/report.json') if root else None
    perf=obj(root+'/performance/report.json') if root else None
    correct=obj(root+'/correctness/report.json') if root else None
    profile=[]
    for path in sorted(files):
        if path.endswith('/report.json') and path.count('/')==2:
            value=obj(path)
            if isinstance(value,dict) and value.get('schema')=='open64.mla-q7.profiling.v1':
                profile.append(dict(report=path,method='ncu_counter_replay',payload=value))
    separated=(perf or {}).get('wall_timing_cuda_events') is False
    result=dict(schema=SCHEMA,producer='mla-q7',workload_id=PROFILES[2],suite_id='mla.quantized.q7.fixed.v1',
        identity=dict(candidate_id=identity,baseline_id=None,corpus_sha256=identity,session_id=root,same_device_and_session=False),
        status='completed' if parent and parent.get('status')=='PASS' else 'incomplete',environment=(perf or correct or {}).get('device'),
        policy=dict(wall_scope=Q7_WALL if separated else Q7_LEGACY_WALL,
                    device_scope=Q7_DEVICE,profiling_separate=True,numerical_contract='Q7-six-outputs-three-oracles-Q3.v1'),
        decision='inconclusive',reason='No matched GPU baseline; no performance acceptance or speedup claim.',
        cases=[],search_invocations=0,extensions=dict(q7=dict(selected_run=root,qualification=parent,
            correctness=correct,performance=perf,profiling=profile)))
    for source in packet['cases']:
        name=source['id'];matches=[x for x in (correct or {}).get('cases',[]) if x.get('id')==name]
        diagnostic_rows=[row for entry in profile for row in entry['payload'].get('cases',[]) if row.get('id')==name]
        diagnostic_status='collected' if any(row.get('status')=='COLLECTED' for row in diagnostic_rows) else 'incomplete' if diagnostic_rows else 'not_collected'
        checked=bool(matches) and {x.get('prefix_bytes') for x in matches}=={40,256} and all(x.get('status')=='PASS' for x in matches)
        rows=[x for x in (perf or {}).get('cases',[]) if x.get('id')==name and x.get('prefix_bytes')==256]
        measured=rows[0] if len(rows)==1 and perf.get('status')=='PASS' else {}
        row=dict(case_id=name,status='passed' if checked else 'not_run',
            correctness=dict(candidate_pass=True if checked else None,baseline_pass=None,
                contract='six outputs; selected-source/PyTorch/scalar; both address residues; Q3 publication/lifetimes'),
            useful_flops=observation(unit='FLOP',reason='Full quantized MLA boundary includes projections/cache updates; reviewed useful-FLOP formula not yet defined.'),
            timing=dict(baseline=dict(wall=timing(None,result['policy']['wall_scope']),device=timing(None,Q7_DEVICE)),
                candidate=dict(wall=timing((measured.get('host_begin_submit_complete_us') or {}).get('samples'),result['policy']['wall_scope']),
                    device=timing([x*1000 for x in measured.get('submission_stream_span_ms',{}).get('samples',[])],Q7_DEVICE))),
            diagnostics=dict(status=diagnostic_status,methods=['ncu_counter_replay'] if diagnostic_rows else [],
                             benchmark_timing_eligible=False))
        row['wall_time_ratio']=ratio(row,False)
        result['cases'].append(row)
    return result


def validate_pm(data):
    """Validate an exported PM timeline without importing vendor code.

    Samples are per-kernel aggregate proxies, never causal exposed-wait ratios.
    Native report attestation remains outside an offline JSON consistency audit.
    """
    if data.get('synthetic_fixture') or data.get('method')!='ncu_pm_sampling' or data.get('single_pass_verified') is not True or not data.get('kernels'):
        raise ValueError('PM single-pass proof missing')
    for kernel in data['kernels']:
        if kernel.get('pass_groups')!=1 or kernel.get('collection_passes')!=1:
            raise ValueError('PM metrics came from multiple/unproved passes')
        start=numeric(kernel['workload_start_ns']);end=numeric(kernel['workload_end_ns'])
        if end<=start or not (numeric(kernel['interval_ns']) or numeric(kernel['interval_cycles'])):
            raise ValueError('invalid PM bounds/interval')
        for label in ('memory','compute'):
            channel=kernel['channels'][label]
            if channel['metric'] not in kernel['pass_metrics']:
                raise ValueError('PM channel not in the common pass')
            times=[]
            for sample in channel['samples']:
                times.append(numeric(sample['timestamp_ns']));numeric(sample['value'])
            if times!=sorted(times) or len(set(times))!=len(times):
                raise ValueError('PM timestamps are unsorted or duplicated')
            if channel['in_kernel_samples']!=sum(start<=x<=end for x in times):
                raise ValueError('PM sample count differs')
        # Missing merge/drop counts remain null, never rewritten as zero.
        for key in ('merged_samples','dropped_samples'):
            if kernel.get(key) is not None:numeric(kernel[key])


def validate(result):
    """Common structural/arithmetic validation; workload audits are additional."""
    issues=[]
    def flag(level,code,message):issues.append(dict(severity=level,code=code,message=message))
    if result.get('schema')!=SCHEMA or result.get('workload_id') not in PROFILES:
        raise ValueError('unsupported shared result schema/workload')
    if result.get('search_invocations')!=0:
        flag('ERROR','SEARCH_ISOLATION','Benchmark reporting must not invoke compiler search.')
    if result.get('status') in ('build_failed','execution_failed'):
        flag('ERROR','REPORTED_FAILURE',result.get('reason') or result['status'])
    ids=set();same=result['identity']['same_device_and_session']
    for case in result['cases']:
        name=case['case_id']
        if name in ids:flag('ERROR','DUPLICATE_CASE',name)
        ids.add(name)
        for role in ('baseline','candidate'):
            for kind in ('wall','device'):
                record=case['timing'][role][kind]
                if record.get('profiled') is not False or record.get('unit')!='us':
                    flag('ERROR','TIMING_ISOLATION',name+' '+role+' '+kind)
                if record.get('evidence')=='measured':
                    if canonical(stats(record['samples']))!=canonical(record['summary']):flag('ERROR','TIMING_SUMMARY',name)
                    if not record.get('scope'):flag('ERROR','TIMING_SCOPE',name)
                elif record.get('evidence')!='unknown' or record.get('samples') or record.get('summary') is not None:
                    flag('ERROR','UNKNOWN_TIMING',name)
        expected=ratio(case,same)
        if canonical(expected)!=canonical(case['wall_time_ratio']):flag('ERROR','RATIO_DIFFERS',name+'; direction is candidate/baseline')
        flops=case['useful_flops']
        if flops['evidence']=='unknown':
            if flops['value'] is not None or not flops.get('reason'):flag('ERROR','UNKNOWN_FLOPS',name)
        elif flops['evidence']!='declared' or type(flops['value']) is not int or flops['value']<0:
            flag('ERROR','USEFUL_FLOPS',name)
        if case['correctness'].get('candidate_pass') is not True:
            flag('MISSING' if case['correctness'].get('candidate_pass') is None else 'ERROR','CORRECTNESS',name)
        if case['correctness'].get('baseline_pass') is False:flag('ERROR','BASELINE_CORRECTNESS',name)
    if result.get('decision')=='accepted' and (result['status']!='completed' or not ids or any(x['wall_time_ratio']['value'] is None for x in result['cases'])):
        flag('ERROR','ACCEPTANCE_WITHOUT_EVIDENCE','Acceptance needs complete, correct, comparable unprofiled samples.')
    if not ids:flag('MISSING','NO_CASES','No cases were reported.')
    levels={x['severity'] for x in issues}
    return dict(schema='open64.gpu-test.validation.v1',overall='FAIL' if 'ERROR' in levels else 'INCOMPLETE' if 'MISSING' in levels else 'PASS_REPORTED',
                workload_id=result['workload_id'],cases=len(ids),findings=issues,
                scope='Reported evidence consistency; no independent GPU rerun.',gpu_workloads_launched=0,search_invocations=0)


def markdown(result,review):
    def safe(value):
        text=str(value).replace('&','&amp;').replace('<','&lt;').replace('>','&gt;').replace('|','\\|').replace('\n',' ')
        for char in ('`','[',']','*','_'):text=text.replace(char,'\\'+char)
        return text
    def value(item):
        return 'unknown' if item['value'] is None else '%g (%s)'%(item['value'],item['evidence'])
    lines=['# Shared GPU results review','','**'+review['overall']+'** — '+safe(result['workload_id']),'',review['scope'],'',
        '| Case | Correctness | Useful FLOPs | Candidate / baseline wall ratio |','| --- | --- | --- | --- |']
    for case in result['cases']:
        lines.append('| '+safe(case['case_id'])+' | '+safe(case['correctness'].get('candidate_pass'))+' | '+value(case['useful_flops'])+' | '+value(case['wall_time_ratio'])+' |')
    if result.get('reason'):lines+=['',safe(result['reason'])]
    lines+=['','| Measurement | Scope |','| --- | --- |']
    scopes={(kind,case['timing']['candidate'][kind]['scope']) for case in result['cases'] for kind in ('wall','device')}
    for kind,scope in sorted(scopes):lines.append('| '+kind+' | '+safe(scope)+' |')
    lines+=['','Wall and device-event batches have explicit, distinct scopes. Profiler durations never enter either. '
            'Unknown baselines/FLOPs do not become estimated measurements. Diagnostic methods retain their own replay/pass policies.','',
            '## Findings','']
    for item in review['findings']:lines.append('- **'+item['severity']+' / '+item['code']+'**: '+safe(item['message']))
    if not review['findings']:lines.append('No inconsistency detected in the supplied reported evidence.')
    return '\n'.join(lines)+'\n'
