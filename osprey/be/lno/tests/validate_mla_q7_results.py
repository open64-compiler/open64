"""Inspect a returned Q7 tar without extraction, execution or GPU dependencies.

Verify transport/file integrity and trusted source identity, then summarize
reported correctness, sanitizer and timing evidence. PASS_REPORTED is not an
independent GPU rerun or transfer approval. Standard library, Python >=3.8.
Contract: doc/AI-COMPILER-MLA-Q7-RESULTS-VALIDATION-v1.0.md.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path, PurePosixPath
import re
import struct
import sys
import tarfile
from mla_q7_profile import SCHEMA as PROFILE_SCHEMA, COUNTERS, available_metrics, summarize_ncu, network_rates, kernel_groups, parse_ncu

ROOT = 'Open64_MLA_Q7_results/'
TOOLS = ('memcheck', 'racecheck', 'synccheck', 'initcheck')
LANES = ('selected-source', 'pytorch', 'scalar')
FLAGS = ('complete_Q3_tuple', 'consumer_completion', 'input_immutability', 'allocation_guards', 'prepared_cancel')
UNAVAILABLE = ('GPU_UNAVAILABLE', 'TOOLS_UNAVAILABLE', 'TOOL_COVERAGE_INCOMPLETE')


def digest_file(path):
    digest = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda: stream.read(1048576), b''): digest.update(block)
    return digest.hexdigest()


def manifest(data):
    result = {}
    for row in data.decode('utf-8').splitlines():
        digest, name = row.split('  ', 1)
        if not re.fullmatch('[0-9a-f]{64}', digest) or name in result or not safe_name(name):
            raise ValueError('invalid/duplicate manifest entry: ' + name)
        result[name] = digest
    return result


def safe_name(name):
    return bool(name) and not name.startswith('/') and not re.search(r'[\\\x00-\x1f\x7f]', name) and all(x not in ('', '.', '..') for x in name.split('/'))


def load_json(data):
    def reject(value): raise ValueError('nonfinite JSON constant: ' + value)
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result: raise ValueError('duplicate JSON key: ' + key)
            result[key] = value
        return result
    def finite(value):
        number=float(value)
        if not math.isfinite(number):raise ValueError('nonfinite JSON number')
        return number
    def integer(value):
        if len(value)>128:raise ValueError('JSON integer exceeds protocol budget')
        return int(value)
    return json.loads(data, parse_constant=reject, parse_float=finite, parse_int=integer, object_pairs_hook=unique)


class Archive:
    """Single streaming pass hashes all files; only bounded text is retained."""
    def __init__(self, path, expected, max_bytes, max_members, max_text, max_retained=256*1024**2, root_prefix=ROOT):
        if not root_prefix.endswith('/') or not safe_name(root_prefix[:-1]):raise ValueError('invalid archive root prefix')
        self.digest = digest_file(path)
        if self.digest != expected: raise ValueError('outer archive SHA-256 differs')
        self.hashes = {}; self.texts = {}; self.oversize = []; kinds = {}; parents = set(); total = retained = 0
        with tarfile.open(str(path), 'r|*') as tar:
            for count, item in enumerate(tar, 1):
                name = item.name.rstrip('/') if item.isdir() else item.name
                if count > max_members or not safe_name(name) or not (name.startswith(root_prefix) or name==root_prefix.rstrip('/') and item.isdir()):
                    raise ValueError('unsafe archive member or member limit: ' + name)
                if name in kinds or not (item.isdir() or item.isfile()) or item.sparse:
                    raise ValueError('duplicate/link/special/sparse archive member: ' + name)
                if item.isfile() and name in parents: raise ValueError('file/directory conflict: ' + name)
                for parent in PurePosixPath(name).parents:
                    p = str(parent)
                    if kinds.get(p) == 'file': raise ValueError('file used as directory: ' + p)
                    parents.add(p)
                kinds[name] = 'dir' if item.isdir() else 'file'
                if item.isdir(): continue
                total += item.size
                if item.size < 0 or total > max_bytes: raise ValueError('expanded archive exceeds byte budget')
                relative = name[len(root_prefix):]; keep = relative == 'SHA256SUMS' or name.endswith(('.json','.stdout','.stderr','.txt','.md','.csv')) or relative == 'packet/PACKET_ID' or relative == 'packet/SHA256SUMS'
                if keep and (item.size > max_text or retained+item.size > max_retained):
                    self.oversize.append(relative); keep = False
                if keep:retained+=item.size
                digest = hashlib.sha256(); parts = []
                stream = tar.extractfile(item)
                for block in iter(lambda: stream.read(1048576), b''):
                    digest.update(block)
                    if keep: parts.append(block)
                self.hashes[relative] = digest.hexdigest()
                if keep: self.texts[relative] = b''.join(parts)
        if 'SHA256SUMS' not in self.texts: raise ValueError('missing or oversized return manifest')
        expected_files = manifest(self.texts['SHA256SUMS'])
        if set(expected_files) != set(self.hashes)-{'SHA256SUMS'}:
            raise ValueError('return manifest has missing/unlisted files')
        for name, digest in expected_files.items():
            if self.hashes[name] != digest: raise ValueError('return file SHA-256 differs: ' + name)
        self.total_bytes = total


class Source:
    """Authenticate the source manifest and each metadata/oracle file consumed."""
    def __init__(self, root, expected):
        self.root = root.resolve(); self.identity = expected
        if (self.root/'SHA256SUMS').stat().st_size>32*1024**2:raise ValueError('source manifest exceeds metadata budget')
        raw = (self.root/'SHA256SUMS').read_bytes()
        if hashlib.sha256(raw).hexdigest() != expected: raise ValueError('trusted source packet identity differs')
        self.files = manifest(raw); self.checked = set(); self.packet = load_json(self.read('packet.json'))
        if self.packet.get('schema') != 'open64.mla-q7.packet.v1' or self.packet.get('architecture') != 'sm_90' or self.packet.get('ptx_isa') != '8.6':
            raise ValueError('unsupported source qualification contract')
        self.cases = {}
        for case in self.packet['cases']:
            key = case['id']
            if not re.fullmatch('[A-Za-z0-9_.-]+', key) or key in self.cases or len(case['outputs']) != 6:
                raise ValueError('invalid trusted case inventory')
            self.cases[key] = case
        if not self.cases: raise ValueError('empty source case inventory')

    def read(self, name):
        path = self.root/name
        if not safe_name(name) or name not in self.files or path.is_symlink() or self.root not in path.resolve().parents:
            raise ValueError('unsafe/unlisted source file: ' + name)
        if path.stat().st_size>32*1024**2:raise ValueError('source metadata/oracle exceeds 32 MiB budget: '+name)
        data = path.read_bytes()
        if hashlib.sha256(data).hexdigest() != self.files[name]: raise ValueError('source file differs: ' + name)
        self.checked.add(name); return data

    def error_bound(self, case, output, reference):
        """Necessary aggregate-error bound only; raw GPU outputs are not returned."""
        rtol, atol = output['rtol'], output['atol']
        if rtol == atol == 0: return 0.0
        name = reference['file']
        if not safe_name(name): raise ValueError('unsafe source reference name')
        data = self.read('cases/'+case['id']+'/'+name)
        code = {'float32':'f', 'float64':'d'}[reference['dtype']]
        values = (x[0] for x in struct.iter_unpack('<'+code, data))
        return max((atol+rtol*abs(x) for x in values if math.isfinite(x)), default=atol)


class Review:
    def __init__(self, archive, source):
        self.a, self.s = archive, source; self.findings = []; self.devices = []; self.performance = []; self.correctness_errors = []; self.profiling = []

    def flag(self, severity, code, path, message):
        entry = dict(severity=severity, code=code, path=path, message=message)
        if entry not in self.findings: self.findings.append(entry)

    def status(self, start):
        levels = {x['severity'] for x in self.findings[start:]}
        return 'FAIL' if 'ERROR' in levels else 'INCOMPLETE' if 'MISSING' in levels else 'PASS_REPORTED'

    def text(self, path, required=True):
        data = self.a.texts.get(path)
        if data is None:
            if required: self.flag('MISSING','MISSING_FILE',path,'Required evidence is absent or exceeds text budget.')
            return None
        return data.decode('utf-8', errors='replace')

    def obj(self, path, required=True):
        text = self.text(path, required)
        if text is None: return None
        try:
            value = load_json(self.a.texts[path])
            if not isinstance(value, dict): raise ValueError('report is not an object')
            return value
        except (ValueError, TypeError, RecursionError) as error:
            self.flag('ERROR','INVALID_JSON',path,str(error)); return None

    def packet_binding(self, report, path):
        bound = report.get('packet_sha256')
        if bound is None: self.flag('MISSING','MISSING_REPORT_BINDING',path,'Report does not record its source packet identity.')
        elif bound != self.s.identity: self.flag('ERROR','PACKET_MISMATCH',path,'Report belongs to a different source packet.')

    def device(self, report, path):
        device = report.get('device')
        if not isinstance(device,dict):
            self.flag('MISSING','MISSING_DEVICE',path,'Actual execution GPU/Driver facts are absent.'); return
        self.devices.append(dict(path=path, facts=device))
        if device.get('compute_capability') != [9,0]: self.flag('ERROR','GPU_INCOMPATIBLE',path,'Fixed sm_90 code requires compute capability 9.0.')
        version = device.get('driver_api_version')
        if not isinstance(version,int): self.flag('MISSING','MISSING_DRIVER_API',path,'Driver API version is not recorded.')
        elif version < 12080: self.flag('ERROR','DRIVER_INCOMPATIBLE',path,'Q6d requires CUDA Driver API >=12.8.')
        if not isinstance(device.get('uuid'),str) or not device['uuid']: self.flag('MISSING','MISSING_GPU_UUID',path,'Cannot identify the execution device.')
        platform = device.get('platform','')
        if not isinstance(platform,str) or not platform: self.flag('MISSING','MISSING_PLATFORM',path,'Execution platform is absent.')
        elif not platform.startswith('Linux') or not any(x in platform for x in ('x86_64','AMD64')):
            self.flag('ERROR','PLATFORM_INCOMPATIBLE',path,'Delivered native binding requires Linux x86_64.')

    def report_state(self, report, path):
        if report.get('status') in UNAVAILABLE:
            self.flag('MISSING',report['status'],path,str(report.get('reason','Execution/tool unavailable.'))); return False
        if report.get('status') != 'PASS':
            self.flag('ERROR','REPORTED_FAILURE',path,str(report.get('reason',report.get('status','missing status')))); return False
        if report.get('gpu_execution') is not True or report.get('driver_model') is not False:
            self.flag('ERROR','NOT_REAL_GPU_EVIDENCE',path,'PASS must record actual GPU execution and driver_model=false.')
        if report.get('authenticated_files')!=len(self.s.files):self.flag('MISSING','SOURCE_AUTHENTICATION_COUNT',path,'Source file-authentication count differs from the trusted packet.')
        self.packet_binding(report,path); self.device(report,path); return True

    def stats(self, metric, path):
        if not isinstance(metric,dict) or not isinstance(metric.get('samples'),list) or not metric['samples']:
            self.flag('MISSING','MISSING_RAW_SAMPLES',path,'Raw performance samples are required.'); return None
        values = metric['samples']
        if any(isinstance(x,bool) or not isinstance(x,(int,float)) or not math.isfinite(x) or x<0 for x in values):
            self.flag('ERROR','INVALID_TIMINGS',path,'Timing samples must be finite and nonnegative.'); return None
        ordered=sorted(values)
        def percentile(q):
            p=(len(ordered)-1)*q; left=int(p); right=min(left+1,len(ordered)-1)
            return ordered[left]+(ordered[right]-ordered[left])*(p-left)
        mean=sum(values)/len(values)
        computed=dict(count=len(values),minimum=min(values),median=percentile(.5),p95=percentile(.95),mean=mean,
                      standard_deviation=math.sqrt(sum((x-mean)**2 for x in values)/len(values)))
        for key,value in computed.items():
            actual=metric.get(key)
            if isinstance(actual,bool) or not isinstance(actual,(int,float)) or not math.isfinite(actual) or not math.isclose(actual,value,rel_tol=1e-9,abs_tol=1e-12):
                self.flag('ERROR','TIMING_SUMMARY_DIFFERS',path,'Stored '+key+' differs from independently recomputed samples.')
        if len(values)<2: self.flag('WARNING','SINGLE_TIMING_SAMPLE',path,'One sample cannot characterize timing variability.')
        return computed

    def cases(self, report, path, performance=False, profile_case=None):
        if report.get('mode')!=('profile' if profile_case else 'performance' if performance else 'correctness'):self.flag('ERROR','REPORT_MODE_DIFFERS',path,'Report mode differs from its evidence stage.')
        records=report.get('cases')
        if not isinstance(records,list) or not records:
            self.flag('MISSING','MISSING_CASES',path,'No executed cases.'); return {}
        found={}
        for case in records:
            if not isinstance(case,dict): self.flag('ERROR','INVALID_CASE',path,'Case record is not an object.');continue
            key=(case.get('id'),case.get('prefix_bytes'))
            if key in found: self.flag('ERROR','DUPLICATE_CASE',path,str(key));continue
            found[key]=case
            if key[0] not in self.s.cases or key[1] not in (40,256): self.flag('ERROR','UNKNOWN_CASE',path,str(key));continue
            source=self.s.cases[key[0]]
            if case.get('status')!='PASS' or case.get('ordered_kernels_per_request')!=3 or any(case.get(x) is not True for x in FLAGS):
                self.flag('ERROR','CASE_CONTRACT_FAILED',path,str(key)+' lacks required Q3/reference/lifetime checks.')
            if key[0]=='dense_s8_decode--async' and case.get('source_guard_refusal') is not True:
                self.flag('ERROR','MISSING_SOURCE_GUARD',path,'Dense async source-guard refusal was not checked.')
            refs=case.get('references')
            if not isinstance(refs,list) or len(refs)!=6:
                self.flag('MISSING','MISSING_REFERENCES',path,str(key)+' needs six outputs and all three reference lanes.');continue
            for ordinal,(metrics,output) in enumerate(zip(refs,source['outputs'])):
                if not isinstance(metrics,dict) or set(metrics)!=set(LANES):
                    self.flag('MISSING','MISSING_REFERENCE_LANE',path,str(key)+' output '+str(ordinal));continue
                elements=math.prod(output['shape'])
                for ref in output['references']:
                    metric=metrics[ref['name']]
                    if not isinstance(metric,dict) or metric.get('elements')!=elements:
                        self.flag('ERROR','REFERENCE_EXTENT_DIFFERS',path,str(key)+' output '+str(ordinal));continue
                    errors=[metric.get(x) for x in ('max_absolute_error','max_relative_error','rms_error')]
                    if any(isinstance(x,bool) or not isinstance(x,(int,float)) or not math.isfinite(x) or x<0 for x in errors):
                        self.flag('ERROR','INVALID_REFERENCE_ERRORS',path,str(key));continue
                    bound=self.s.error_bound(source,output,ref)
                    if errors[0]>bound+1e-15 or errors[2]>errors[0]+1e-15 or bound==0 and any(errors):
                        self.flag('ERROR','REFERENCE_ERROR_OUT_OF_BOUNDS',path,str(key)+' output '+str(ordinal))
            if not performance and not profile_case:
                aggregate={}
                for lane in LANES:
                    valid=[item.get(lane) for item in refs if isinstance(item,dict) and isinstance(item.get(lane),dict)]
                    valid=[item for item in valid if all(isinstance(item.get(x),(int,float)) and not isinstance(item.get(x),bool) and math.isfinite(item[x]) and item[x]>=0 for x in ('max_absolute_error','max_relative_error','rms_error','elements'))]
                    aggregate[lane]=dict(outputs_with_statistics=len(valid),elements=sum(item['elements'] for item in valid),
                        max_absolute_error=max((item['max_absolute_error'] for item in valid),default=None),
                        max_relative_error=max((item['max_relative_error'] for item in valid),default=None),
                        peak_output_rms_error=max((item['rms_error'] for item in valid),default=None))
                self.correctness_errors.append(dict(report=path,case=key[0],prefix_bytes=key[1],references=aggregate))
            if performance:
                host=self.stats(case.get('host_begin_submit_complete_us'),path+' '+str(key)+' host')
                span=self.stats(case.get('submission_stream_span_ms'),path+' '+str(key)+' stream')
                if host and span:
                    if host['count']!=span['count']:self.flag('ERROR','TIMING_COUNTS_DIFFER',path,str(key))
                    self.performance.append(dict(case=key[0],prefix_bytes=key[1],geometry=source.get('source_geometry'),host_us=host,submission_stream_ms=span))
        expected={(case,prefix) for case in self.s.cases for prefix in ((256,) if performance else (256,40))}
        if profile_case:
            expected={(profile_case,256)}
            if set(found)-expected:self.flag('ERROR','PROFILE_CAPTURE_CASES',path,'Profile child ran extra cases/residues.')
        missing=expected-set(found)
        if missing:self.flag('MISSING','CASE_COVERAGE',path,'Missing '+str(len(missing))+' case/residue pairs: '+str(sorted(missing)))
        if not profile_case and report.get('all_fixed_cases') is not True:self.flag('MISSING','SUBSET_RUN',path,'Report does not claim all fixed products.')
        return found

    def profile_run(self, root):
        """Recompute diagnostics from raw CSV without granting causal inference.

        Missing optional counters are diagnostic incompleteness, independent of
        numerical/timing qualification. Contradictory or fabricated summaries
        are evidence errors and fail the review.
        """
        path=root+'/report.json'; report=self.obj(path); start=len(self.findings)
        result=dict(status='UNAVAILABLE',report=path,cases=[],benchmark_samples_used=False)
        if not report:return result
        if report.get('schema')!=PROFILE_SCHEMA:
            self.flag('ERROR','PROFILE_SCHEMA',path,'Unknown profiling schema.');return dict(result,status='INVALID')
        self.packet_binding(report,path)
        if report.get('separate_process') is not True or report.get('benchmark_samples_emitted') is not False or report.get('search_invocations')!=0:
            self.flag('ERROR','PROFILE_ISOLATION',path,'Profiling must be separate from timing and compiler search.')
        entries=report.get('commands'); commands={}
        if not isinstance(entries,list):
            self.flag('ERROR','PROFILE_COMMANDS',path,'Profiling command inventory is absent.');entries=[]
        for entry in entries:
            if not isinstance(entry,dict) or not isinstance(entry.get('label'),str) or entry['label'] in commands:
                self.flag('ERROR','PROFILE_COMMANDS',path,'Invalid or duplicate profiling command.');continue
            commands[entry['label']]=entry
        query=self.text(root+'/ncu-query.stdout',False)
        available=set(available_metrics(query or ''))
        requested=report.get('requested_metrics',[])
        if not isinstance(requested,list) or any(not isinstance(x,str) for x in requested) or len(set(requested))!=len(requested) or set(requested)!=available:
            self.flag('ERROR','PROFILE_METRIC_CAPABILITY',path,'Requested metrics differ from captured capability query.');requested=[]
        if requested:
            version=self.text(root+'/ncu-version.stdout',False) or self.text(root+'/ncu-version.stderr',False)
            if not version or commands.get('ncu-version',{}).get('returncode')!=0 or commands.get('ncu-query',{}).get('returncode')!=0:
                self.flag('WARNING','PROFILE_TOOL_VERSION',path,'Successful NCU version/capability evidence is missing.')
        rows=report.get('cases',[]); seen=set(); measured=0
        if not isinstance(rows,list):
            self.flag('ERROR','PROFILE_CASES',path,'Invalid profiling case list.');rows=[]
        def relative(name):
            if not isinstance(name,str) or not safe_name(name):raise ValueError('unsafe profiling evidence path')
            return root+'/'+name
        for row in rows:
            if not isinstance(row,dict):
                self.flag('ERROR','PROFILE_CASES',path,'Invalid profiling case.');continue
            name=row.get('id')
            if not isinstance(name,str) or name not in self.s.cases or name in seen:
                self.flag('ERROR','PROFILE_CASES',path,'Unknown or duplicate profile case.');continue
            seen.add(name); summary=dict(id=name,status='UNAVAILABLE',kernels=[],ambient_network=[],
                groups={key:dict(status='UNKNOWN') for key in ('compute_wait','data_paths','data_origin')})
            summary['unknown_measurements']=kernel_groups(dict(counters={}))
            result['cases'].append(summary)
            for kind in ('ncu','timeline','observation'):
                evidence=row.get(kind)
                if evidence is None:continue
                try:
                    if not isinstance(evidence,dict):raise ValueError('invalid profiling evidence')
                    command=commands.get(evidence.get('command'),{}); argv=command.get('argv')
                    if not isinstance(argv,list) or not all(isinstance(x,str) for x in argv):raise ValueError('invalid command argv')
                    if command.get('returncode')!=evidence.get('returncode'):raise ValueError('tool and case exit codes differ')
                    def option(key,value):
                        return argv.count(key)==1 and argv.index(key)+1<len(argv) and argv[argv.index(key)+1]==value
                    for key,value in (('--mode','profile'),('--expected-sha256',self.s.identity),('--case',name),('--prefix','256')):
                        if not option(key,value):raise ValueError('profile command binding differs: '+key)
                    if kind in ('ncu','timeline') and argv.count('--capture-profile-range')!=1:raise ValueError('uncaptured profile request')
                    if kind=='ncu':
                        if Path(argv[0]).name!='ncu':raise ValueError('not an NCU command')
                        for key,value in (('--profile-from-start','off'),('--target-processes','all'),('--replay-mode','kernel'),
                                          ('--cache-control','all'),('--clock-control','none'),('--page','raw'),('--print-units','base'),('--print-metric-name','name'),('--print-kernel-base','function')):
                            if not option(key,value):raise ValueError('NCU capture policy differs: '+key)
                        if argv.count('--metrics')!=1 or set(argv[argv.index('--metrics')+1].split(','))!=set(requested):raise ValueError('NCU metric command differs')
                        if '--csv' not in argv or '--print-fp' not in argv:raise ValueError('NCU raw CSV policy absent')
                    if evidence.get('returncode')!=0:
                        self.flag('WARNING','PROFILE_TOOL_FAILED',path,name+' '+kind+' exit '+str(evidence.get('returncode')))
                        if 'summary' in evidence:raise ValueError('failed profile must not assert measured summary')
                        continue
                    childpath=relative(evidence['execution']); child=self.obj(childpath)
                    if child is None:continue
                    if child.get('mode')!='profile' or child.get('instrumented') is not (kind!='observation') or child.get('timing_samples_emitted') is not False or child.get('profile_range_requests')!=1 or child.get('search_invocations')!=0:
                        raise ValueError('child profiling isolation/range contract differs')
                    if kind in ('ncu','timeline') and child.get('capture_profile_range') is not True:raise ValueError('child capture range was not enabled')
                    childstart=len(self.findings)
                    if not self.report_state(child,childpath):raise ValueError('successful profiler lacks a successful real-GPU child')
                    self.cases(child,childpath,profile_case=name)
                    if any(x['severity'] in ('ERROR','MISSING') for x in self.findings[childstart:]):raise ValueError('profile child correctness/binding evidence is incomplete')
                    for item in child.get('cases',[]):
                        if any(key in item for key in ('host_begin_submit_complete_us','submission_stream_span_ms')):raise ValueError('profile child emitted timing samples')
                    if kind in ('ncu','timeline') and relative(evidence['native']) not in self.a.hashes:
                        self.flag('WARNING','PROFILE_NATIVE_MISSING',path,'Native profiler report is missing: '+name+' '+kind)
                    if kind=='ncu':
                        csvpath=relative(evidence['csv']); raw=self.text(csvpath,False)
                        if raw is None:
                            self.flag('WARNING','PROFILE_CSV_MISSING',path,'Raw counter CSV absent or over budget: '+name);continue
                        if hashlib.sha256(self.a.texts[csvpath]).hexdigest()!=evidence.get('csv_sha256'):raise ValueError('raw counter CSV identity differs')
                        recomputed=summarize_ncu(raw)
                        if json.dumps(recomputed,sort_keys=True)!=json.dumps(evidence.get('summary'),sort_keys=True):raise ValueError('counter summary/evidence labels differ from independently recomputed raw CSV')
                        launches,_=parse_ncu(raw)
                        if not any(value is not None for launch in launches for value in launch['counters'].values()):raise ValueError('no numerical hardware counter values were captured')
                        for launch in launches:
                            present={COUNTERS[key][0] for key in launch['counters']}
                            if present-set(requested):raise ValueError('unrequested counters in raw CSV')
                            if set(requested)-present:self.flag('WARNING','PROFILE_COUNTER_COVERAGE',csvpath,'Some requested counters are absent; they remain unknown.')
                        trusted=load_json(self.s.read('cases/'+name+'/code.json'))
                        expected=[x['function'] for x in trusted['stages']]
                        actual=[x['kernel'].split('(')[0].strip() for x in recomputed['kernels']]
                        if len(expected)!=3 or actual!=expected:raise ValueError('captured launches differ from the three selected WHIRL stage entries/order')
                        summary.update(status='MEASURED_WITH_UNKNOWNS',kernels=recomputed['kernels'],
                            groups=dict(compute_wait=dict(status='PROXIES_AND_UNKNOWN_EXPOSED_WAIT'),
                                        data_paths=dict(status='COUNTERS_WITH_UNKNOWNS'),data_origin=dict(status='UNKNOWN_WAIT_ATTRIBUTION')))
                        measured+=1
                    if kind in ('ncu','observation'):
                        folder=childpath.rsplit('/',1)[0]
                        before=self.obj(folder+'/network-before.json',False);after=self.obj(folder+'/network-after.json',False)
                        if before is not None and after is not None:
                            recomputed=network_rates(before,after)
                            if json.dumps(recomputed,sort_keys=True)!=json.dumps(row.get('ambient_network'),sort_keys=True):raise ValueError('network rate/classification differs from snapshots')
                            summary['ambient_network']=recomputed
                            if not before.get('interfaces') or not after.get('interfaces'):self.flag('WARNING','PROFILE_NETWORK_UNAVAILABLE',path,name+' no host interfaces observed; traffic is unknown.')
                        else:self.flag('WARNING','PROFILE_NETWORK_MISSING',path,name+' host-interface snapshots absent.')
                except (ValueError,KeyError,TypeError,IndexError) as error:
                    self.flag('ERROR','PROFILE_EVIDENCE_DIFFERS',path,name+' '+kind+': '+str(error));summary['status']='INVALID'
        if seen!=set(self.s.cases):self.flag('WARNING','PROFILE_CASE_COVERAGE',path,'Profiling covers '+str(len(seen))+' of '+str(len(self.s.cases))+' fixed products; no pooling.')
        if report.get('status') not in ('COLLECTED','INCOMPLETE','FAIL'):self.flag('ERROR','PROFILE_STATUS',path,'Unknown profiling collection status.')
        if report.get('status')=='FAIL':self.flag('WARNING','PROFILE_COLLECTION_FAILED',path,str(report.get('errors')))
        if report.get('status')=='COLLECTED' and measured!=len(rows):self.flag('ERROR','PROFILE_COLLECTION_CONTRADICTION',path,'COLLECTED lacks matching raw stage evidence.')
        result['status']='INVALID' if any(x['severity']=='ERROR' for x in self.findings[start:]) else 'MEASURED_WITH_UNKNOWNS' if measured else 'UNAVAILABLE'
        result['measured_cases']=measured
        self.profiling.append(result)
        return result

    def commands(self, root):
        path=root+'/commands.json';text=self.text(path)
        if text is None:return {}
        try:
            entries=load_json(text)
            if not isinstance(entries,list):raise ValueError('commands must be a list')
            result={}
            for entry in entries:
                if not isinstance(entry,dict) or entry.get('label') in result:raise ValueError('invalid/duplicate command label')
                result[entry['label']]=entry
            return result
        except (ValueError,KeyError,TypeError) as error:self.flag('ERROR','INVALID_COMMANDS',path,str(error));return {}

    def command(self, commands, label, path, mode, sanitizer=False):
        entry=commands.get(label)
        if not entry:self.flag('MISSING','MISSING_COMMAND',path,label);return None
        if entry.get('returncode')!=0:self.flag('MISSING' if entry.get('returncode') in (77,78,124,130,143) else 'ERROR','COMMAND_FAILED',path,label+' exit '+str(entry.get('returncode')))
        argv=entry.get('argv')
        if not isinstance(argv,list) or not all(isinstance(x,str) for x in argv):self.flag('ERROR','INVALID_COMMAND',path,label);return None
        def option(key,value):
            return argv.count(key)==1 and argv.index(key)+1<len(argv) and argv[argv.index(key)+1]==value
        if not option('--mode',mode) or not option('--expected-sha256',self.s.identity):self.flag('ERROR','COMMAND_BINDING_DIFFERS',path,label)
        instrumented=any(Path(x).name in ('compute-sanitizer','ncu','nsys') for x in argv)
        if not sanitizer and instrumented:self.flag('ERROR','INSTRUMENTED_BASELINE',path,label+' must be uninstrumented.')
        return argv

    def sanitizer(self, root, tool, commands, canonical):
        start=len(self.findings);path=root+'/'+tool+'/report.json';report=self.obj(path)
        argv=self.command(commands,tool,root+'/commands.json','correctness',True)
        flags={'--tool':tool,'--error-exitcode':'86','--check-bulk-copy':'yes'}
        if tool=='memcheck':flags['--leak-check']='full'
        if tool=='racecheck':flags.update({'--racecheck-memcpy-async':'yes','--racecheck-fence-proxy-async':'yes','--racecheck-detect-level':'info','--racecheck-report':'all'})
        if tool=='initcheck':flags['--initcheck-address-space']='all'
        if argv:
            if not argv or Path(argv[0]).name!='compute-sanitizer':self.flag('ERROR','NOT_SANITIZER_COMMAND',path,tool)
            for key,value in flags.items():
                if argv.count(key)!=1 or argv.index(key)+1>=len(argv) or argv[argv.index(key)+1]!=value:self.flag('MISSING','SANITIZER_COVERAGE_FLAGS',path,key+' '+value+' required.')
            if any(x.startswith('--suppress') for x in argv):self.flag('MISSING','SANITIZER_SUPPRESSIONS',path,'Suppressed diagnostics cannot establish the full gate.')
        help_text=''.join(self.text(root+'/sanitizer-help.'+x) or '' for x in ('stdout','stderr'))
        for key in flags:
            if key[2:] not in help_text:self.flag('MISSING','SANITIZER_CAPABILITY',path,'Help does not advertise '+key)
        version=''.join(self.text(root+'/sanitizer-version.'+x) or '' for x in ('stdout','stderr'))
        if not version.strip():self.flag('MISSING','SANITIZER_VERSION',path,'Run-time sanitizer version is not recorded.')
        for label,option in (('sanitizer-help','--help'),('sanitizer-version','--version')):
            entry=commands.get(label,{})
            if entry.get('returncode')!=0 or option not in entry.get('argv',[]):self.flag('MISSING','SANITIZER_VERSION_COMMAND',path,label+' command absent or unsuccessful.')
        log=''.join(self.text(root+'/'+tool+'.'+x) or '' for x in ('stdout','stderr'))
        errors=re.findall(r'ERROR SUMMARY:\s*(\d+) errors?',log)
        hazards=re.findall(r'RACECHECK SUMMARY:\s*(\d+) hazards? displayed',log)
        if any(int(x) for x in re.findall(r'(\d+) (?:errors?|warnings?|bytes leaked)\b',log)) or any(int(x) for x in hazards):
            self.flag('ERROR','SANITIZER_HAZARD',path,'Nonzero error/warning/hazard/leak count.')
        if not (hazards if tool=='racecheck' else errors):self.flag('MISSING','SANITIZER_SUMMARY',path,'No explicit clean summary.')
        if re.search(r'overflow|unsupported|not supported|not instrument|no attach|timed out',log,re.I):self.flag('MISSING','SANITIZER_COVERAGE_INCOMPLETE',path,'Instrumentation overflow/unsupported/timeout diagnostic.')
        if report and self.report_state(report,path):
            observed=self.cases(report,path)
            if canonical and observed!=canonical:self.flag('ERROR','SANITIZER_RESULTS_DIFFER',path,'Sanitized case/reference records differ from correctness.')
        return dict(status=self.status(start),report=path,tool_version=version.strip()[:4096])

    def analyze_run(self, root):
        parent=self.obj(root+'/report.json');commands=self.commands(root);sections={}
        if parent:
            if parent.get('status') in UNAVAILABLE:self.flag('MISSING',parent['status'],root,str(parent.get('reason','Unavailable.')))
            elif parent.get('status')!='PASS':self.flag('ERROR','RUN_FAILED',root,str(parent.get('reason','Run failed.')))
            else:self.packet_binding(parent,root+'/report.json')
        start=len(self.findings);path=root+'/correctness/report.json';report=self.obj(path);canonical={}
        if report and self.report_state(report,path):canonical=self.cases(report,path)
        self.command(commands,'correctness',root+'/commands.json','correctness')
        sections['correctness']=dict(status=self.status(start),report=path,case_residue_pairs=len(canonical))
        for tool in TOOLS:sections[tool]=self.sanitizer(root,tool,commands,canonical)
        start=len(self.findings);path=root+'/performance/report.json';perf=self.obj(path)
        argv=self.command(commands,'performance',root+'/commands.json','performance')
        if perf and self.report_state(perf,path):
            if self.s.packet.get('shared_results_contract') and (perf.get('wall_timing_cuda_events') is not False or perf.get('device_timing_separate_batch') is not True):
                self.flag('ERROR','TIMING_BATCH_ISOLATION',path,'Shared-format Q7 requires separate wall and device-event batches.')
            self.cases(perf,path,True)
            if 'NOT pure kernel duration' not in perf.get('timing_scope',''):
                self.flag('MISSING','TIMING_SCOPE',path,'Expected admission/host-gap scope is not explicit.')
            if argv and '--iterations' in argv:
                try:
                    requested=int(argv[argv.index('--iterations')+1])
                    if any(row['host_us']['count']!=requested for row in self.performance):self.flag('ERROR','SAMPLE_BUDGET_DIFFERS',path,'Sample counts differ from recorded command.')
                except (ValueError,IndexError):self.flag('ERROR','INVALID_ITERATION_COMMAND',path,'Invalid --iterations.')
        sections['performance']=dict(status=self.status(start),report=path)
        # Parent assertions are checked only after independent child/log checks.
        if parent and parent.get('status')=='PASS':
            if parent.get('gpu_execution') is not True or parent.get('race_qualified') is not True:self.flag('ERROR','PARENT_GATE_CONTRADICTION',root,'PASS lacks actual execution/race qualification flags.')
            for name in ('correctness',)+TOOLS+('performance',):
                child=self.obj(root+'/'+name+'/report.json',False)
                embedded=parent.get('records',{}).get(name)
                if name in ('correctness','performance') and child and embedded!=child:self.flag('ERROR','PARENT_CHILD_CONTRADICTION',root,name)
                if name in TOOLS and (not isinstance(embedded,dict) or embedded.get('status')!='PASS' or embedded.get('clean_summaries') is not True or embedded.get('actual_GPU_child') is not True):self.flag('ERROR','PARENT_TOOL_CONTRADICTION',root,name)
        return sections

    def environment(self):
        env=self.obj('environment/capture.json');result=dict(collection=env,execution=self.devices)
        if not env:return result
        for command in env.get('commands',[]):
            if command.get('status')!='CAPTURED':self.flag('WARNING','ENVIRONMENT_UNAVAILABLE','environment/capture.json',command.get('label','tool')+': '+str(command.get('status')))
        nvcc=self.text('environment/nvcc.stdout',False) or ''
        release=re.search(r'release\s+(\d+)\.(\d+)',nvcc)
        if release:
            result['installed_toolkit_release']='.'.join(release.groups())
            if tuple(map(int,release.groups()))!=(12,8):self.flag('WARNING','TOOLKIT_DIFFERS','environment/nvcc.stdout','Collection-time toolkit differs from producer 12.8.1; retained cubins are used for execution.')
        else:self.flag('WARNING','TOOLKIT_NOT_RECORDED','environment/nvcc.stdout','Installed nvcc version is unavailable; do not infer it from nvidia-smi.')
        smi=self.text('environment/nvidia-smi-query.stdout',False) or ''
        for key,pattern in [('nvidia_driver_version',r'Driver Version\s*:\s*([\d.]+)'),('driver_cuda_capability',r'CUDA Version\s*:\s*([\d.]+)')]:
            match=re.search(pattern,smi)
            if match:result[key]=match.group(1)
        uuids={x['facts'].get('uuid') for x in self.devices if x['facts'].get('uuid')}
        versions={x['facts'].get('driver_api_version') for x in self.devices if x['facts'].get('driver_api_version')}
        if len(uuids)>1:self.flag('ERROR','MIXED_EXECUTION_GPUS','reports','Execution stages record different device UUIDs.')
        if len(versions)>1:self.flag('ERROR','MIXED_EXECUTION_DRIVERS','reports','Execution stages record different Driver API versions.')
        result['status']='INCOMPATIBLE' if any(x['code'] in ('GPU_INCOMPATIBLE','DRIVER_INCOMPATIBLE','PLATFORM_INCOMPATIBLE','MIXED_EXECUTION_GPUS','MIXED_EXECUTION_DRIVERS') for x in self.findings) else 'OBSERVED_COMPATIBLE' if self.devices else 'NOT_VERIFIED'
        return result

    def analyze(self, selected=None):
        bundle=self.obj('bundle.json')
        if not bundle or bundle.get('schema')!='open64.mla-q7.return-bundle.v1':raise ValueError('unknown return bundle schema')
        if bundle.get('packet_sha256')!=self.s.identity:raise ValueError('return bundle source packet differs from trusted source')
        for name in ('SHA256SUMS','packet.json'):
            data=self.a.texts.get('packet/'+name)
            if data is None:self.flag('MISSING','MISSING_SOURCE_PROVENANCE','packet/'+name,'Required source identity copy is absent.')
            elif name=='SHA256SUMS' and hashlib.sha256(data).hexdigest()!=self.s.identity:raise ValueError('returned source manifest differs')
            elif name=='packet.json' and hashlib.sha256(data).hexdigest()!=self.s.files['packet.json']:raise ValueError('returned source metadata differs')
        for note in bundle.get('collection_notes',[]):
            self.flag('MISSING' if any(x in note.get('reason','') for x in ('changed','JSON','different packet','MISSING')) else 'WARNING','COLLECTION_NOTE',note.get('path','bundle.json'),note.get('reason','Collection note'))
        inventory=[];compilation=[];candidates=set();profile_candidates=set()
        for path in sorted(self.a.hashes):
            if not path.startswith('results/'):continue
            if path.endswith('/status.json'):
                report=self.obj(path)
                if report and report.get('schema')=='open64.mla-q7.workflow.v1':
                    code=report.get('exit_code')
                    if code not in (0,None):self.flag('MISSING' if code in (77,78,124,130,143) else 'ERROR','WORKFLOW_NONZERO',path,'Workflow exit '+str(code))
                    if report.get('packet_sha256')!=self.s.identity:self.flag('ERROR','WORKFLOW_PACKET_DIFFERS',path,'Workflow source identity differs.')
            if path.endswith('/build-report.json'):
                report=self.obj(path)
                if report:
                    compilation.append(dict(path=path,status=report.get('status'),exact_native_cubin_matches=report.get('exact_native_cubin_matches')))
                    if report.get('status')!='PASS':self.flag('ERROR','COMPILE_FAILED',path,str(report.get('reason','Build failed.')))
                    matches=report.get('exact_native_cubin_matches')
                    if isinstance(matches,dict) and (set(matches)!=set(self.s.cases) or not all(matches.values())):self.flag('WARNING','REBUILT_IMAGE_DIFFERS',path,'Rebuild coverage/digests differ; execution must use approved retained images.')
            if path.endswith('/report.json'):
                report=self.obj(path)
                if report:inventory.append(dict(path=path,status=report.get('status'),gpu_execution=report.get('gpu_execution'),reason=report.get('reason')))
                root=path.rsplit('/',1)[0]
                if report and report.get('schema')==PROFILE_SCHEMA:profile_candidates.add(root)
                if root.count('/')==1 and (report and report.get('mode')=='all' or root+'/correctness/report.json' in self.a.hashes):candidates.add(root)
        if selected:
            if selected not in candidates:raise ValueError('--run is not a qualification root in this archive')
            candidates={selected}
        if not compilation:self.flag('WARNING','NO_COMPILE_REPORT','results','No offline compilation report returned; retained-code execution is evaluated separately.')
        if len(candidates)>1:self.flag('MISSING','AMBIGUOUS_RUNS','results','Multiple qualification runs; use --run to select one. Passes are never pooled.')
        sections={}
        if len(candidates)==1:sections=self.analyze_run(next(iter(candidates)))
        else:
            self.flag('MISSING','MISSING_QUALIFICATION_RUN','results','No uniquely selected complete Q7 run.')
            sections={name:dict(status='INCOMPLETE') for name in ('correctness',)+TOOLS+('performance',)}
            # Preserve useful partial standalone measurements without pooling
            # them into a full run or assuming instrumentation provenance.
            for name in ('correctness','performance'):
                partial=[]
                for item in inventory:
                    if item['path'].count('/')==2:
                        report=self.obj(item['path'],False)
                        if report and report.get('mode')==name:partial.append((item['path'],report))
                if len(partial)==1:
                    path,report=partial[0];start=len(self.findings);cases={}
                    if self.report_state(report,path):cases=self.cases(report,path,name=='performance')
                    self.flag('MISSING','STANDALONE_PROVENANCE',path,'No full-run command/tool provenance; standalone data cannot close Q7.')
                    sections[name]=dict(status=self.status(start),report=path,case_residue_pairs=len(cases),standalone=True)
        if len(profile_candidates)>1:self.flag('WARNING','MULTIPLE_PROFILES','results','Distinct profile runs are displayed separately; their counters are never pooled.')
        for root in sorted(profile_candidates):self.profile_run(root)
        sections['profiling']=dict(status='INVALID' if any(x['status']=='INVALID' for x in self.profiling) else
            'MEASURED_WITH_UNKNOWNS' if any(x.get('measured_cases',0) for x in self.profiling) else 'UNAVAILABLE' if self.profiling or self.s.packet.get('profiling_contract') else 'NOT_REQUESTED_OR_LEGACY',
            independent_from_timing=True,runs=[x['report'] for x in self.profiling])
        if self.s.packet.get('profiling_contract') and not profile_candidates:self.flag('WARNING','PROFILE_MISSING','results','Diagnostic profiling absent/skipped; numerical and timing qualification are evaluated separately.')
        for path in self.a.oversize:self.flag('MISSING','TEXT_BUDGET',path,'Evidence exceeds --max-text-bytes and was not interpreted.')
        compatibility=self.environment()
        overall=self.status(0)
        return dict(schema='open64.mla-q7.return-validation.v1',overall=overall,integrity='VERIFIED',archive_sha256=self.a.digest,
                    packet_sha256=self.s.identity,archive_files=len(self.a.hashes),expanded_bytes=self.a.total_bytes,
                    source_validation='trusted manifest and consumed metadata/oracle hashes; no code execution',source_files_consumed=sorted(self.s.checked),
                    selected_run=next(iter(candidates)) if len(candidates)==1 else None,sections=sections,compilation=compilation,
                    compatibility=compatibility,correctness_errors=self.correctness_errors,performance=self.performance,profiling=self.profiling,report_inventory=inventory,findings=self.findings,
                    scope='Reported evidence consistency only; raw GPU output tensors are not returned. No independent GPU rerun, race proof, speedup or transfer approval.',
                    gpu_workloads_launched=0,search_invocations=0,aot_selection='deferred')


def markdown(report):
    def clean(value):
        text=str(value).replace('&','&amp;').replace('<','&lt;').replace('>','&gt;').replace('|','\\|').replace('\n',' ')
        for char in ('`','[',']','*','_'):text=text.replace(char,'\\'+char)
        return text
    lines=['# Q7 returned-results review','', '**Overall: '+report['overall']+'**', '',report.get('scope','Integrity validation failed.'),'',
           'Archive SHA-256: `'+report.get('archive_sha256','unknown')+'`','', '## Evidence','', '| Section | Status |','| --- | --- |']
    for name,section in report.get('sections',{}).items():lines.append('| '+name+' | '+section['status']+' |')
    lines+=['','## Reported correctness error peaks','',
            '| Case | Prefix | Source max abs | PyTorch max abs | Scalar max abs |','| --- | --- | --- | --- | --- |']
    path=report.get('sections',{}).get('correctness',{}).get('report')
    for row in report.get('correctness_errors',[]):
        if row['report']==path:lines.append('| '+clean(row['case'])+' | '+str(row['prefix_bytes'])+' | '+' | '.join(clean(row['references'][lane]['max_absolute_error']) for lane in LANES)+' |')
    compatibility=report.get('compatibility',{});devices=compatibility.get('execution',[])
    lines+=['','## GPU/CUDA compatibility','', 'Status: **'+compatibility.get('status','NOT_VERIFIED')+'**.','',
            '| Observation | Value |','| --- | --- |',
            '| Required contract | sm_90 / capability 9.0, Driver API >=12.8; producer toolkit 12.8.1 / PTX ISA 8.6 |',
            '| Installed nvcc release (collection time) | '+clean(compatibility.get('installed_toolkit_release','not recorded'))+' |',
            '| nvidia-smi Driver version (collection time) | '+clean(compatibility.get('nvidia_driver_version','not recorded'))+' |',
            '| Driver-advertised CUDA capability (not installed toolkit) | '+clean(compatibility.get('driver_cuda_capability','not recorded'))+' |']
    seen=set()
    for item in devices:
        device=item['facts'];key=(device.get('uuid'),device.get('driver_api_version'))
        if key in seen:continue
        seen.add(key)
        lines.append('| Execution GPU / UUID / capability / Driver API | '+clean(device.get('name'))+' / '+clean(device.get('uuid'))+' / '+clean(device.get('compute_capability'))+' / '+clean(device.get('driver_api_version'))+' |')
    lines+=['','## Performance (recomputed from raw samples)','',
            'Host interval includes Begin/Submit/Q3 completion; stream span includes admission copies and host gaps. Neither is pure kernel time or serving throughput.','',
            '| Case | Prefix | Samples | Host median / p95 (us) | Stream median / p95 (ms) |','| --- | --- | --- | --- | --- |']
    for row in report.get('performance',[]):
        h,s=row['host_us'],row['submission_stream_ms']
        lines.append('| %s | %s | %s | %.6g / %.6g | %.6g / %.6g |'%(clean(row['case']),row['prefix_bytes'],h['count'],h['median'],h['p95'],s['median'],s['p95']))
    lines+=['','## Diagnostic profiling (independent of timing and compiler search)','',
            'Measured counters, derived values, proxies, estimates and unknowns remain distinct. '
            'Warp-stall share and unused scheduler issue capacity are not exposed compute-wait percentages. '
            'Unknown means unavailable or unattributable, never zero. No profiler duration enters benchmark statistics.','']
    def show(item, scale=1):
        if item.get('value') is None:return 'unknown'
        return '%.5g (%s)'%(item['value']/scale,item['evidence'])
    for profile in report.get('profiling',[]):
        lines+=['Report: '+clean(profile['report'])+'; status: **'+profile['status']+'**.','',
                '| Case / kernel | Issue inactive % | Memory stall % | Tensor activity % | HBM GB/s | HBM utilization % | Exposed local / peer / remote wait % |',
                '| --- | --- | --- | --- | --- | --- | --- |']
        for case in profile['cases']:
            if not case['kernels']:lines.append('| '+clean(case['id'])+' | unknown | unknown | unknown | unknown | unknown | unknown / unknown / unknown |')
            for kernel in case['kernels']:
                c,p,o=(kernel['groups'][x] for x in ('compute_wait','data_paths','data_origin'))
                waits=' / '.join(show(o[x]) for x in ('local_gpu_exposed_wait_pct','peer_gpu_exposed_wait_pct','remote_node_exposed_wait_pct'))
                lines.append('| '+clean(case['id']+' / '+kernel['kernel'])+' | '+' | '.join((show(c['issue_inactive_pct']),show(c['memory_dependency_stall_pct']),show(c['tensor_active_pct']),show(p['hbm_bytes_per_second'],1e9),show(p['hbm_utilization_pct']),waits))+' |')
        lines+=['','| Case / kernel | NVLink TX / RX bytes | PCIe read / write bytes |','| --- | --- | --- |']
        for case in profile['cases']:
            for kernel in case['kernels']:
                p=kernel['groups']['data_paths']
                lines.append('| '+clean(case['id']+' / '+kernel['kernel'])+' | '+show(p['nvlink_tx_bytes'])+' / '+show(p['nvlink_rx_bytes'])+' | '+show(p['pcie_read_bytes'])+' / '+show(p['pcie_write_bytes'])+' |')
        lines+=['','Link counters are device-scoped collection observations, not proof that this kernel waited on a peer. '
                'Host-interface traffic includes other processes and is not attributed to remote GPU communication. '
                'Topology/native timeline files support further investigation; no automatic causal partition is asserted.','']
        for case in profile['cases']:
            for item in case.get('ambient_network',[]):
                lines.append('- '+clean(case['id'])+' / '+clean(item['interface'])+' '+item['direction']+': '+show(item['measurement'])+' byte/s (host interface scope).')
    if not report.get('profiling'):lines+=['No diagnostic profile returned. See the profiling section status and findings.','']
    lines+=['','## Findings','']
    for issue in report.get('findings',[]):lines.append('- **'+issue['severity']+' / '+issue['code']+'** — '+clean(issue['path'])+': '+clean(issue['message']))
    if not report.get('findings'):lines.append('No inconsistencies found in the supplied reported evidence.')
    lines+=['','See summary.json for complete facts, per-case errors and evidence paths.','']
    return '\n'.join(lines)


def main(argv=None):
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--archive',type=Path,required=True);p.add_argument('--checksum',type=Path);p.add_argument('--expected-archive-sha256')
    p.add_argument('--packet',type=Path,required=True);p.add_argument('--expected-packet-sha256',required=True)
    p.add_argument('--output',type=Path,required=True);p.add_argument('--run')
    p.add_argument('--max-expanded-bytes',type=int,default=16*1024**3);p.add_argument('--max-members',type=int,default=20000)
    p.add_argument('--max-text-bytes',type=int,default=32*1024**2)
    p.add_argument('--max-retained-text-bytes',type=int,default=256*1024**2)
    a=p.parse_args(argv);out=a.output.resolve()
    if out==a.packet.resolve() or a.packet.resolve() in out.parents:
        print('Summary output must be outside the original source packet.',file=sys.stderr);return 3
    try:out.mkdir(parents=True,exist_ok=False)
    except OSError as error:print('Cannot create fresh summary directory: '+str(error),file=sys.stderr);return 3
    try:
        if min(a.max_expanded_bytes,a.max_members,a.max_text_bytes,a.max_retained_text_bytes)<=0:raise ValueError('invalid archive budget')
        expected=a.expected_archive_sha256
        sidecar=a.checksum or Path(str(a.archive)+'.sha256')
        if a.checksum or expected is None:
            if sidecar.stat().st_size>4096:raise ValueError('checksum sidecar exceeds 4096 bytes')
            rows=sidecar.read_text().splitlines()
            if len(rows)!=1 or not rows[0].split():raise ValueError('checksum sidecar must contain exactly one digest')
            digest=rows[0].split()[0]
            if expected and expected.lower()!=digest.lower():raise ValueError('supplied archive digests disagree')
            expected=digest
        if not re.fullmatch('[0-9a-fA-F]{64}',expected or ''):raise ValueError('invalid expected archive digest')
        archive=Archive(a.archive,expected.lower(),a.max_expanded_bytes,a.max_members,a.max_text_bytes,a.max_retained_text_bytes)
        source=Source(a.packet,a.expected_packet_sha256.lower())
        report=Review(archive,source).analyze(a.run)
    except (OSError,ValueError,TypeError,KeyError,IndexError,AttributeError,RecursionError,OverflowError,struct.error,tarfile.TarError) as error:
        report=dict(schema='open64.mla-q7.return-validation.v1',overall='INVALID',integrity='NOT_ACCEPTED',
                    findings=[dict(severity='ERROR',code='INVALID_INPUT',path=str(a.archive),message=str(error))],
                    gpu_workloads_launched=0,search_invocations=0,aot_selection='deferred')
    (out/'summary.json').write_text(json.dumps(report,indent=2,allow_nan=False)+'\n')
    (out/'summary.md').write_text(markdown(report))
    print(report['overall']+' — '+str(out/'summary.md'))
    return {'PASS_REPORTED':0,'FAIL':1,'INCOMPLETE':2,'INVALID':3}[report['overall']]


if __name__=='__main__':raise SystemExit(main())
