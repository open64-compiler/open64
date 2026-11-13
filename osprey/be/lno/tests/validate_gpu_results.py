"""One offline validator for scaffold results and authenticated MLA Q7 returns.

The common schema/math layer is shared; Q7's existing numerical, sanitizer,
publication and counter audits are mandatory for Q7 acceptance. No returned
code, compiler, profiler or GPU workload is executed. Contract:
doc/GPU-TEST-SHARED-RESULTS-v1.0.md.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import struct
import sys
import gpu_test_results as shared


def safe(name):
    return isinstance(name,str) and bool(name) and not name.startswith('/') and not re.search(r'[\\\x00-\x1f\x7f]',name) and all(x not in ('','.','..') for x in name.split('/'))


def read(root,name,limit=32*1024**2):
    if not safe(name):raise ValueError('unsafe evidence path')
    path=root/name
    if path.is_symlink() or root.resolve() not in path.resolve().parents or path.stat().st_size>limit:
        raise ValueError('unsafe/oversized evidence file: '+name)
    return path.read_bytes()


class Packet:
    """Authenticate source metadata/data without executing package modules."""
    def __init__(self,root,expected):
        self.root=root.resolve();raw=read(self.root,'SHA256SUMS')
        if hashlib.sha256(raw).hexdigest()!=expected:raise ValueError('trusted packet manifest identity differs')
        self.files={}
        for line in raw.decode().splitlines():
            digest,name=line.split('  ',1)
            if not safe(name) or name in self.files or not re.fullmatch('[0-9a-f]{64}',digest):raise ValueError('invalid source manifest')
            self.files[name]=digest

    def read(self,name):
        if name not in self.files:raise ValueError('unlisted source data: '+name)
        raw=read(self.root,name)
        if hashlib.sha256(raw).hexdigest()!=self.files[name]:raise ValueError('source data hash differs: '+name)
        return raw


def audit_scaffold(result,packet,artifact_root):
    """Apply server-owned corpus/FLOP rules and the original acceptance policy."""
    findings=[]
    def flag(level,code,message):findings.append(dict(severity=level,code=code,message=message))
    native=result['extensions']['scaffold_v1']
    if native.get('schema_version')!=1:raise ValueError('unknown preserved scaffold payload')
    if shared.canonical(shared.from_scaffold(native))!=shared.canonical(result):
        flag('ERROR','ADAPTER_DIFFERS','Common result differs from its preserved native payload.')
    if native.get('diagnostics',{}).get('status') in ('failed','unsupported','partial'):
        flag('WARNING','DIAGNOSTIC_INCOMPLETE','Optional profiling did not complete; benchmark evidence remains independent.')
    kind={'gemm.dense.bf16-fp32.bf16.v1':'gemm','mla.decode.paged.bf16-fp32.v1':'mla'}[result['workload_id']]
    if packet is None:
        flag('MISSING','SOURCE_BINDING','Trusted --packet and --expected-packet-sha256 are needed for full corpus validation.')
    else:
        def content_id(items):
            digest=hashlib.sha256()
            for name,content in sorted(items):digest.update(name.encode()+b'\0'+content+b'\0')
            return digest.hexdigest()
        source_prefix='submissions/'+kind+'/'
        submission=[(name[len(source_prefix):],packet.read(name)) for name in packet.files if name.startswith(source_prefix)]
        manifest=shared.loads(packet.read(source_prefix+'manifest.json'))
        if content_id(submission)!=result['identity']['candidate_id']:flag('ERROR','SUBMISSION_BINDING','Source submission identity differs.')
        baseline=manifest['baseline']['source']
        sources=[(baseline,packet.read(source_prefix+baseline)),('server/workload.h',packet.read('server/include/'+kind+'/workload.h'))]
        sources += [(name,content) for name,content in submission if name.endswith(('.h','.cuh'))]
        if content_id(sources)!=result['identity']['baseline_id']:flag('ERROR','BASELINE_BINDING','Baseline/header identity differs.')
        if manifest['workload_id']!=result['workload_id'] or manifest['suite_id']!=result['suite_id']:flag('ERROR','WORKLOAD_BINDING','Submission workload boundary differs.')
        if result['status']=='completed':
            environment=result.get('environment') or {}
            for field in ('sm','driver','cuda_runtime'):
                if type(environment.get(field)) is not int:flag('MISSING','GPU_COMPATIBILITY','Missing actual '+field)
            if type(environment.get('sm')) is int and environment['sm']<manifest['target_sm']:
                flag('ERROR','GPU_COMPATIBILITY','Reported GPU capability is below the compilation target.')
            if any(type(environment.get(field)) is int and environment[field]<11000 for field in ('driver','cuda_runtime')):
                flag('ERROR','GPU_COMPATIBILITY','BF16 scaffold requires CUDA >=11.')
        prefix='data/'+kind+'/'
        raw=packet.read(prefix+'index.json');index=shared.loads(raw)
        if hashlib.sha256(raw).hexdigest()!=result['identity']['corpus_sha256'] or index['workload_id']!=result['workload_id'] or index['suite_id']!=result['suite_id']:
            flag('ERROR','CORPUS_BINDING','Workload/suite/corpus identities differ.')
        expected={}
        for entry in index['cases']:
            name=entry['case_id']
            if not safe(name) or '/' in name or name in expected:raise ValueError('invalid trusted corpus case')
            for filename,digest in entry['files'].items():
                content=packet.read(prefix+name+'/'+filename)
                if hashlib.sha256(content).hexdigest()!=digest:raise ValueError('corpus index/file hash differs')
            meta=shared.loads(packet.read(prefix+name+'/case.json'));p=meta['parameters']
            if kind=='gemm':flops=2*p['m']*p['n']*p['k']
            else:
                lengths=packet.read(prefix+name+'/lengths.bin')
                flops=2*p['heads']*sum(x[0] for x in struct.iter_unpack('<i',lengths))*(2*p['latent_dim']+p['rope_dim'])
            if meta['useful_flops']!=flops:raise ValueError('trusted useful-FLOP formula differs')
            expected[name]=(flops,meta)
        if set(expected)!={x['case_id'] for x in result['cases']}:flag('ERROR','CASE_COVERAGE','Cases differ from the trusted server suite.')
        for case in result['cases']:
            if case['case_id'] not in expected:continue
            flops,meta=expected[case['case_id']]
            if case['useful_flops']['value']!=flops:flag('ERROR','FLOP_FORMULA',case['case_id'])
            if native['policy']['atol']!=meta['atol'] or native['policy']['rtol']!=meta['rtol']:
                flag('ERROR','NUMERICAL_CONTRACT',case['case_id']+' tolerances differ from trusted suite.')
    if result['status']=='completed':
        if native['policy'].get('noise_multiplier')!=3 or not 0<=native['policy']['min_improvement']<1:flag('ERROR','ACCEPTANCE_POLICY','Unsupported acceptance/noise policy.')
        if not result['identity']['same_device_and_session'] or not result['identity'].get('session_id') or not (result.get('environment') or {}).get('gpu_uuid'):
            flag('ERROR','SESSION_BINDING','Completed paired timing needs GPU UUID and an execution session.')
        accepted=True
        for case,original in zip(result['cases'],native['cases']):
            if case['correctness'].get('baseline_pass') is not True or case['correctness'].get('candidate_pass') is not True:
                accepted=False
                continue
            for role in ('baseline','candidate'):
                for label,legacy_key in (('wall','wall_us'),('device','device_us')):
                    record=case['timing'][role][label]
                    if not record['summary']:
                        flag('MISSING','RAW_SAMPLES',case['case_id']+' '+role+' '+label);accepted=False;continue
                    actual=original[role]
                    if actual.get('samples')!=record['summary']['count'] or actual[legacy_key]!=record['summary']['median']:
                        flag('ERROR','NATIVE_TIMING_DIFFERS',case['case_id'])
                    if label=='wall' and actual.get('wall_mad_us')!=record['summary']['mad']:flag('ERROR','NATIVE_MAD_DIFFERS',case['case_id'])
                    if record['summary']['count']!=native['policy']['samples']:flag('ERROR','SAMPLE_BUDGET',case['case_id'])
                device=case['timing'][role]['device']['summary']
                if device and device['median']>0 and case['useful_flops']['value'] is not None:
                    expected=case['useful_flops']['value']/(device['median']*1e6)
                    value=case['effective_tflops'][role]
                    if value['evidence']!='derived' or value['value']!=expected:flag('ERROR','TFLOPS_DIFFERS',case['case_id'])
            b=case['timing']['baseline']['wall']['summary'];c=case['timing']['candidate']['wall']['summary']
            if b and c and b['median']>0:
                threshold=max(native['policy']['min_improvement'],3*(b['mad']+c['mad'])/b['median'])
                accepted &= c['median']/b['median'] < 1-threshold
                if original.get('wall_time_ratio')!=c['median']/b['median']:flag('ERROR','NATIVE_RATIO_DIFFERS',case['case_id'])
        if result['decision']=='accepted' and not accepted:flag('ERROR','ACCEPTANCE_DIFFERS','Claimed acceptance fails correctness/timing/noise policy.')
    for case in result['cases']:
        diagnostic=case.get('diagnostics',{})
        if diagnostic.get('status') in ('failed','unsupported','partial'):
            flag('WARNING','DIAGNOSTIC_INCOMPLETE',case['case_id']+'; unprofiled timing/acceptance remain independent.')
        if 'artifact' not in diagnostic:continue
        try:
            index=shared.loads(read(artifact_root,diagnostic['artifact']))
            identity=index['benchmark_identity']
            if index['case_id']!=case['case_id'] or any(identity.get(key)!=native.get(key) for key in ('submission_id','workload_id','suite_id','corpus_sha256','baseline_id')):
                raise ValueError('PM identity/case differs from benchmark')
            if identity.get('environment')!=native.get('environment'):raise ValueError('PM GPU/environment differs')
            parent=Path(diagnostic['artifact']).parent.as_posix()
            for role,record in index['implementations'].items():
                if role not in ('baseline','candidate'):raise ValueError('unknown PM role')
                if record['status']!='collected':
                    flag('WARNING','PM_UNAVAILABLE',case['case_id']+' '+role);continue
                shared.validate_pm(shared.loads(read(artifact_root,parent+'/'+record['samples'])))
                read(artifact_root,parent+'/'+record['native_report'])
        except (OSError,ValueError,KeyError,TypeError) as error:
            flag('ERROR' if diagnostic.get('status')=='collected' else 'WARNING','PM_EVIDENCE',str(error))
    return findings


def archive_result(args,out):
    """Retain the mandatory deep Q7 review before validating its shared form."""
    import validate_mla_q7_results as q7
    argv=['--archive',str(args.archive),'--packet',str(args.packet),'--expected-packet-sha256',args.expected_packet_sha256,
          '--output',str(out/'q7-review')]
    for key in ('checksum','expected_archive_sha256','run'):
        value=getattr(args,key)
        if value:argv+=['--'+key.replace('_','-'),str(value)]
    code=q7.main(argv)
    audit=shared.loads((out/'q7-review/summary.json').read_bytes())
    audit['schema']='open64.gpu-test.validation.v1'
    if code==3:return None,audit
    archive=q7.Archive(args.archive,audit['archive_sha256'],16*1024**3,20000,32*1024**2)
    source=q7.Source(args.packet,args.expected_packet_sha256)
    try:
        result=shared.from_q7(source.packet,source.identity,archive.texts)
    except (ValueError,KeyError,TypeError,AttributeError,RecursionError,OverflowError) as error:
        audit['findings'].append(dict(severity='ERROR',code='SHARED_ADAPTATION_FAILED',message=str(error)))
        audit['overall']='FAIL';audit['native_q7_review']='q7-review/summary.json'
        return None,audit
    review=shared.validate(result)
    embedded=archive.texts.get('result.json')
    if embedded is not None and shared.canonical(shared.loads(embedded))!=shared.canonical(result):
        review['findings'].append(dict(severity='ERROR',code='Q7_SHARED_DIFFERS',message='Returned shared result differs from native evidence.'))
    elif embedded is None and source.packet.get('shared_results_contract')==shared.SCHEMA:
        review['findings'].append(dict(severity='MISSING',code='Q7_SHARED_MISSING',message='New-format packet did not return its shared result.'))
    review['findings']+=audit['findings']
    review['native_q7_review']='q7-review/summary.json'
    review['profiling_status']=audit.get('sections',{}).get('profiling',{}).get('status')
    review['scope']=audit['scope']+' Full Q7 numerical, race, Q3 and counter checks remain mandatory.'
    return result,review


def main(argv=None):
    """Audit shared reports through trusted producer adapters without running returns."""
    parser=argparse.ArgumentParser(description=__doc__)
    choice=parser.add_mutually_exclusive_group(required=True)
    choice.add_argument('--result',type=Path);choice.add_argument('--archive',type=Path)
    parser.add_argument('--packet',type=Path);parser.add_argument('--expected-packet-sha256')
    parser.add_argument('--checksum',type=Path);parser.add_argument('--expected-archive-sha256');parser.add_argument('--run')
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args(argv)
    if args.archive and (not args.packet or not args.expected_packet_sha256):parser.error('Q7 archives require a trusted source packet and identity')
    out=args.output.resolve()
    if args.packet and (out==args.packet.resolve() or args.packet.resolve() in out.parents):parser.error('review output must be outside the trusted packet')
    out.mkdir(parents=True,exist_ok=False)
    result=None
    try:
        if args.archive:result,review=archive_result(args,out)
        else:
            result=shared.loads(read(args.result.resolve().parent,args.result.name))
            review=shared.validate(result)
            if result['producer']=='mla-q7':
                review['findings'].append(dict(severity='MISSING',code='Q7_ARCHIVE_REQUIRED',message='Q7 JSON alone cannot close reference/race/Q3 gates; validate the return tar.'))
            elif result['producer']=='testing-scaffold':
                packet=Packet(args.packet,args.expected_packet_sha256) if args.packet and args.expected_packet_sha256 else None
                review['findings']+=audit_scaffold(result,packet,args.result.resolve().parent)
            elif result['producer']=='h20-h7':
                from validate_h20_h7 import audit
                packet=Packet(args.packet,args.expected_packet_sha256) if args.packet and args.expected_packet_sha256 else None
                review['findings']+=audit(result,packet,args.result.resolve().parent)
            else:raise ValueError('unknown producer adapter')
        if result is not None:
            levels={x['severity'] for x in review['findings']}
            review['overall']='FAIL' if 'ERROR' in levels else 'INCOMPLETE' if 'MISSING' in levels else 'PASS_REPORTED'
            review['environment']=result.get('environment');review['identity']=result['identity']
            (out/'result.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
            text=shared.markdown(result,review)
            if args.archive:text+='\n[Full Q7 correctness, sanitizer, Q3 and profiling audit](q7-review/summary.md)\n'
        else:text='# Shared GPU results review\n\n'+review['overall']+' — see summary.json and [the native Q7 review](q7-review/summary.md).\n'
    except (OSError,ValueError,KeyError,TypeError,IndexError,AttributeError,RecursionError,OverflowError) as error:
        review=dict(schema='open64.gpu-test.validation.v1',overall='INVALID',findings=[dict(severity='ERROR',code='INVALID_INPUT',message=str(error))],
                    gpu_workloads_launched=0,search_invocations=0)
        text='# Shared GPU results review\n\nINVALID — '+str(error).replace('<','&lt;')+'\n'
    (out/'summary.json').write_text(json.dumps(review,indent=2,allow_nan=False)+'\n');(out/'summary.md').write_text(text)
    print(review['overall']+' — '+str(out/'summary.md'))
    return {'PASS_REPORTED':0,'FAIL':1,'INCOMPLETE':2,'INVALID':3}[review['overall']]


if __name__=='__main__':raise SystemExit(main())
