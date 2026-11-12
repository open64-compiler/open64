"""Offline review of a combined return, delegating all workload acceptance.

Authenticate the complete tar before unpacking bounded data into a private
temporary directory. Never execute returned files or add them to import paths.
Each workload uses the existing shared validator and Q7 deep audit.
Contract: doc/GPU-TEST-COMBINED-PACKAGE-v1.0.md.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import tarfile
import tempfile
import sys
from gpu_test_suite_common import catalog,write,ROOT,RETURN_SCHEMA,ROLES
from gpu_test_results import loads
from validate_mla_q7_results import Archive,safe_name,digest_file
import validate_gpu_results as shared_validator


def review(args,out):
    table=catalog(args.packet,args.expected_packet_sha256,full=False)
    expected=args.expected_archive_sha256
    if not expected:
        sidecar=args.checksum or Path(str(args.archive)+'.sha256')
        if sidecar.stat().st_size>4096:raise ValueError('oversized checksum sidecar')
        lines=sidecar.read_text().splitlines()
        if len(lines)!=1:raise ValueError('invalid checksum sidecar')
        expected=lines[0].split()[0]
    if not re.fullmatch('[0-9a-f]{64}',expected):raise ValueError('invalid archive SHA-256')
    archive=Archive(args.archive,expected,args.max_expanded_bytes,args.max_members,args.max_text_bytes,root_prefix=ROOT)
    metadata=loads(archive.texts['bundle.json'])
    if metadata.get('schema')!=RETURN_SCHEMA or metadata.get('packet_sha256')!=args.expected_packet_sha256 or metadata.get('packets')!=table['packets']:
        raise ValueError('combined return/source identities differ')
    index=loads(archive.texts['suite-results.json'])
    if index.get('schema')!='open64.gpu-test.suite-results.v1' or set(index.get('profiles',{}))!=set(ROLES) or index.get('timings_pooled') is not False or index.get('profiling_separate') is not True:
        raise ValueError('invalid combined profile index or isolation contract')
    findings=[];results={};uuids=set()
    def flag(level,code,message):findings.append(dict(severity=level,code=code,message=message))
    if metadata.get('search_invocations')!=0 or metadata.get('GPU_workloads_launched_by_collector')!=0:
        flag('ERROR','COLLECTION_ISOLATION','Collection must not launch GPU workloads or compiler search.')
    for note in metadata.get('collection_notes',[]):flag('MISSING','COLLECTION_NOTE',str(note))
    if archive.oversize:flag('MISSING','TEXT_BUDGET',str(archive.oversize))
    with tempfile.TemporaryDirectory(prefix='gpu-suite-evidence-') as temporary:
        data=Path(temporary)
        # All names/types/budgets/manifests were checked before any disk writes.
        # Recheck each copied file against that scan to refuse a changing input.
        with tarfile.open(args.archive,'r|*') as tar:
            for member in tar:
                if not member.isfile():continue
                relative=member.name[len(ROOT):]
                if not member.name.startswith(ROOT) or not safe_name(relative) or relative not in archive.hashes:raise ValueError('archive changed after validation')
                path=data/relative;path.parent.mkdir(parents=True,exist_ok=True);digest=hashlib.sha256()
                with path.open('xb') as target:
                    stream=tar.extractfile(member)
                    for block in iter(lambda:stream.read(1048576),b''):target.write(block);digest.update(block)
                if digest.hexdigest()!=archive.hashes[relative]:raise ValueError('archive payload changed after validation')
        if digest_file(args.archive)!=expected:raise ValueError('archive changed during extraction')
        for role in ROLES:
            path='results/mla-q7/Open64_MLA_Q7_results.tar.gz' if role=='mla-q7' else 'results/'+role+'/result.json'
            entry=index['profiles'][role]
            if entry.get('path')!=path or entry.get('kind')!=('q7_return' if role=='mla-q7' else 'shared_json'):
                raise ValueError('unexpected profile evidence path')
            if entry.get('present') is not (path in archive.hashes):raise ValueError('profile presence assertion differs')
            if path not in archive.hashes:
                flag('MISSING','MISSING_PROFILE',role+' evidence absent; raw partial files may be retained.')
                results[role]=dict(overall='INCOMPLETE');continue
            packet=args.packet/('mla-q7' if role=='mla-q7' else 'scaffold')
            target=out/role
            argv=['--archive' if role=='mla-q7' else '--result',str(data/path),'--packet',str(packet),
                '--expected-packet-sha256',table['packets']['mla-q7' if role=='mla-q7' else 'scaffold']['packet_sha256'],'--output',str(target)]
            shared_validator.main(argv)
            child=loads((target/'summary.json').read_bytes());results[role]=dict(overall=child['overall'],review=role+'/summary.json')
            resultpath=target/'result.json'
            if resultpath.is_file():
                expected_workload={'gemm':'gemm.dense.bf16-fp32.bf16.v1','mla-core':'mla.decode.paged.bf16-fp32.v1','mla-q7':'mla.quantized.functional-cache.q3.v1'}[role]
                if loads(resultpath.read_bytes()).get('workload_id')!=expected_workload:flag('ERROR','WORKLOAD_BINDING',role+' contains another workload.')
            if child['overall'] in ('FAIL','INVALID'):flag('ERROR','WORKLOAD_FAILED',role+': '+child['overall'])
            elif child['overall']!='PASS_REPORTED':flag('MISSING','WORKLOAD_INCOMPLETE',role+': '+child['overall'])
            environment=child.get('environment') or {}
            uuid=environment.get('gpu_uuid') or environment.get('uuid')
            if uuid:uuids.add(uuid.replace('GPU-','').replace('-','').lower())
        workflow=loads(archive.texts['workflow/status.json']) if 'workflow/status.json' in archive.texts else None
        if workflow is None:flag('MISSING','MISSING_WORKFLOW','Original commands/exit statuses are absent.')
        else:
            if workflow.get('packet_sha256')!=args.expected_packet_sha256:flag('ERROR','WORKFLOW_BINDING','Workflow belongs to another source packet.')
            for step in workflow.get('steps',[]):
                code=step.get('exit_code')
                if code not in (0,None) and step.get('label')!='q7-profiling':
                    flag('MISSING' if code in (77,78,124,130,143) else 'ERROR','STEP_NONZERO',step.get('label','unknown')+' exit '+str(code))
            labels=[step.get('label') for step in workflow.get('steps',[])]
            if len(labels)!=len(set(labels)):flag('ERROR','DUPLICATE_STEPS','Workflow step labels are ambiguous.')
            required={'scaffold-unit','gemm','mla-core','q7-unit'}
            if not workflow.get('check_only') and workflow.get('workloads',{}).get('mla-q7',{}).get('exit_code')==0:
                required|={'q7-compile-generated','q7-qualification'}
            if required-set(labels):flag('MISSING','STEP_COVERAGE',str(sorted(required-set(labels))))
            for label in ('exit_code','inner_collection_exit_code'):
                code=workflow.get(label)
                if code not in (None,0):flag('MISSING' if code in (77,78,124,130,143) else 'ERROR','WORKFLOW_NONZERO',label+' '+str(code))
            if set(workflow.get('workloads',{}))!=set(ROLES):flag('MISSING','WORKLOAD_LEDGER','Not all workload exit statuses were recorded.')
            if workflow.get('check_only'):flag('MISSING','CHECK_ONLY','CPU/corpus checks do not constitute GPU qualification.')
    if len(uuids)>1:flag('ERROR','MIXED_GPUS','Workloads record different execution GPUs; results are not pooled.')
    levels={x['severity'] for x in findings}
    return dict(schema='open64.gpu-test.suite-validation.v1',overall='FAIL' if 'ERROR' in levels else 'INCOMPLETE' if 'MISSING' in levels else 'PASS_REPORTED',
        packet_sha256=args.expected_packet_sha256,archive_sha256=expected,profiles=results,findings=findings,
        execution_gpu_uuids=sorted(uuids),timings_pooled=False,gpu_workloads_launched=0,search_invocations=0,
        scope='Reported evidence consistency; each workload retains its original numerical, timing and profiling contracts.')


def main(argv=None):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--archive',type=Path,required=True);parser.add_argument('--checksum',type=Path);parser.add_argument('--expected-archive-sha256')
    parser.add_argument('--packet',type=Path,required=True);parser.add_argument('--expected-packet-sha256',required=True)
    parser.add_argument('--output',type=Path,required=True)
    parser.add_argument('--max-expanded-bytes',type=int,default=16*1024**3);parser.add_argument('--max-members',type=int,default=20000)
    parser.add_argument('--max-text-bytes',type=int,default=32*1024**2)
    args=parser.parse_args(argv);args.packet=args.packet.resolve();out=args.output.resolve()
    if out==args.packet or args.packet in out.parents:parser.error('review output must be outside the source packet')
    out.mkdir(parents=True,exist_ok=False)
    try:
        if min(args.max_expanded_bytes,args.max_members,args.max_text_bytes)<=0:raise ValueError('invalid archive budget')
        report=review(args,out)
    except (OSError,ValueError,KeyError,TypeError,IndexError,AttributeError,RecursionError,OverflowError,tarfile.TarError) as error:
        report=dict(schema='open64.gpu-test.suite-validation.v1',overall='INVALID',findings=[dict(severity='ERROR',code='INVALID_INPUT',message=str(error))],gpu_workloads_launched=0,search_invocations=0)
    write(out/'summary.json',report)
    lines=['# Combined GPU test review','','**'+report['overall']+'**','',report.get('scope','Input integrity not accepted.'),'',
        '| Workload | Status | Review |','| --- | --- | --- |']
    for role,item in report.get('profiles',{}).items():lines.append('| '+role+' | '+item['overall']+' | '+('[details]('+role+'/summary.md)' if item.get('review') else 'missing')+' |')
    lines+=['','Timing, useful-work numerators, ratios and profiling remain separate for each workload.','']
    for issue in report['findings']:
        text=str(issue['message']).replace('&','&amp;').replace('<','&lt;').replace('>','&gt;').replace('\n',' ')
        for char in ('`','[',']','*','_'):text=text.replace(char,'\\'+char)
        lines.append('- '+issue['severity']+' / '+issue['code']+': '+text)
    (out/'summary.md').write_text('\n'.join(lines)+'\n');print(report['overall']+' — '+str(out/'summary.md'))
    return {'PASS_REPORTED':0,'FAIL':1,'INCOMPLETE':2,'INVALID':3}[report['overall']]


if __name__=='__main__':raise SystemExit(main())
