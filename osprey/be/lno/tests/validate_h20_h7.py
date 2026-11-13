"""Read-only H7 audit used by the shared offline GPU results validator.

Trusted packet owns goldens/numerical scopes. Returned bytes are data only:
no import, compiler, GPU, profiler or source fallback execution is permitted.
See doc/AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md.
"""
import hashlib
from pathlib import Path
from h20_h7_common import ROLES, PROFILES, WALL, DEVICE, metadata, eligibility, compare
from gpu_test_results import loads, canonical
from validate_gpu_results import read
from mla_q7_common import check_sanitizer


def audit(result, packet, artifacts):
    """Join every gate to full source/corpus/binary/GPU/launch evidence."""
    if packet is None:raise ValueError('H7 requires the trusted fixed source packet')
    source,table=metadata(packet.root,result['identity']['candidate_id'])
    role=next((r for r in ROLES if PROFILES[r]==result['workload_id']),None)
    if role is None:raise ValueError('unknown H7 workload')
    findings=[]
    def flag(level, code, message):
        """Keep absence, failure and optional diagnostics separate."""
        findings.append(dict(severity=level,code=code,message=message))
    # Combined collector roots are fixed, never supplied by returned JSON.
    root=artifacts.parent
    def document(path):
        """Bounded no-link read relative to the already safely extracted H7 root."""
        return loads(read(root,path))
    try:workflow=document('workflow.json')
    except OSError:
        flag('MISSING','H7_WORKFLOW','Qualification workflow absent.');return findings
    if workflow.get('packet_sha256')!=result['identity']['candidate_id'] or workflow.get('search_invocations')!=0:
        raise ValueError('H7 workflow/source isolation differs')
    if workflow.get('session_id')!=result['identity']['session_id']:raise ValueError('H7 session identity differs')
    if workflow.get('exit_code') not in (0,None):flag('MISSING' if workflow['exit_code'] in (77,124,130,143) else 'ERROR','H7_WORKFLOW_STATUS',str(workflow['exit_code']))
    if workflow.get('check_only'):
        flag('MISSING','H7_CHECK_ONLY','CPU corpus checks are not H20 qualification.');return findings
    expected={case['id']:case for case in table['profiles'][role]}
    if set(c['case_id'] for c in result['cases'])!=set(expected):raise ValueError('H7 common case matrix differs')
    try:correct=document('correctness/report.json')
    except OSError:
        flag('MISSING','H7_CORRECTNESS','Real GPU correctness evidence absent.');return findings
    if correct.get('status')=='UNAVAILABLE' and correct.get('gpu_execution') is False:
        flag('MISSING','H7_HARDWARE_UNAVAILABLE',correct.get('reason') or 'GPU/tool observation unavailable.');return findings
    ok,reason=eligibility(table['gpu_device'],correct.get('device') or {})
    if not ok:flag('ERROR','H20_DEVICE_BINDING',reason)
    if result.get('environment')!=correct.get('device'):raise ValueError('H7 reported GPU differs from native child')
    if correct.get('status')!='PASS' or correct.get('gpu_execution') is not True or correct.get('mode')!='correctness' or correct.get('profiled') is not False:
        flag('ERROR','H7_CORRECTNESS','Real unprofiled correctness did not pass.')
    try:build=document('build/report.json')
    except OSError:build={};flag('MISSING','H7_BUILD','Selected binary/source build evidence absent.')
    required=set()
    for case in expected.values():
        if build.get('cases',{}).get(case['id'],{}).get('source_sha256')!=hashlib.sha256(source.read(case['source'])).hexdigest():
            flag('ERROR','H7_SOURCE_BINDING',case['id'])
        built=build.get('cases',{}).get(case['id'],{})
        binary=read(root,'build/'+case['id']+'/selected.cubin',64*1024**2)
        if hashlib.sha256(binary).hexdigest()!=built.get('binary_sha256'):raise ValueError('H7 cubin identity differs')
        for variant in case['variants']:
            for prefix in (16,256):required.add((case['id'],variant['id'],prefix))
        if 'COMMENT' not in source.read(case['native_trace']).decode():flag('ERROR','H7_COMMENT_PROVENANCE',case['id'])
    def check_records(report, folder, matrix=None):
        """Recheck outputs/guards/inputs and selected launch/resource census."""
        seen=set()
        for row in report.get('cases',[]):
            if row.get('case_id') not in expected:continue
            key=(row['case_id'],row['variant'],row['prefix'])
            if row.get('passed') is not True:flag('ERROR','H7_REFERENCE',str(key)+' did not report a passing invocation')
            if key in seen:raise ValueError('duplicate H7 correctness record')
            seen.add(key);case=expected[row['case_id']]
            variant=next((v for v in case['variants'] if v['id']==row['variant']),None)
            if not variant or row['prefix'] not in (16,256):raise ValueError('unknown H7 variant/residue')
            outputs={b['id'] for b in case['buffers'] if b['role']=='output'}
            if set(row.get('outputs',{}))!=outputs or set(row.get('guards',{}))!={b['id'] for b in case['buffers']}:
                raise ValueError('missing output/guard census')
            for name,path in row['outputs'].items():
                if not compare(read(root,folder+'/'+path),source.read(variant['outputs'][name]),case['numerical']):
                    flag('ERROR','H7_REFERENCE',str(key)+' output '+name)
            for name,guard in row['guards'].items():
                if guard!=(b'\xa7'*(row['prefix']+256)).hex():flag('ERROR','H7_GUARD',str(key)+' '+name)
            for name,path in variant['inputs'].items():
                if row.get('input_sha256',{}).get(name)!=hashlib.sha256(source.read(path)).hexdigest():flag('ERROR','H7_INPUT_MUTATION',str(key)+' '+name)
            resources=row.get('resources',[])
            if len(resources)!=len(case['kernels']):raise ValueError('missing resource/kernel census')
            for resource,kernel in zip(resources,case['kernels']):
                attrs=resource['attributes'];device=correct['device']
                if resource.get('compiler_binding')!=case['compiler_binding'] or resource.get('selected')!=kernel or resource.get('kernel')!=kernel['name'] or resource.get('binary_sha256')!=build['cases'][case['id']]['binary_sha256']:
                    raise ValueError('selected launch/binary/resource binding differs')
                if kernel['threads']>min(attrs['max_threads'],device['max_threads_per_block']) or attrs['static_shared']>min(kernel['shared_bytes'],device['shared_bytes_per_block']) or resource['active_blocks_per_sm']<1:
                    flag('ERROR','H7_RESOURCE_LIMIT',str(key))
                if any(g>device['grid_'+axis] for g,axis in zip(kernel['grid'],('x','y','z'))) or attrs['registers']*kernel['threads']>device['registers_per_block']:
                    flag('ERROR','H7_RESOURCE_LIMIT',str(key)+' grid or register minimum exceeds observed limit')
                estimate=case['registers_estimate']
                if resource.get('register_estimate')!=estimate or resource.get('estimate_exceeded')!=(estimate is not None and attrs['registers']>estimate):
                    raise ValueError('register estimate disposition differs from actual attributes')
                if resource.get('estimate_exceeded'):flag('MISSING','H7_RF_ESTIMATE',case['id']+' actual registers exceed compiler planning reservation; fresh compiler rebind needed.')
        if seen!=(required if matrix is None else matrix):flag('MISSING','H7_MATRIX',folder+' lacks exact variant/residue coverage.')
    check_records(correct,'correctness')
    for tool in ('memcheck','racecheck','initcheck','synccheck'):
        try:
            report=document(tool+'/report.json')
            if report.get('device')!=correct.get('device'):raise ValueError('sanitizer GPU identity differs')
            log=read(root,'logs/'+tool+'.stdout').decode()+read(root,'logs/'+tool+'.stderr').decode()
            check_sanitizer(tool,0 if next((s for s in workflow['steps'] if s['label']==tool),{}).get('exit_code')==0 else 1,log,report)
            check_records(report,tool)
        except OSError:flag('MISSING','H7_SANITIZER',tool+' evidence absent.')
        except ValueError as error:flag('ERROR','H7_SANITIZER',tool+': '+str(error))
    try:
        timing=document('timing/report.json')
        if timing.get('status')!='PASS' or timing.get('gpu_execution') is not True or timing.get('device')!=correct.get('device') or timing.get('profiled') is not False or timing.get('mode')!='timing' or timing.get('timing_samples_emitted') is not True:
            raise ValueError('timing device/instrumentation binding differs')
        timing_matrix={(case['id'],case['variants'][0]['id'],256) for case in expected.values()}
        check_records(timing,'timing',timing_matrix)
        check_records(dict(cases=timing.get('wall_checks',[])),'timing',timing_matrix)
        performance=[r['case_id'] for r in timing.get('performance',[]) if r.get('case_id') in expected]
        if len(performance)!=len(expected) or set(performance)!=set(expected):raise ValueError('timing performance case census differs')
        for common in result['cases']:
            native=next((r for r in timing.get('performance',[]) if r['case_id']==common['case_id']),{})
            if native.get('profiled') is not False or native.get('wall_scope')!=WALL or native.get('device_scope')!=DEVICE or native.get('wall_cuda_events') is not False:
                raise ValueError('wall/event isolation differs')
            if common['timing']['candidate']['wall']['samples']!=native.get('wall_us') or common['timing']['candidate']['device']['samples']!=native.get('device_us'):
                raise ValueError('shared/native timing samples differ')
    except OSError:flag('MISSING','H7_TIMING','Separate real wall/event batches absent.')
    if workflow.get('profiling')=='auto':
        try:
            from mla_q7_profile import summarize_ncu
            profile=document('profiling/report.json')
            if profile.get('benchmark_samples_emitted') is not False or profile.get('search_invocations')!=0 or profile.get('separate_process') is not True:
                raise ValueError('profiling/search/timing isolation differs')
            expected_profile={table['profiles'][role][-1]['id']}
            provided={x['case_id'] for x in profile.get('cases',[]) if x.get('case_id') in expected}
            if provided!=expected_profile:flag('WARNING','H7_PROFILE_COVERAGE','Representative counter case missing.')
            for item in profile.get('cases',[]):
                if item['case_id'] not in expected:continue
                if item['status']!='COLLECTED':
                    flag('WARNING','H7_PROFILE_UNAVAILABLE',item['case_id']);continue
                raw=read(root,'profiling/'+item['csv'])
                if hashlib.sha256(raw).hexdigest()!=item['csv_sha256'] or canonical(summarize_ncu(raw.decode()))!=canonical(item['summary']):
                    raise ValueError('raw hardware counter summary differs')
                execution=document('profiling/'+item['case_id']+'/execution/report.json')
                if execution.get('mode')!='profile' or execution.get('profiled') is not True or execution.get('timing_samples_emitted') is not False or execution.get('performance'):
                    raise ValueError('profile child emitted benchmark timing')
                if execution.get('device')!=correct.get('device'):raise ValueError('profile GPU differs from benchmark')
                read(root,'profiling/'+item['case_id']+'/counters.ncu-rep',64*1024**2)
        except OSError:flag('ERROR' if 'profile' in locals() and profile.get('status')=='COLLECTED' else 'WARNING','H7_PROFILE_UNAVAILABLE','Optional profiler evidence missing; benchmark samples unchanged.')
    if workflow.get('model_calibration')!='QUALIFIED_PHYSICAL_PROFILE':
        flag('MISSING','H7_MODEL_CALIBRATION','Retained compiler profile is synthetic; observed hardware/timing does not qualify its predictions.')
    flag('WARNING','H7_BASELINE_UNKNOWN','No matched native baseline; wall ratio remains null and no speedup is accepted.')
    return findings
