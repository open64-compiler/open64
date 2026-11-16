"""H7 NVIDIA qualification of fixed selected WHIRL-derived H20 products.

Reuses Q7 Driver ownership, child-group lifecycle and sanitizer/profiling
contracts. No compiler search, JIT tuning, candidate lowering or fallback launch.
Contract: doc/AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md.
"""
import argparse
import ctypes as C
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time
import uuid

STATE = dict(device_submission_attempted=False,device=None)
from h20_h7_common import ROLES, PROFILES, WALL, DEVICE, metadata, eligibility, compare, write, sha
from mla_q7_common import authenticate, sanitizer_flags, check_sanitizer
from run_mla_q7 import DRIVER, Unavailable, P, U
from run_mla_q7_workflow import run_step, interrupt, Interrupted
import gpu_test_results as shared


class H7_DRIVER(DRIVER):
    """Extend the existing private Driver surface for fixed pointer-argument kernels."""
    def __init__(self, ordinal):
        """Own one Q7-compatible context and record read-only device/MIG observations."""
        super().__init__(ordinal)
        specs = {'cuDeviceTotalMem_v2': [C.POINTER(C.c_size_t), C.c_int],
                 'cuModuleLoad': [C.POINTER(P), C.c_char_p], 'cuModuleUnload': [P],
                 'cuModuleGetFunction': [C.POINTER(P), P, C.c_char_p],
                 'cuFuncGetAttribute': [C.POINTER(C.c_int), C.c_int, P],
                 'cuOccupancyMaxActiveBlocksPerMultiprocessor': [C.POINTER(C.c_int), P, C.c_int, C.c_size_t],
                 'cuMemAlloc_v2': [C.POINTER(U), C.c_size_t], 'cuMemFree_v2': [U],
                 'cuMemcpyHtoD_v2': [U, P, C.c_size_t], 'cuMemcpyDtoH_v2': [P, U, C.c_size_t],
                 'cuLaunchKernel': [P, C.c_uint, C.c_uint, C.c_uint, C.c_uint, C.c_uint, C.c_uint, C.c_uint, P, P, P]}
        for name, types in specs.items():
            f = getattr(self.lib, name); f.argtypes = types; f.restype = C.c_int; self.functions[name] = f
        dev = C.c_int(); self.call('cuDeviceGet', C.byref(dev), ordinal)
        memory = C.c_size_t(); self.call('cuDeviceTotalMem_v2', C.byref(memory), dev.value)
        self.info['total_memory_bytes'] = memory.value
        for name, number in [('registers_per_block',12),('shared_per_sm',81),('registers_per_sm',82),('threads_per_sm',39),('grid_x',5),('grid_y',6),('grid_z',7)]:
            value=C.c_int();self.call('cuDeviceGetAttribute',C.byref(value),number,dev.value);self.info[name]=value.value
        self.info['mig_enabled'] = None
        try:
            command=['nvidia-smi','--query-gpu=uuid,mig.mode.current','--format=csv,noheader,nounits']
            result=subprocess.run(command,capture_output=True,text=True,timeout=15)
            for line in result.stdout.splitlines() if result.returncode==0 else []:
                ident, mode=[s.strip() for s in line.split(',')]
                if ident.replace('GPU-','').replace('-','').lower()==self.info['uuid']:
                    self.info['mig_enabled']=False if mode in ('Disabled','N/A','[N/A]','Not Supported','[Not Supported]') else True
        except (OSError,ValueError,subprocess.TimeoutExpired):
            pass


class SESSION:
    """Own one immutable-input invocation with guards and selected kernel resources."""
    def __init__(self, driver, root, build, case, variant, prefix):
        """Validate fixed function resources before guarded allocation in this child."""
        self.driver,self.root,self.case,self.variant,self.prefix=driver,root,case,variant,prefix
        self.buffers={};self.functions=[];self.resources=[];self.module=P()
        binary=build/case['id']/'selected.cubin';self.binary_sha=sha(binary)
        driver.call('cuModuleLoad',C.byref(self.module),os.fsencode(binary))
        for kernel in case['kernels']:
            function=P();driver.call('cuModuleGetFunction',C.byref(function),self.module,kernel['name'].encode())
            attrs={}
            for name,attribute in [('max_threads',0),('static_shared',1),('local_bytes',3),('registers',4),('ptx_version',5),('binary_version',6)]:
                value=C.c_int();driver.call('cuFuncGetAttribute',C.byref(value),attribute,function);attrs[name]=value.value
            threads=kernel['threads'];active=C.c_int()
            driver.call('cuOccupancyMaxActiveBlocksPerMultiprocessor',C.byref(active),function,threads,0)
            if threads>min(attrs['max_threads'],driver.info['max_threads_per_block']) or attrs['static_shared']>driver.info['shared_bytes_per_block'] or active.value<1:
                raise ValueError('selected CTA cannot reside on observed device')
            if attrs['static_shared']>kernel['shared_bytes']:
                raise ValueError('compiled shared allocation exceeds selected launch reservation')
            if any(g>driver.info['grid_'+axis] for g,axis in zip(kernel['grid'],('x','y','z'))):
                raise ValueError('selected grid exceeds observed launch limit')
            self.resources.append(dict(kernel=kernel['name'],selected=kernel,compiler_binding=case['compiler_binding'],attributes=attrs,active_blocks_per_sm=active.value,
                register_estimate=case['registers_estimate'],estimate_exceeded=case['registers_estimate'] is not None and attrs['registers']>case['registers_estimate'],binary_sha256=self.binary_sha))
            self.functions.append(function)
        for b in case['buffers']:
            pointer=U();driver.call('cuMemAlloc_v2',C.byref(pointer),prefix+b['bytes']+256);self.buffers[b['id']]=(pointer,b)
            self.store(pointer.value,b'\xa7'*prefix);self.store(pointer.value+prefix+b['bytes'],b'\xa7'*256)
            if b['role']=='input':self.store(pointer.value+prefix,(root/variant['inputs'][b['id']]).read_bytes())
        self.reset()

    def store(self, address, payload):
        """Host initialization stays outside every measured operation boundary."""
        data=C.create_string_buffer(payload);self.driver.call('cuMemcpyHtoD_v2',address,data,len(payload))

    def read(self, address, size):
        """Read only after stream completion; never include oracle transfer in timing."""
        data=C.create_string_buffer(size);self.driver.call('cuMemcpyDtoH_v2',data,address,size);return data.raw

    def reset(self):
        """Poison output interiors; scratch remains uninitialized for initcheck."""
        for pointer,b in self.buffers.values():
            if b['role']=='output':self.store(pointer.value+self.prefix,b'\xcd'*b['bytes'])

    def launch(self):
        """Consume the exact selected launch manifest, without scheduling policy."""
        STATE['device_submission_attempted']=True
        for kernel,function in zip(self.case['kernels'],self.functions):
            values=[U(self.buffers[name][0].value+self.prefix) for name in kernel['arguments']]
            params=(P*len(values))(*[C.cast(C.byref(value),P).value for value in values])
            self.driver.call('cuLaunchKernel',function,*kernel['grid'],kernel['threads'],1,1,0,self.driver.streams[0],params,None)

    def verify(self, out, tag):
        """Retain real outputs and inspect every input/guard after GPU completion."""
        self.driver.call('cuStreamSynchronize',self.driver.streams[0]);outputs={};guards={};inputs={}
        for name,(pointer,b) in self.buffers.items():
            guards[name]=(self.read(pointer.value,self.prefix)+self.read(pointer.value+self.prefix+b['bytes'],256)).hex()
            if guards[name]!=(b'\xa7'*(self.prefix+256)).hex():raise ValueError('allocation guard modified')
            if b['role']=='input':
                actual=self.read(pointer.value+self.prefix,b['bytes']);expected=(self.root/self.variant['inputs'][name]).read_bytes()
                if actual!=expected:raise ValueError('immutable input changed')
                import hashlib
                inputs[name]=hashlib.sha256(actual).hexdigest()
            if b['role']=='output':
                actual=self.read(pointer.value+self.prefix,b['bytes']);expected=(self.root/self.variant['outputs'][name]).read_bytes()
                if not compare(actual,expected,self.case['numerical']):raise ValueError('GPU output differs from independent reference')
                path=out/(tag+'-out'+name+'.bin');path.write_bytes(actual);outputs[name]=path.name
        return dict(case_id=self.case['id'],variant=self.variant['id'],prefix=self.prefix,outputs=outputs,guards=guards,input_sha256=inputs,
            resources=self.resources,binary_sha256=self.binary_sha,passed=True)

    def close(self):
        """Only a known completed stream permits retirement of this invocation."""
        self.driver.call('cuStreamSynchronize',self.driver.streams[0])
        for pointer,b in self.buffers.values():self.driver.call('cuMemFree_v2',pointer)
        self.driver.call('cuModuleUnload',self.module)


def execute(args, out, table):
    """Correctness/profile/timing children have distinct instrumentation contracts."""
    driver=H7_DRIVER(0);STATE['device']=driver.info;ok,reason=eligibility(table['gpu_device'],driver.info)
    if not ok:
        if driver.info.get('mig_enabled') is None:raise Unavailable(reason)
        raise ValueError('H20 target mismatch: '+reason)
    records=[];wall_records=[];performance=[];launched=False
    for role in ROLES:
        for case in table['profiles'][role]:
            if args.case and case['id']!=args.case:continue
            variants=case['variants'] if args.mode=='correctness' else case['variants'][:1]
            for variant in variants:
                for prefix in ((16,256) if args.mode=='correctness' else (256,)):
                    session=SESSION(driver,args.packet,args.build,case,variant,prefix)
                    tag=case['id']+'-'+variant['id']+'-'+str(prefix)
                    if args.mode=='profile':
                        session.launch();driver.call('cuStreamSynchronize',driver.streams[0])
                        driver.profile(True);session.launch();driver.call('cuStreamSynchronize',driver.streams[0]);driver.profile(False);launched=True
                    elif args.mode=='timing':
                        for _ in range(args.warmup):session.launch();driver.call('cuStreamSynchronize',driver.streams[0])
                        wall=[];device=[]
                        for _ in range(args.samples):
                            session.reset();start=time.perf_counter_ns();session.launch();driver.call('cuStreamSynchronize',driver.streams[0]);wall.append((time.perf_counter_ns()-start)/1000)
                        wall_records.append(session.verify(out,tag+'-wall'))
                        start_event=driver.event();stop_event=driver.event()
                        for _ in range(args.samples):
                            session.reset();driver.call('cuEventRecord',start_event,driver.streams[0]);session.launch();driver.call('cuEventRecord',stop_event,driver.streams[0]);driver.call('cuEventSynchronize',stop_event)
                            elapsed=C.c_float();driver.call('cuEventElapsedTime',C.byref(elapsed),start_event,stop_event);device.append(elapsed.value*1000)
                        performance.append(dict(case_id=case['id'],wall_us=wall,device_us=device,profiled=False,wall_scope=WALL,device_scope=DEVICE,wall_cuda_events=False))
                        launched=True
                    else:session.launch();launched=True
                    records.append(session.verify(out,tag));session.close()
    info=driver.info;driver.close()
    return dict(status='PASS',gpu_execution=launched,device=info,cases=records,wall_checks=wall_records,performance=performance,
        mode=args.mode,profiled=args.mode=='profile',timing_samples_emitted=args.mode=='timing',search_invocations=0)


def build(args, out, table, step):
    """Compile existing fixed CUDA outside search/timing; no algorithm is generated."""
    nvcc=shutil.which('nvcc')
    if not nvcc:raise Unavailable('nvcc unavailable')
    records={}
    for cases in table['profiles'].values():
        for case in cases:
            folder=out/case['id'];folder.mkdir()
            source=(args.packet/case['source']).resolve()
            shutil.copyfile(source,folder/'candidate.cu')
            if case.get('math_source'):shutil.copyfile(args.packet/case['math_source'],folder/'math.cu')
            wrapper=folder/'module.cu';wrapper.write_text('#include "candidate.cu"\n'+
                ('#include "math.cu"\n' if case.get('math_source') else ''))
            command=[nvcc,'-std=c++17','-arch=sm_90','--fmad=false','--ftz=false','--prec-div=true','--prec-sqrt=true','--cubin','-Xptxas=-v',wrapper,'-o',folder/'selected.cubin']
            if step('compile-'+case['id'],command):raise RuntimeError('fixed selected CUDA compile failed')
            records[case['id']]=dict(source_sha256=sha(source),binary_sha256=sha(folder/'selected.cubin'),command=list(map(str,command)),compiler_profile=case['compiler_profile'])
    write(out/'report.json',dict(status='PASS',cases=records,search_invocations=0,GPU=False))


def common_results(args, out, table, status):
    """Export four independent shared envelopes, preserving every missing gate."""
    correct=json.loads((out/'correctness/report.json').read_text()) if (out/'correctness/report.json').is_file() else {}
    perf=json.loads((out/'timing/report.json').read_text()) if (out/'timing/report.json').is_file() else {}
    for role in ROLES:
        folder=out/role;folder.mkdir();cases=[]
        for source in table['profiles'][role]:
            checks=[r for r in correct.get('cases',[]) if r['case_id']==source['id']]
            passed=correct.get('status')=='PASS' and len(checks)==len(source['variants'])*2 and all(r.get('passed') for r in checks)
            samples=next((r for r in perf.get('performance',[]) if r['case_id']==source['id']),{})
            record=dict(case_id=source['id'],status='completed' if passed else 'not_run',correctness=dict(candidate_pass=True if passed else None,baseline_pass=None),
                timing={r:dict(wall=shared.timing(samples.get('wall_us') if r=='candidate' else None,WALL),device=shared.timing(samples.get('device_us') if r=='candidate' else None,DEVICE)) for r in ('baseline','candidate')},
                useful_flops=shared.observation(source['useful_flops'],'FLOP','declared','2MNK') if source['useful_flops'] is not None else shared.observation(unit='FLOP',reason='No reviewed full-operation useful-work numerator.'),
                diagnostics=dict(status='not_requested' if args.profiling=='off' else 'see_native_report',benchmark_timing_eligible=False))
            record['wall_time_ratio']=shared.ratio(record,False)
            record['effective_tflops']={}
            for implementation in ('baseline','candidate'):
                measured=record['timing'][implementation]['device']['summary']
                record['effective_tflops'][implementation]=shared.observation(source['useful_flops']/(measured['median']*1e6),'TFLOP/second','derived','Useful work / whole operation event span.') if source['useful_flops'] is not None and measured and measured['median']>0 else shared.observation(unit='TFLOP/second',reason='Matched device duration or reviewed useful-work count unavailable.')
            if args.profiling=='auto' and (out/'profiling/report.json').is_file():
                diagnostic=json.loads((out/'profiling/report.json').read_text())
                item=next((x for x in diagnostic['cases'] if x['case_id']==source['id']),None)
                record['diagnostics'].update(status='not_selected' if item is None else 'collected' if item['status']=='COLLECTED' else 'unsupported',method='ncu_counter_replay',benchmark_timing_eligible=False)
            cases.append(record)
        write(folder/'result.json',dict(schema=shared.SCHEMA,producer='h20-h7',workload_id=PROFILES[role],suite_id='h20-h7-fixed-v1',
            identity=dict(candidate_id=args.expected_sha256,baseline_id=None,corpus_sha256=args.expected_sha256,session_id=status['session_id'],same_device_and_session=False),
            environment=correct.get('device'),status='completed' if correct.get('status')=='PASS' else 'not_run',policy=dict(timing='separate unprofiled wall and event batches'),decision='inconclusive',
            reason='Matched GPU baseline and physical H3 calibration are not yet qualified; no speedup acceptance.',cases=cases,search_invocations=0,
            extensions=dict(h20_h7=dict(packet_sha256=args.expected_sha256,role=role,native_root='..',workflow='workflow.json'))))


def orchestrate(args, out, table):
    """Own finite child lifecycle; gate timing on correctness, races and resources."""
    (out/'logs').mkdir();status=dict(schema='open64.h20-h7.workflow.v1',packet_sha256=args.expected_sha256,session_id=uuid.uuid4().hex,steps=[],search_invocations=0,
        check_only=args.check_only,profiling=args.profiling,exit_code=77,model_calibration='UNQUALIFIED_SYNTHETIC_PROFILE')
    def step(label, command):
        """Reuse existing descendant drain/timeout behavior before collection."""
        row={};status['steps'].append(row);write(out/'workflow.json',status)
        return run_step(label,command,out/'logs',args.timeout,row)
    def child(mode, path):
        """Run one fixed lane in a separate CUDA context; profile emits no samples."""
        return [sys.executable,Path(__file__),'--packet',args.packet,'--expected-sha256',args.expected_sha256,'--mode',mode,
            '--build',out/'build','--output',path,'--samples',str(args.samples),'--warmup',str(args.warmup)]
    code=77
    try:
        if args.check_only:status['reason']='CPU authentication/corpus only; no CUDA or sanitizer evidence';code=0
        else:
            build_dir=out/'build';build_dir.mkdir();build(args,build_dir,table,step)
            code=step('correctness',child('correctness',out/'correctness'))
            if code:raise Unavailable('correctness unavailable') if code==77 else RuntimeError('correctness failed')
            sanitizer=shutil.which('compute-sanitizer')
            if not sanitizer:raise Unavailable('Compute Sanitizer unavailable; timing denied')
            help_text=subprocess.run([sanitizer,'--help'],capture_output=True,text=True,timeout=30).stdout
            for tool in ('memcheck','racecheck','initcheck','synccheck'):
                flags=sanitizer_flags(tool,help_text);path=out/tool
                code=step(tool,[sanitizer,*flags,*child('correctness',path)])
                record=json.loads((path/'report.json').read_text()) if (path/'report.json').is_file() else {}
                log=(out/'logs'/(tool+'.stdout')).read_text()+(out/'logs'/(tool+'.stderr')).read_text()
                check_sanitizer(tool,code,log,record)
                matrix=lambda value:{(r['case_id'],r['variant'],r['prefix'],r['binary_sha256']) for r in value.get('cases',[])}
                if matrix(record)!=matrix(json.loads((out/'correctness/report.json').read_text())):
                    raise ValueError('sanitizer matrix/binary differs from correctness')
            code=step('timing',child('timing',out/'timing'))
            if code:raise RuntimeError('timing failed')
            if args.profiling=='auto':
                from profile_h20_h7 import collect
                collect(args,out/'profiling',table,child,step)
            code=0
    except Unavailable as error:code=77;status['reason']=str(error)
    except (OSError,ValueError,RuntimeError,subprocess.TimeoutExpired) as error:code=code if code not in (0,77) else 1;status['reason']=str(error)
    finally:
        status['exit_code']=code;write(out/'workflow.json',status);common_results(args,out,table,status)
    return code


def main(argv=None):
    """Authenticate the full packet before any trusted code compiles or executes."""
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--packet',type=Path,required=True);parser.add_argument('--expected-sha256',required=True)
    parser.add_argument('--output',type=Path,required=True);parser.add_argument('--build',type=Path)
    parser.add_argument('--mode',choices=('all','correctness','timing','profile','build'),default='all')
    parser.add_argument('--case');parser.add_argument('--check-only',action='store_true')
    parser.add_argument('--profiling',choices=('off','auto'),default='off');parser.add_argument('--profile-timeline',action='store_true')
    parser.add_argument('--samples',type=int,default=20);parser.add_argument('--warmup',type=int,default=3);parser.add_argument('--timeout',type=int,default=3600)
    args=parser.parse_args(argv);args.packet=args.packet.resolve();out=args.output.resolve()
    if out==args.packet or args.packet in out.parents:parser.error('outputs must be outside immutable source')
    if not 1<=args.samples<=10000 or not 0<=args.warmup<=1000 or not 1<=args.timeout<=86400:parser.error('invalid finite execution budget')
    out.mkdir(parents=True,exist_ok=False)
    STATE.update(device_submission_attempted=False,device=None)
    try:
        authenticate(args.packet,args.expected_sha256);_,table=metadata(args.packet,args.expected_sha256)
        if args.case and args.case not in {c['id'] for cases in table['profiles'].values() for c in cases}:raise ValueError('unknown fixed case')
        if args.mode=='all':return orchestrate(args,out,table)
        if args.mode=='build':
            (out/'logs').mkdir()
            def step(label,command):
                """Offline fixed build command has an owned bounded child group."""
                return run_step(label,command,out/'logs',args.timeout,{})
            build(args,out,table,step);return 0
        if not args.build:raise ValueError('execution requires a qualified fixed build directory')
        report=execute(args,out,table);write(out/'report.json',report);return 0
    except Unavailable as error:
        write(out/'report.json',dict(status='UNAVAILABLE',gpu_execution=False,device=STATE['device'],reason=str(error),search_invocations=0));return 77
    except (OSError,ValueError,RuntimeError,subprocess.TimeoutExpired) as error:
        write(out/'report.json',dict(status='FAIL',gpu_execution=False,device=STATE['device'],device_submission_attempted=STATE['device_submission_attempted'],reason=str(error),search_invocations=0));return 1


if __name__=='__main__':raise SystemExit(main())
