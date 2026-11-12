"""One command for aligned GEMM, BF16 MLA and full quantized MLA/Q3 tests.

Own orchestration/finalization only. Existing authenticated entry points own
correctness, timing and optional independent profiling. No compiler search.
Contract: doc/GPU-TEST-COMBINED-PACKAGE-v1.0.md.
"""
import argparse
import os
from pathlib import Path
import signal
import sys
import tempfile
from gpu_test_suite_common import catalog,write,stage_scaffold
from run_mla_q7_workflow import Interrupted,interrupt,run_step,utc


def main(argv=None):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--packet',type=Path,required=True);parser.add_argument('--results-parent',type=Path)
    parser.add_argument('--samples',type=int,default=20);parser.add_argument('--warmup',type=int,default=3)
    parser.add_argument('--gpu',help='optional single CUDA_VISIBLE_DEVICES ordinal or UUID; all jobs use visible device 0')
    parser.add_argument('--profiling',choices=('off','auto'),default='off')
    parser.add_argument('--profile-timeline',action='store_true',help='additional separate Q7 Nsight Systems trace')
    parser.add_argument('--check-only',action='store_true',help='CPU/corpus checks only; never hardware qualification')
    parser.add_argument('--timeout',type=int,default=3600);parser.add_argument('--step-timeout',type=int,default=14400)
    parser.add_argument('--collection-timeout',type=int,default=900)
    args=parser.parse_args(argv)
    if sys.version_info<(3,9):parser.error('Combined suite requires Python >=3.9 (existing scaffold requirement)')
    if not 1<=args.samples<=10000 or not 0<=args.warmup<=1000 or min(args.timeout,args.step_timeout,args.collection_timeout)<=0:
        parser.error('invalid sample/timeout budget')
    if args.gpu is not None and (not args.gpu or ',' in args.gpu or any(x.isspace() for x in args.gpu)):
        parser.error('--gpu must select one visible GPU')
    if args.profile_timeline and args.profiling!='auto':parser.error('--profile-timeline requires --profiling auto')
    packet=args.packet.resolve();parent=(args.results_parent or packet.parent).resolve()
    if parent==packet or packet in parent.parents:parser.error('results must be outside the source packet')
    parent.mkdir(parents=True,exist_ok=True);run=Path(tempfile.mkdtemp(prefix='gpu-suite-run-',dir=parent))
    logs=run/'workflow';logs.mkdir();q7=run/'mla-q7';(q7/'workflow').mkdir(parents=True)
    status=dict(schema='open64.gpu-test.suite-workflow.v1',packet_sha256=None,started_utc=utc(),steps=[],
        check_only=args.check_only,profiling=args.profiling,workloads={},search_invocations=0,aot_selection='deferred')
    print('GPU suite results:',run,flush=True)
    previous={sig:signal.signal(sig,interrupt) for sig in (signal.SIGINT,signal.SIGTERM)}
    old_visibility=os.environ.get('CUDA_VISIBLE_DEVICES');old_bytecode=os.environ.get('PYTHONDONTWRITEBYTECODE')
    if args.gpu is not None:os.environ['CUDA_VISIBLE_DEVICES']=args.gpu
    os.environ['PYTHONDONTWRITEBYTECODE']='1'
    status['CUDA_VISIBLE_DEVICES']=os.environ.get('CUDA_VISIBLE_DEVICES')
    code=0;inner_code=None;collect_code=1;identity='';table=None
    def step(label,command,timeout=None):
        row={};status['steps'].append(row)
        return run_step(label,command,logs,timeout or args.step_timeout,row)
    try:
        try:
            identity=(packet/'PACKET_ID').read_text().strip();status['packet_sha256']=identity
            table=catalog(packet,identity)
            status['packets']=table['packets']
            scaffold=stage_scaffold(packet/'scaffold',run/'scaffold-work');native=packet/'mla-q7'
            status['scaffold_execution_directory']=str(scaffold)
            unit=step('scaffold-unit',[sys.executable,'-m','unittest','discover','-s',scaffold/'tests','-v'])
            code=code or unit
            for role,kind in (('gemm','gemm'),('mla-core','mla')):
                command=[sys.executable,scaffold/'server/run.py',scaffold/'submissions'/kind,
                    '--output',run/role/'result.json','--samples',str(args.samples),'--warmup',str(args.warmup),'--timeout',str(args.timeout)]
                if args.check_only:command+=['--check-only']
                elif args.profiling=='auto':command+=['--profile-pm']
                rc=step(role,command);status['workloads'][role]=dict(exit_code=rc)
                code=code or rc
            q7steps=[];q7code=0;profile_code=None
            commands=[('unit',[sys.executable,'-m','unittest','discover','-s',native,'-p','test_mla_q7*.py','-v'])]
            if not args.check_only:
                commands += [('compile-generated',[sys.executable,native/'build_mla_q7.py','--packet',native,
                    '--expected-sha256',table['packets']['mla-q7']['packet_sha256'],'--mode','generated','--output',q7/'compile-generated']),
                    ('qualification',[sys.executable,native/'run_mla_q7.py','--packet',native,
                    '--expected-sha256',table['packets']['mla-q7']['packet_sha256'],'--mode','all','--output',q7/'qualification',
                    '--device','0','--iterations',str(args.samples),'--warmup',str(args.warmup),'--timeout',str(args.timeout)])]
            for label,command in commands:
                q7code=step('q7-'+label,command);q7steps.append(status['steps'][-1])
                if q7code:break
            status['workloads']['mla-q7']=dict(exit_code=q7code,check_only=args.check_only)
            code=code or q7code
            if not q7code and not args.check_only and args.profiling=='auto':
                command=[sys.executable,native/'profile_mla_q7.py','--packet',native,
                    '--expected-sha256',table['packets']['mla-q7']['packet_sha256'],'--output',q7/'profiling','--device','0','--timeout',str(args.timeout)]
                if args.profile_timeline:command+=['--timeline']
                profile_code=step('q7-profiling',command);q7steps.append(status['steps'][-1])
            write(q7/'workflow/status.json',dict(schema='open64.mla-q7.workflow.v1',packet_sha256=table['packets']['mla-q7']['packet_sha256'],
                exit_code=q7code,steps=q7steps,check_only=args.check_only,profiling=args.profiling,profiling_exit_code=profile_code))
        except Interrupted as error:
            code=128+error.signum;status['signal']=error.signum
        except Exception as error:
            code=code or 1;status['error']=str(error)
        finally:
            # Leaf process groups are stopped by run_step before either snapshot.
            # Q7's original collector creates its canonical native return first.
            if table is not None:
                try:
                    paths=[q7/'workflow',q7/'compile-generated',q7/'qualification']
                    if (q7/'profiling').exists():paths.append(q7/'profiling')
                    inner_code=step('q7-results',[sys.executable,packet/'mla-q7/package_mla_q7_results.py','--packet',packet/'mla-q7',
                        '--expected-sha256',table['packets']['mla-q7']['packet_sha256'],'--results',*paths,
                        '--output',q7/'Open64_MLA_Q7_results.tar.gz'],args.collection_timeout)
                except Interrupted as error:
                    inner_code=128+error.signum;code=code or inner_code;status['signal']=error.signum
                except Exception as error:
                    inner_code=1;status['q7_collection_error']=str(error)
            status.update(finished_utc=utc(),exit_code=code,inner_collection_exit_code=inner_code,
                          status='COMPLETED_CHECK_ONLY' if args.check_only and code==0 else 'COMPLETED' if code==0 else 'FAILED_OR_INTERRUPTED')
            write(logs/'status.json',status)
            # Collector logs live outside its input roots; no live-file snapshot.
            try:
                record={}
                collect_code=run_step('collect-results',[sys.executable,Path(__file__).with_name('package_gpu_test_suite_results.py'),
                    '--packet',packet,'--expected-sha256',identity,'--run',run,'--output',run/'Open64_GPU_Test_results.tar.gz'],
                    run,args.collection_timeout,record)
            except Interrupted as error:
                collect_code=128+error.signum;code=code or collect_code;status['signal']=error.signum
            except Exception as error:
                collect_code=1;status['collection_error']=str(error)
    finally:
        for sig,handler in previous.items():signal.signal(sig,handler)
        if old_visibility is None:os.environ.pop('CUDA_VISIBLE_DEVICES',None)
        else:os.environ['CUDA_VISIBLE_DEVICES']=old_visibility
        if old_bytecode is None:os.environ.pop('PYTHONDONTWRITEBYTECODE',None)
        else:os.environ['PYTHONDONTWRITEBYTECODE']=old_bytecode
    archive=run/'Open64_GPU_Test_results.tar.gz';created=archive.is_file() and Path(str(archive)+'.sha256').is_file()
    if not created:collect_code=collect_code or 1
    final=code or inner_code or collect_code
    write(run/'completion.json',dict(exit_code=final,workflow_exit_code=code,q7_collection_exit_code=inner_code,
        collection_exit_code=collect_code,archive_created=created,archive=str(archive)))
    if created:print('SEND BACK:',archive,'\nCHECKSUM:',str(archive)+'.sha256',flush=True)
    else:print('Collection failed; logs retained at',run,file=sys.stderr)
    return final


if __name__=='__main__':raise SystemExit(main())
