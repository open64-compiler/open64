"""Run fixed Q7 phases and always attempt authenticated evidence collection.

No GPU algorithm/IR/search changes. Each child has its own process group;
failures, timeouts and catchable interrupts reach the same finalizer. Contract:
doc/AI-COMPILER-MLA-Q7-RESULTS-RETURN-v1.1.md.
"""
import argparse
import datetime
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile


class Interrupted(BaseException):
    def __init__(self, signum):
        self.signum = signum


def utc():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def interrupt(signum, frame):
    """Unwind the active step before entering evidence collection."""
    raise Interrupted(signum)


def stop_group(process):
    """Stop only our new-session child group, including orphaned descendants."""
    try:
        os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        pass
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    # Reap our direct child before snapshotting its logs. Driver-context cleanup
    # is not asserted as numerical correctness or permission for source fallback.
    process.wait(timeout=5)


def run_step(label, argv, logs, timeout, record):
    """Keep exact command/logs and original status; no pipe can mask failure."""
    record.update(label=label, argv=list(map(str, argv)), started_utc=utc())
    process = None
    print('Q7 step:', label, flush=True)
    try:
        with (logs / (label + '.stdout')).open('wb') as stdout, (logs / (label + '.stderr')).open('wb') as stderr:
            process = subprocess.Popen(record['argv'], stdout=stdout, stderr=stderr, start_new_session=True)
            try:
                status = process.wait(timeout=timeout)
                record['exit_code'] = 128-status if status < 0 else status
            except subprocess.TimeoutExpired:
                record.update(exit_code=124, reason='step timeout')
            except Interrupted as error:
                record.update(exit_code=128+error.signum, signal=error.signum)
                raise
    except OSError as error:
        record.update(exit_code=127, reason=str(error))
    finally:
        if process is not None:
            stop_group(process)
        record['finished_utc'] = utc()
    return record['exit_code']


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--packet', type=Path, required=True)
    parser.add_argument('--results-parent', type=Path)
    parser.add_argument('--device', type=int, default=0)
    parser.add_argument('--warmup', type=int, default=3)
    parser.add_argument('--iterations', type=int, default=20)
    parser.add_argument('--timeout', type=int, default=3600, help='Q7 per-GPU/tool child timeout')
    parser.add_argument('--step-timeout', type=int, default=14400, help='whole unit/build/qualification step timeout')
    parser.add_argument('--collection-timeout', type=int, default=900)
    parser.add_argument('--profiling', choices=('auto','off'), default='off', help='opt-in separate diagnostic lane after qualification; never used by timing/search')
    parser.add_argument('--profile-case', action='append', help='fixed case subset for diagnostic profiling; default all cases')
    parser.add_argument('--profile-timeline', action='store_true', help='also capture a separate Nsight Systems timeline')
    args = parser.parse_args(argv)
    if args.profiling=='off' and (args.profile_case or args.profile_timeline):
        parser.error('--profile-case/--profile-timeline require --profiling auto')
    if args.device < 0 or not 0 <= args.warmup <= 1000 or not 1 <= args.iterations <= 10000 or min(args.timeout,args.step_timeout,args.collection_timeout) <= 0:
        parser.error('invalid device/sample/timeout budget')
    packet = args.packet.resolve(); parent = (args.results_parent or packet.parent).resolve()
    if parent == packet or packet in parent.parents:
        parser.error('results-parent must be outside the immutable source packet')
    parent.mkdir(parents=True, exist_ok=True)
    run = Path(tempfile.mkdtemp(prefix='q7-run-', dir=str(parent)))
    logs = run / 'workflow'; logs.mkdir()
    archive = run / 'Open64_MLA_Q7_results.tar.gz'
    status = dict(schema='open64.mla-q7.workflow.v1', started_utc=utc(), packet=str(packet),
                  results_directory=str(run), steps=[], search_invocations=0, aot_selection='deferred',
                  profiling=args.profiling, profiling_exit_code=None)
    print('Q7 results directory:', run, flush=True)
    previous = {sig: signal.signal(sig, interrupt) for sig in (signal.SIGINT, signal.SIGTERM)}
    workflow_code = 1; collector_code = 1; identity = ''; collector = {}
    try:
        try:
            identity = (packet / 'PACKET_ID').read_text().strip()
            status['packet_sha256'] = identity
            commands = [
                ('unit', [sys.executable, '-m', 'unittest', 'discover', '-s', str(packet), '-p', 'test_mla_q7*.py', '-v']),
                ('compile-generated', [sys.executable, str(packet/'build_mla_q7.py'), '--packet', str(packet), '--expected-sha256', identity,
                    '--mode', 'generated', '--output', str(run/'compile-generated')]),
                ('qualification', [sys.executable, str(packet/'run_mla_q7.py'), '--packet', str(packet), '--expected-sha256', identity,
                    '--mode', 'all', '--output', str(run/'qualification'), '--device', str(args.device),
                    '--warmup', str(args.warmup), '--iterations', str(args.iterations), '--timeout', str(args.timeout)])]
            workflow_code = 0
            for label, command in commands:
                record = {}; status['steps'].append(record)
                workflow_code = run_step(label, command, logs, args.step_timeout, record)
                if workflow_code:
                    break
            # A diagnostic profiler failure cannot replace qualification/timing
            # status. It is preserved independently in the returned evidence.
            if workflow_code == 0 and args.profiling == 'auto' and (packet/'profile_mla_q7.py').is_file():
                command = [sys.executable, str(packet/'profile_mla_q7.py'), '--packet', str(packet),
                           '--expected-sha256', identity, '--output', str(run/'profiling'),
                           '--device', str(args.device), '--timeout', str(args.timeout)]
                for name in args.profile_case or []:
                    command += ['--case', name]
                if args.profile_timeline:
                    command += ['--timeline']
                record = {}; status['steps'].append(record)
                status['profiling_exit_code'] = run_step('profiling', command, logs, args.step_timeout, record)
        except Interrupted as error:
            workflow_code = 128+error.signum; status['signal'] = error.signum
        except Exception as error:
            workflow_code = 1; status['error'] = str(error)
        finally:
            # This immutable pre-collection status goes INSIDE the return tar.
            # Collector output/final completion stay outside its input roots so
            # its own live logs cannot create a changing-file snapshot.
            status.update(finished_utc=utc(), exit_code=workflow_code,
                          status='COMPLETED' if workflow_code==0 else 'FAILED_OR_INTERRUPTED')
            (logs/'status.json').write_text(json.dumps(status, indent=2)+'\n')
            try:
                collector_code = run_step('collector', [sys.executable, str(packet/'package_mla_q7_results.py'),
                    '--packet', str(packet), '--expected-sha256', identity, '--results', str(logs), str(run/'compile-generated'),
                    str(run/'qualification')]+([str(run/'profiling')] if (run/'profiling').exists() else [])+
                    ['--output', str(archive)], run, args.collection_timeout, collector)
            except Interrupted as error:
                collector_code = 128+error.signum; collector.update(exit_code=collector_code, signal=error.signum)
            except Exception as error:
                collector_code = 1; collector.update(exit_code=collector_code, error=str(error))
    finally:
        for sig, handler in previous.items(): signal.signal(sig, handler)
    created = archive.is_file() and Path(str(archive)+'.sha256').is_file()
    if collector_code == 0 and not created:
        collector_code = 1; collector['error'] = 'collector returned without archive/checksum'
    final_code = workflow_code or collector_code
    complete = dict(workflow_exit_code=workflow_code, collector_exit_code=collector_code, exit_code=final_code,
                    profiling_exit_code=status['profiling_exit_code'],
                    collector=collector, results_directory=str(run), archive=str(archive), checksum=str(archive)+'.sha256',
                    archive_created=created,
                    hardware_qualification='read original Q7 reports; collection is not acceptance')
    (run/'completion.json').write_text(json.dumps(complete, indent=2)+'\n')
    if complete['archive_created']:
        print('SEND BACK:', archive, '\nCHECKSUM:', str(archive)+'.sha256', flush=True)
    else:
        print('RESULT COLLECTION FAILED; logs retained at:', run, file=sys.stderr, flush=True)
    print('Workflow exit:', workflow_code, 'Collector exit:', collector_code, 'Final exit:', final_code, flush=True)
    return final_code


if __name__ == '__main__':
    raise SystemExit(main())
