"""Profile fixed selected Q7 requests, separately from benchmark and search.

The default auto lane queries installed NCU capabilities, records raw CSV/native
reports and optionally Nsight Systems timelines. Missing tools/permissions are
explicit partial evidence. No compiler, candidate search or timing lane runs.
Contract: doc/AI-COMPILER-MLA-Q7-PROFILING-v1.0.md.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
from mla_q7_common import authenticate
from mla_q7_profile import SCHEMA, COUNTERS, available_metrics, summarize_ncu, network_rates


def write(path, value):
    path.write_text(json.dumps(value, indent=2, allow_nan=False)+'\n')


def collect(args):
    packet = args.packet.resolve(); out = args.output.resolve()
    if out == packet or packet in out.parents:
        raise ValueError('profiling outputs must be outside the immutable packet')
    out.mkdir(parents=True, exist_ok=False)
    report = dict(schema=SCHEMA, status='INCOMPLETE', packet_sha256=args.expected_sha256,
                  separate_process=True, benchmark_samples_emitted=False, search_invocations=0,
                  aot_selection='deferred', cases=[], commands=[], errors=[],
                  scope='selected fixed kernels only; diagnostic profile durations are never benchmark timing',
                  attribution='No automatic causal local/peer/remote wait attribution; unknowns remain null.')
    def run(label, argv):
        # Own each tool process group, including its CUDA child. Always stop
        # descendants before evidence collection, including timeout/interrupt.
        entry = dict(label=label, argv=list(map(str, argv))); report['commands'].append(entry)
        write(out/'report.json', report)
        child = None
        try:
            with (out/(label+'.stdout')).open('w') as stdout, (out/(label+'.stderr')).open('w') as stderr:
                child = subprocess.Popen(entry['argv'], stdout=stdout, stderr=stderr, start_new_session=True)
                try:
                    code = child.wait(timeout=args.timeout)
                except subprocess.TimeoutExpired:
                    entry.update(returncode=124, status='TIMEOUT')
                    write(out/'report.json', report)
                    raise RuntimeError('profiling child timeout: '+label)
            entry.update(returncode=code, status='CAPTURED' if code==0 else 'FAILED')
        except OSError as error:
            entry.update(returncode=127, status='UNAVAILABLE', reason=str(error))
        finally:
            if child is not None:
                from run_mla_q7_workflow import stop_group
                stop_group(child)
        write(out/'report.json', report)
        return entry['returncode']
    try:
        report['authenticated_files'] = authenticate(packet, args.expected_sha256)
        metadata = json.loads((packet/'packet.json').read_text())
        selected = [x for x in metadata['cases'] if not args.case or x['id'] in args.case]
        if not selected or args.case and set(args.case)-{x['id'] for x in selected}:
            raise ValueError('unknown/empty profile case selection')
        report['expected_cases'] = [x['id'] for x in selected]
        # Topology and identity observations are not stall attribution.
        for label, argv in (
            ('gpu-list', ['nvidia-smi', '-L']), ('topology', ['nvidia-smi', 'topo', '-m']),
            ('link-status', ['nvidia-smi', 'nvlink', '--status']),
            ('ncu-version', [args.ncu, '--version']), ('nsys-version', [args.nsys, '--version'])):
            run(label, argv)
        ncu = shutil.which(args.ncu); nsys = shutil.which(args.nsys) if args.timeline else None
        supported = []
        if ncu and run('ncu-query', [ncu, '--query-metrics', '--query-metrics-mode', 'all', '--devices', str(args.device)]) == 0:
            supported = available_metrics((out/'ncu-query.stdout').read_text())
        report['requested_metrics'] = list(supported)
        report['unavailable_metrics'] = [name for name, unit in COUNTERS.values() if name not in supported]
        for case in selected:
            name = case['id']; row = dict(id=name, status='INCOMPLETE', ncu=None, timeline=None)
            report['cases'].append(row)
            base = [sys.executable, str(packet/'run_mla_q7.py'), '--packet', str(packet),
                    '--expected-sha256', args.expected_sha256, '--mode', 'profile', '--case', name,
                    '--device', str(args.device), '--prefix', '256']
            if ncu and supported:
                folder = out/name; folder.mkdir()
                argv = [ncu, '--profile-from-start', 'off', '--target-processes', 'all',
                        '--replay-mode', 'kernel', '--cache-control', 'all', '--clock-control', 'none',
                        '--metrics', ','.join(supported), '--csv', '--page', 'raw', '--print-units', 'base',
                        '--print-fp', '--print-metric-name', 'name', '--print-kernel-base', 'function', '--log-file', str(folder/'metrics.csv'),
                        '--export', str(folder/'counters')]+base+['--capture-profile-range', '--output', str(folder/'execution')]
                label = name+'-ncu'; code = run(label, argv)
                row['ncu'] = dict(command=label, returncode=code, csv=name+'/metrics.csv',
                                  execution=name+'/execution/report.json', native=name+'/counters.ncu-rep')
                if code == 0:
                    try:
                        raw = (folder/'metrics.csv').read_text()
                        row['ncu']['summary'] = summarize_ncu(raw)
                        row['ncu']['csv_sha256'] = hashlib.sha256(raw.encode()).hexdigest()
                        if len(row['ncu']['summary']['kernels']) != 3:
                            raise ValueError('profile capture must contain exactly three selected stage launches')
                        row['status'] = 'COLLECTED'
                    except (ValueError, OSError) as error:
                        row['ncu']['error'] = str(error)
                elif code in (77, 127):
                    row['reason'] = 'GPU/tool unavailable; see original execution and tool logs.'
            if nsys:
                folder = out/(name+'-timeline'); folder.mkdir()
                label = name+'-nsys'
                argv = [nsys, 'profile', '--trace', 'cuda,nvtx', '--sample', 'none', '--cpuctxsw', 'none',
                        '--capture-range', 'cudaProfilerApi', '--capture-range-end', 'stop',
                        '--output', str(folder/'timeline')]+base+['--capture-profile-range', '--output', str(folder/'execution')]
                code = run(label, argv)
                row['timeline'] = dict(command=label, returncode=code, execution=name+'-timeline/execution/report.json',
                                       native=name+'-timeline/timeline.nsys-rep', attribution='unknown; trace retained for dependency analysis')
            # A plain profile child is still useful when profiling tools are
            # missing: it records Q3 correctness and ambient network deltas, but
            # it does not create synthetic counter measurements or timings.
            if row['ncu'] is None:
                folder = out/(name+'-observation'); label = name+'-observation'
                code = run(label, base+['--output', str(folder)])
                row['observation'] = dict(command=label, returncode=code, execution=name+'-observation/report.json')
            evidence = row.get('ncu') or row.get('observation')
            if evidence and evidence['returncode'] == 0:
                folder = out/Path(evidence['execution']).parent
                try:
                    row['ambient_network'] = network_rates(json.loads((folder/'network-before.json').read_text()),
                                                           json.loads((folder/'network-after.json').read_text()))
                except (OSError, ValueError, KeyError) as error:
                    row['network_reason'] = str(error)
            write(out/'report.json', report)
        report['status'] = 'COLLECTED' if all(x['status']=='COLLECTED' for x in report['cases']) else 'INCOMPLETE'
        if not ncu:
            report['errors'].append('Nsight Compute unavailable; all hardware counter values are unknown.')
    except Exception as error:
        report['status'] = 'FAIL'; report['errors'].append(str(error))
    write(out/'report.json', report)
    return 1 if report['status']=='FAIL' else 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--packet', type=Path, required=True); parser.add_argument('--expected-sha256', required=True)
    parser.add_argument('--output', type=Path, required=True); parser.add_argument('--device', type=int, default=0)
    parser.add_argument('--case', action='append'); parser.add_argument('--timeout', type=int, default=3600)
    parser.add_argument('--ncu', default='ncu'); parser.add_argument('--nsys', default='nsys')
    parser.add_argument('--timeline', action='store_true', help='also retain an independent Nsight Systems timeline')
    args = parser.parse_args(argv)
    if args.device<0 or args.timeout<=0:
        parser.error('invalid device/timeout')
    def interrupted(signum, frame):
        raise InterruptedError('profiling interrupted by signal '+str(signum))
    previous = {sig: signal.signal(sig, interrupted) for sig in (signal.SIGINT, signal.SIGTERM)}
    try:
        return collect(args)
    finally:
        for sig, handler in previous.items():
            signal.signal(sig, handler)


if __name__ == '__main__':
    raise SystemExit(main())
