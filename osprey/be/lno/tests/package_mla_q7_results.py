"""Package Q7 results and read-only NVIDIA/CUDA environment observations.

Standard-library-only return collector, usable with Q7 v1.0 and later packets.
It preserves original reports (including failures), never runs GPU workloads,
and never decides hardware qualification. Design:
doc/AI-COMPILER-MLA-Q7-RESULTS-RETURN-v1.0.md.
"""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile


def digest_file(path):
    """Bound memory while hashing potentially large source/result files."""
    digest = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            digest.update(chunk)
    return digest.hexdigest()


def authenticate_packet(root, expected):
    """Bind results to the exact source packet before collecting any environment."""
    manifest = root / 'SHA256SUMS'
    if digest_file(manifest) != expected:
        raise ValueError('packet manifest identity differs')
    seen = set()
    for row in manifest.read_text().splitlines():
        digest, name = row.split('  ', 1)
        path = root / name
        if name in seen or Path(name).is_absolute() or '..' in Path(name).parts or path.is_symlink() or root not in path.resolve().parents:
            raise ValueError('unsafe or duplicate packet path: ' + name)
        if digest_file(path) != digest:
            raise ValueError('packet file differs: ' + name)
        seen.add(name)
    return len(seen)


def snapshot_file(source, destination):
    """Copy one regular file without following a final symlink; flag live writes."""
    destination.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(str(source), os.O_RDONLY | getattr(os, 'O_NOFOLLOW', 0))
    with os.fdopen(descriptor, 'rb') as incoming:
        before = os.fstat(incoming.fileno())
        if not stat.S_ISREG(before.st_mode):
            raise ValueError('not a regular file')
        with destination.open('xb') as outgoing:
            shutil.copyfileobj(incoming, outgoing, 1024 * 1024)
        after = os.fstat(incoming.fileno())
    return (before.st_size, before.st_mtime_ns, before.st_ctime_ns) != (after.st_size, after.st_mtime_ns, after.st_ctime_ns)


def capture_environment(folder, timeout):
    """Read versions/settings only; unavailable tools retain diagnostics, not PASS."""
    folder.mkdir()
    allowed = ('CUDA_HOME', 'CUDA_PATH', 'CUDA_VISIBLE_DEVICES', 'CUDA_MODULE_LOADING',
               'NVIDIA_VISIBLE_DEVICES', 'NVIDIA_DRIVER_CAPABILITIES', 'PATH', 'LD_LIBRARY_PATH')
    # os.uname is a syscall; platform.uname on older Python can spawn an
    # unbounded `uname -p` subprocess while deriving its processor field.
    host = os.uname() if hasattr(os, 'uname') else None
    observations = dict(captured_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                        timing='collection-time observations; original run-time snapshots remain in results',
                        system=host.sysname if host else sys.platform, release=host.release if host else None,
                        kernel_version=host.version if host else None, machine=host.machine if host else None, python=sys.version,
                        variables={key: os.environ[key] for key in allowed if key in os.environ}, commands=[])
    cuda = Path(os.environ.get('CUDA_HOME') or os.environ.get('CUDA_PATH') or '/usr/local/cuda')
    commands = [('nvidia-smi-list', 'nvidia-smi', ['-L']), ('nvidia-smi-query', 'nvidia-smi', ['-q'])]
    for tool in ('nvcc', 'ptxas', 'nvlink', 'compute-sanitizer', 'ncu', 'nsys'):
        local = cuda / 'bin' / tool
        commands.append((tool, str(local) if local.is_file() else tool, ['--version']))
    commands.append(('host-cxx', 'g++', ['--version']))
    for label, tool, flags in commands:
        executable = shutil.which(tool)
        entry = dict(label=label, executable=executable, argv=([executable] + flags) if executable else [tool] + flags)
        stdout = stderr = ''
        if not executable:
            entry['status'] = 'UNAVAILABLE'
        else:
            try:
                result = subprocess.run([executable] + flags, capture_output=True, text=True, errors='replace', timeout=timeout)
                stdout, stderr = result.stdout, result.stderr
                entry.update(status='CAPTURED' if result.returncode == 0 else 'COMMAND_FAILED', returncode=result.returncode)
            except subprocess.TimeoutExpired as error:
                def text(value):
                    return value.decode(errors='replace') if isinstance(value, bytes) else value or ''
                stdout, stderr = text(error.stdout), text(error.stderr)
                entry['status'] = 'TIMEOUT'
            except OSError as error:
                stderr = str(error); entry['status'] = 'UNAVAILABLE'
        (folder / (label + '.stdout')).write_text(stdout)
        (folder / (label + '.stderr')).write_text(stderr)
        observations['commands'].append(entry)
    for label, name in (('os-release', '/etc/os-release'), ('nvidia-kernel-driver', '/proc/driver/nvidia/version')):
        try:
            with open(name, 'r') as stream:
                content = stream.read(131072)
            (folder / (label + '.txt')).write_text(content)
        except OSError as error:
            observations[label] = dict(status='UNAVAILABLE', reason=str(error))
    (folder / 'capture.json').write_text(json.dumps(observations, indent=2) + '\n')
    return observations


def collect_results(args):
    """Snapshot explicit result roots, then atomically publish a new return tar."""
    packet = args.packet.resolve(); output = args.output.absolute()
    checksum = output.with_name(output.name + '.sha256')
    if any(char in output.name for char in '\n\r\\'):
        raise ValueError('output filename contains an unsupported control/escape character')
    if output.exists() or output.is_symlink() or checksum.exists() or checksum.is_symlink():
        raise ValueError('output archive/checksum already exists; choose a new filename')
    if packet == output or packet in output.resolve().parents:
        raise ValueError('write the return archive outside the immutable source packet')
    for source in args.results:
        if source.resolve() == packet:
            raise ValueError('results must be run outputs, not the source packet')
        if source.is_dir() and (source.resolve() == output.resolve() or source.resolve() in output.resolve().parents):
            raise ValueError('return archive must be outside every collected result directory')
    verified = authenticate_packet(packet, args.expected_sha256)
    output.parent.mkdir(parents=True, exist_ok=True)
    notes = []; roots = []; reports = []
    with tempfile.TemporaryDirectory(prefix='.q7-return-', dir=str(output.parent)) as temporary:
        work = Path(temporary); bundle = work / 'Open64_MLA_Q7_results'; bundle.mkdir()
        provenance = bundle / 'packet'; provenance.mkdir()
        for name in ('PACKET_ID', 'SHA256SUMS', 'packet.json', 'harness-source-hashes.json',
                     'build-platform.json', 'isolation.json', 'unchanged-generated-source.json'):
            source = packet / name
            if source.is_file():
                shutil.copyfile(source, provenance / name)
        for index, supplied in enumerate(args.results, 1):
            source = supplied.absolute()
            label = '%02d-%s' % (index, re.sub(r'[^A-Za-z0-9_.-]', '_', source.name)[:100] or 'results')
            entry = dict(label=label, supplied_path=str(source), files=0)
            roots.append(entry)
            if source.is_symlink() or not source.is_dir():
                entry['status'] = 'MISSING_OR_NOT_A_REAL_DIRECTORY'
                notes.append(dict(path=str(source), reason=entry['status'])); continue
            entry['status'] = 'SNAPSHOTTED'
            def walk_error(error):
                notes.append(dict(path=str(error.filename), reason=str(error)))
            for parent, directories, files in os.walk(str(source), followlinks=False, onerror=walk_error):
                directories.sort(); files.sort()
                for name in list(directories):
                    path = Path(parent) / name
                    if path.is_symlink():
                        directories.remove(name); notes.append(dict(path=str(path), reason='symlink skipped'))
                for name in files:
                    path = Path(parent) / name; relative = path.relative_to(source)
                    if any(char in str(relative) for char in '\n\r\\'):
                        notes.append(dict(path=str(path), reason='unsupported control/escape character')); continue
                    if path.is_symlink() or not path.is_file():
                        notes.append(dict(path=str(path), reason='symlink/non-regular file skipped')); continue
                    destination = bundle / 'results' / label / relative
                    try:
                        changed = snapshot_file(path, destination)
                    except (OSError, ValueError) as error:
                        if destination.exists(): destination.unlink()
                        notes.append(dict(path=str(path), reason=str(error))); continue
                    entry['files'] += 1
                    if changed:
                        notes.append(dict(path=str(path), reason='file changed during snapshot; run may still be active'))
                    if name in ('report.json', 'build-report.json', 'progress.json'):
                        record = dict(file=destination.relative_to(bundle).as_posix())
                        try:
                            original = json.loads(destination.read_text())
                            if isinstance(original, dict):
                                for key in ('status', 'mode', 'gpu_execution', 'device_submission_attempted', 'race_qualified', 'all_fixed_cases', 'device', 'timing_scope', 'reason'):
                                    if key in original: record[key] = original[key]
                                bound = original.get('packet_sha256')
                                record['packet_binding'] = 'NOT_RECORDED' if bound is None else 'MATCH' if bound == args.expected_sha256 else 'MISMATCH'
                                if bound is not None: record['reported_packet_sha256'] = bound
                                if record['packet_binding'] == 'MISMATCH': notes.append(dict(path=str(path), reason='report belongs to a different packet'))
                            else: record['kind'] = 'progress list'
                        except (ValueError, UnicodeError) as error:
                            record['parse_error'] = str(error); notes.append(dict(path=str(path), reason='invalid/incomplete JSON retained verbatim'))
                        reports.append(record)
        environment = capture_environment(bundle / 'environment', args.command_timeout)
        metadata = dict(schema='open64.mla-q7.return-bundle.v1', packaging_status='PACKAGED_WITH_NOTES' if notes else 'PACKAGED',
                        qualification_status='NOT_ASSESSED_BY_COLLECTOR', created_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                        packet_sha256=args.expected_sha256, authenticated_packet_files=verified, result_roots=roots, report_inventory=reports,
                        collection_notes=notes, environment_commands=environment['commands'],
                        original_report_bytes_preserved=True, GPU_workloads_launched_by_collector=0,
                        search_invocations=0, aot_selection='deferred')
        (bundle / 'bundle.json').write_text(json.dumps(metadata, indent=2) + '\n')
        source_packet=json.loads((packet/'packet.json').read_text())
        if source_packet.get('shared_results_contract')=='open64.gpu-test.results.v1':
            # Adapt the completed snapshot, never live GPU buffers. Original
            # native reports remain authoritative for the receiver's Q7 audit.
            from gpu_test_results import from_q7
            native={p.relative_to(bundle).as_posix():p.read_bytes() for p in bundle.rglob('report.json') if p.stat().st_size<=32*1024**2}
            try:
                shared=from_q7(source_packet,args.expected_sha256,native)
                (bundle/'result.json').write_text(json.dumps(shared,indent=2,allow_nan=False)+'\n')
            except (ValueError,KeyError,TypeError,AttributeError,RecursionError,OverflowError) as error:
                # A torn/invalid child report is exactly the evidence an always-
                # return collector must preserve. Do not synthesize successful
                # common data or prevent publication of the original bytes.
                notes.append(dict(path='result.json',reason='Shared JSON adaptation unavailable: '+str(error)))
                metadata['packaging_status']='PACKAGED_WITH_NOTES'
                (bundle/'shared-result-error.json').write_text(json.dumps(dict(status='UNAVAILABLE',reason=str(error)),indent=2)+'\n')
                (bundle/'bundle.json').write_text(json.dumps(metadata,indent=2)+'\n')
        (bundle / 'README.md').write_text('''# Open64 MLA Q7 returned evidence

Packaging success is not hardware qualification. Inspect bundle.json and the
original report/log bytes under results/. Failed, partial, unavailable and
different-packet results are preserved and identified rather than rewritten.
Missing paths, skipped links, unreadable or changing files appear in notes.
The collector launches no GPU workload, search, lowering or fallback.

environment/ captures read-only collection-time GPU/Driver/CUDA/tool versions.
Original run-time snapshots and device/timing facts remain under results/.
nvidia-smi's CUDA version describes Driver capability; nvcc --version describes
the installed toolkit. Missing tools are recorded, never treated as passed.
Only explicitly named result directories and allowlisted environment variables
are collected. Source packet identities/manifests are under packet/; the full
source packet is not duplicated. Check SHA256SUMS after extraction.
''')
        manifest = ''.join(digest_file(path) + '  ' + path.relative_to(bundle).as_posix() + '\n'
                           for path in sorted(bundle.rglob('*')) if path.is_file())
        (bundle / 'SHA256SUMS').write_text(manifest)
        archive = work / 'return.tar.gz'
        def normalize(info):
            info.uid = info.gid = 0; info.uname = info.gname = ''
            info.mode = 0o755 if info.isdir() else 0o644
            return info
        with tarfile.open(str(archive), 'w:gz') as tar:
            for path in sorted(bundle.rglob('*')):
                tar.add(str(path), arcname='Open64_MLA_Q7_results/' + path.relative_to(bundle).as_posix(), recursive=False, filter=normalize)
        # Both published files are complete and use no-replace links. Temporary
        # data is on the same filesystem and only our staging directory is removed.
        digest = digest_file(archive); sidecar = work / 'checksum'
        sidecar.write_text(digest + '  ' + output.name + '\n')
        os.link(str(archive), str(output)); os.link(str(sidecar), str(checksum))
    return dict(archive=str(output), checksum=str(checksum), sha256=digest, size_bytes=output.stat().st_size,
                packaging_status=metadata['packaging_status'], qualification_status=metadata['qualification_status'],
                copied_result_files=sum(x['files'] for x in roots), collection_notes=len(notes))


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--packet', type=Path, required=True)
    parser.add_argument('--expected-sha256', required=True)
    parser.add_argument('--results', type=Path, nargs='+', required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--command-timeout', type=int, default=15)
    args = parser.parse_args(argv)
    try:
        if not 1 <= args.command_timeout <= 120: raise ValueError('command-timeout must be 1..120 seconds')
        result = collect_results(args); print(json.dumps(result, indent=2)); return 0
    except (OSError, ValueError) as error:
        print('Results packaging failed: ' + str(error), file=sys.stderr); return 1


if __name__ == '__main__':
    raise SystemExit(main())
