"""GS2 remote client with durable submission receipts and offline review.

Use GS1/GS2 transport for control; retain original packet numerical/measurement
contracts. Downloaded source is data, never executable client code. Contract:
doc/GPU-TEST-SERVER-GS2-v1.0.md. OpenCTI remains deferred.
"""
import argparse
from contextlib import contextmanager
import fcntl
import hashlib
import http.client
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys
import tarfile
import time
from urllib.error import HTTPError, URLError
from urllib.parse import urlsplit
from urllib.request import build_opener, ProxyHandler, HTTPRedirectHandler, Request
import uuid

from gpu_test_results import loads
from gpu_test_worker_state import TERMINAL, valid_request, encoded
from gpu_test_worker import child_environment
from gpu_test_suite_common import catalog
from validate_mla_q7_results import manifest, safe_name, digest_file

HERE = Path(__file__).resolve().parent


class ClientError(Exception):
    pass


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def atomic_json(path, value):
    path = Path(path)
    temp = path.with_name('.' + path.name + '.' + uuid.uuid4().hex)
    with temp.open('x') as handle:
        handle.write(encoded(value) + '\n')
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temp, path)
    descriptor = os.open(str(path.parent), os.O_RDONLY | getattr(os, 'O_DIRECTORY', 0))
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


@contextmanager
def output_lock(output):
    output = Path(output).resolve()
    output.mkdir(parents=True, exist_ok=True)
    with (output / '.client.lock').open('a+b') as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise ClientError('Another client is using this output directory.') from None
        try:
            yield output
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)


class Client:
    def __init__(self, server, token_file, timeout=30, attempts=3):
        url = urlsplit(server or '')
        if url.scheme not in ('http', 'https') or not url.hostname or url.username or url.password or url.query or url.fragment or url.path not in ('', '/'):
            raise ClientError('Expected an HTTP(S) server origin without credentials or path.')
        if url.scheme == 'http' and url.hostname not in ('127.0.0.1', 'localhost'):
            raise ClientError('Plain HTTP is limited to loopback; use an SSH tunnel or HTTPS endpoint.')
        path = Path(token_file)
        if path.stat().st_mode & 0o077:
            raise ClientError('Token file must have mode 0600.')
        token = path.read_text().strip()
        if not re.fullmatch(r'[A-Za-z0-9_-]{32,256}', token):
            raise ClientError('Token file contains an invalid bearer value.')
        self.server = server.rstrip('/')
        self.token = token
        self.timeout, self.attempts = timeout, attempts
        self.opener = build_opener(ProxyHandler({}), NoRedirect())
        self.capabilities = None

    def open(self, method, endpoint, value=None, key=None):
        if not endpoint.startswith('/v1/') or endpoint.startswith('//') or urlsplit(endpoint).netloc:
            raise ClientError('Refusing an artifact/control URL outside the configured server.')
        headers = {'Authorization': 'Bearer ' + self.token, 'Accept': 'application/json'}
        raw = None if value is None else encoded(value).encode()
        if raw is not None:
            headers['Content-Type'] = 'application/json'
        if key is not None:
            headers['Idempotency-Key'] = key
        return self.opener.open(Request(self.server + endpoint, data=raw, headers=headers, method=method), timeout=self.timeout)

    def json(self, method, endpoint, value=None, key=None):
        for attempt in range(self.attempts):
            try:
                with self.open(method, endpoint, value, key) as response:
                    raw = response.read(4 * 1024**2 + 1)
                    if len(raw) > 4 * 1024**2:
                        raise ClientError('Server JSON exceeds the client limit.')
                    result = loads(raw)
                    if not isinstance(result, dict):
                        raise ClientError('Server returned a non-object response.')
                    return result
            except HTTPError as error:
                data = error.read(65537)
                try:
                    problem = loads(data)
                    code = problem.get('code', str(error.code))
                    message = str(problem.get('message', ''))[:512]
                except (ValueError, AttributeError):
                    code = str(error.code)
                    message = ''
                if error.code not in (429, 503) or attempt + 1 == self.attempts:
                    raise ClientError('Server refused request: ' + str(code) + (': ' + message if message else '')) from None
            except (URLError, TimeoutError, ConnectionError, http.client.HTTPException):
                if attempt + 1 == self.attempts:
                    raise ClientError('Transport interrupted; reuse the saved output/key or resume by job ID.') from None
            time.sleep(.2 * (attempt + 1))
        raise ClientError('Request retry limit reached.')

    def discover(self):
        value = self.json('GET', '/v1/capabilities')
        if value.get('schema') != 'open64.gpu-test.capabilities.v1' or value.get('contract_version') != '0.2.0' or not isinstance(value.get('principal'), str) or not re.fullmatch(r'server_[0-9a-f]{32}', value.get('server_id', '')):
            raise ClientError('Unsupported server capability contract.')
        self.capabilities = value
        return value

    def job(self, job_id):
        if not re.fullmatch(r'job_[0-9a-f]{32}', job_id):
            raise ClientError('Invalid job identity.')
        value = self.json('GET', '/v1/jobs/' + job_id)
        if value.get('schema') != 'open64.gpu-test.job.v1' or value.get('job_id') != job_id or value.get('server_id') != self.capabilities['server_id']:
            raise ClientError('Job identity differs from the configured server.')
        if value.get('state') not in TERMINAL | {'queued', 'preparing', 'running', 'cancelling', 'collecting', 'validating'}:
            raise ClientError('Unknown job state.')
        if value.get('packet', {}).get('packet_id') != value.get('request', {}).get('packet_id'):
            raise ClientError('Job/source binding differs.')
        return value

    def bind(self, output):
        value = dict(server=self.server, server_id=self.capabilities['server_id'], principal=self.capabilities['principal'])
        path = output / 'server.json'
        if path.exists() and loads(path.read_bytes()) != value:
            raise ClientError('Output belongs to another server or principal; use its original identity.')
        if not path.exists():
            atomic_json(path, value)

    def submit(self, request, output, key=None):
        request = valid_request(request)
        self.bind(output)
        path = output / 'submission.json'
        if path.exists():
            receipt = loads(path.read_bytes())
            if receipt.get('request') != request or key is not None and key != receipt.get('idempotency_key'):
                raise ClientError('Output has a different submission; use a fresh directory for a new run.')
        else:
            receipt = dict(schema='open64.gpu-test.client-submission.v1', request=request,
                idempotency_key=key or uuid.uuid4().hex, job_id=None)
            # Persist before POST: uncertain acceptance can reuse the same key.
            atomic_json(path, receipt)
        value = self.json('POST', '/v1/jobs', request, receipt['idempotency_key'])
        job_id = value.get('job_id', '')
        if not re.fullmatch(r'job_[0-9a-f]{32}', job_id) or receipt['job_id'] not in (None, job_id):
            raise ClientError('Idempotent response returned another job.')
        job = self.job(job_id)
        if job['request'] != request:
            raise ClientError('Server job request differs from the durable submission.')
        receipt['job_id'] = job_id
        atomic_json(path, receipt)
        return job

    def watch(self, job_id, output, wait_timeout=0, poll_seconds=.5, progress=True):
        if not math.isfinite(wait_timeout) or wait_timeout < 0 or not math.isfinite(poll_seconds) or poll_seconds <= 0:
            raise ClientError('Watch limits must be finite and nonnegative, with a positive poll interval.')
        self.bind(output)
        path = output / 'watch.json'
        cursor = 0
        if path.exists():
            saved = loads(path.read_bytes())
            if saved.get('job_id') != job_id:
                raise ClientError('Output contains progress for another job.')
            cursor = saved['after']
        started, previous = time.monotonic(), None
        while True:
            job = self.job(job_id)
            if progress and (job['state'], job['phase']) != previous:
                print(job_id + ': ' + job['state'] + ' / ' + job['phase'], file=sys.stderr, flush=True)
                previous = (job['state'], job['phase'])
            limit = min(100, self.capabilities['limits']['max_events_per_page'])
            while True:
                page = self.json('GET', '/v1/jobs/' + job_id + '/events?after=' + str(cursor) + '&limit=' + str(limit))
                if page.get('schema') != 'open64.gpu-test.events.v1':
                    raise ClientError('Unsupported progress response.')
                for event in page['events']:
                    if type(event.get('sequence')) is not int or event['sequence'] != cursor + 1:
                        raise ClientError('Event history is discontinuous; cursor retained for investigation.')
                    cursor = event['sequence']
                if page['next_after'] != cursor:
                    raise ClientError('Event cursor differs from returned events.')
                atomic_json(path, dict(job_id=job_id, after=cursor))
                if not page['has_more']:
                    break
            atomic_json(output / 'job.json', job)
            if job['state'] in TERMINAL:
                return job
            if wait_timeout and time.monotonic() - started >= wait_timeout:
                raise ClientError('Watch timed out; job ' + job_id + ' continues. Resume watch with the same output.')
            time.sleep(poll_seconds)

    def download(self, row, destination, max_bytes=4 * 1024**3):
        name, size, expected = row.get('name'), row.get('size_bytes'), row.get('sha256')
        artifact_id = row.get('artifact_id', '')
        if not isinstance(name, str) or not re.fullmatch(r'[A-Za-z0-9_.-]+', name) or name in ('.', '..'):
            raise ClientError('Unsafe artifact filename.')
        if type(size) is not int or not 0 <= size <= max_bytes or not re.fullmatch('[0-9a-f]{64}', expected or ''):
            raise ClientError('Artifact size/digest is unsupported.')
        if row.get('download_url') != '/v1/artifacts/' + artifact_id or not re.fullmatch(r'[A-Za-z0-9_]{1,128}', artifact_id):
            raise ClientError('Artifact reference differs from its opaque identity.')
        destination = Path(destination)
        destination.parent.mkdir(parents=True, exist_ok=True)
        if destination.exists() or destination.is_symlink():
            if not destination.is_symlink() and destination.is_file() and destination.stat().st_size == size and digest_file(destination) == expected:
                return destination
            raise ClientError('Existing output differs; refusing to replace it: ' + destination.name)
        temporary = destination.with_name('.' + destination.name + '.' + uuid.uuid4().hex + '.part')
        try:
            with self.open('GET', row['download_url']) as response, temporary.open('xb') as handle:
                if response.headers.get('X-Content-SHA256') != expected or response.headers.get('Content-Length') != str(size):
                    raise ClientError('Transport headers differ from artifact metadata.')
                count = 0
                h = hashlib.sha256()
                for block in iter(lambda: response.read(1048576), b''):
                    count += len(block)
                    if count > size:
                        raise ClientError('Artifact exceeds advertised length.')
                    handle.write(block)
                    h.update(block)
                handle.flush()
                os.fsync(handle.fileno())
            if count != size or h.hexdigest() != expected:
                raise ClientError('Artifact checksum/length mismatch; partial data was not published.')
            os.link(temporary, destination)
            temporary.unlink()
            return destination
        except (HTTPError, URLError, TimeoutError, ConnectionError, http.client.HTTPException):
            raise ClientError('Artifact transfer interrupted; retry fetch with the same output/job ID.') from None

    def fetch(self, job, output, packet_dir=None):
        self.bind(output)
        if job['state'] not in TERMINAL:
            raise ClientError('Job is still active; watch it before fetching its final evidence.')
        artifacts = job['artifacts']
        if len(artifacts) > 1000 or len({x['name'] for x in artifacts}) != len(artifacts):
            raise ClientError('Artifact inventory is ambiguous or too large.')
        inventory = []
        for row in artifacts:
            target = output / 'artifacts' / row['name']
            # Validate the name before it can be used to construct directories.
            if not re.fullmatch(r'[A-Za-z0-9_.-]+', row['name']) or row['name'] in ('.', '..'):
                raise ClientError('Unsafe artifact filename.')
            self.download(row, target)
            inventory.append(dict(metadata=row, relative_path=target.relative_to(output).as_posix()))
        source = None
        if packet_dir is not None:
            source = Path(packet_dir).resolve()
            catalog(source, job['packet']['packet_id'])
        elif any(x['metadata']['name'] == 'Open64_GPU_Test_results.tar.gz' for x in inventory):
            sources = job['packet']['source_artifacts']
            archive = next(x for x in sources if x['kind'] == 'source_archive')
            if archive['sha256'] != job['packet']['archive_sha256'] or archive['size_bytes'] != job['packet']['archive_size_bytes']:
                raise ClientError('Source transport binding differs.')
            path = self.download(archive, output / 'source' / archive['name'])
            source_record = output / 'source.json'
            if source_record.exists():
                record = loads(source_record.read_bytes())
                source = local_path(output, record['relative_path'])
                catalog(source, job['packet']['packet_id'])
            else:
                source = unpack_source(path, output / 'source', job['packet']['packet_id'], archive['sha256'])
                atomic_json(source_record, dict(relative_path=source.relative_to(output).as_posix()))
        result = dict(schema='open64.gpu-test.client-fetch.v1', job=job, artifacts=inventory,
            source_relative_path=source.relative_to(output).as_posix() if source is not None and source.is_relative_to(output) else None)
        atomic_json(output / 'fetch.json', result)
        return review(output, packet_dir=packet_dir)


def local_path(output, relative):
    if not isinstance(relative, str) or not safe_name(relative):
        raise ClientError('Invalid retained evidence path.')
    path = output / relative
    if path.is_symlink() or not path.resolve().is_relative_to(output):
        raise ClientError('Retained evidence escapes the output directory.')
    return path


def unpack_source(archive, parent, packet_id, archive_sha):
    """Integrity/budget preflight before extraction; source remains inert data."""
    if digest_file(archive) != archive_sha:
        raise ClientError('Source archive checksum differs.')
    hashes, sizes, texts, seen, root, total = {}, {}, {}, set(), None, 0
    with tarfile.open(archive, 'r|*') as tar:
        for index, member in enumerate(tar, 1):
            name = member.name.rstrip('/') if member.isdir() else member.name
            if index > 20000 or not safe_name(name) or name in seen or not (member.isdir() or member.isfile()) or member.sparse:
                raise ClientError('Unsafe source archive member or resource limit.')
            seen.add(name)
            current = name.split('/')[0]
            root = root or current
            if root != current:
                raise ClientError('Source archive has multiple roots.')
            if member.isdir():
                continue
            if '/' not in name:
                raise ClientError('Source archive file lacks a root directory.')
            relative = name.split('/', 1)[1]
            total += member.size
            if member.size < 0 or total > 16 * 1024**3:
                raise ClientError('Source archive expansion budget exceeded.')
            if relative in ('SHA256SUMS', 'PACKET_ID') and member.size > 32 * 1024**2:
                raise ClientError('Source manifest exceeds metadata limit.')
            h, parts = hashlib.sha256(), []
            stream = tar.extractfile(member)
            for block in iter(lambda: stream.read(1048576), b''):
                h.update(block)
                if relative in ('SHA256SUMS', 'PACKET_ID'):
                    parts.append(block)
            hashes[relative] = h.hexdigest()
            sizes[relative] = member.size
            if parts:
                texts[relative] = b''.join(parts)
    if hashes.get('SHA256SUMS') != packet_id or texts.get('PACKET_ID', b'').strip() != packet_id.encode():
        raise ClientError('Source manifest identity differs from the submitted packet.')
    expected = manifest(texts['SHA256SUMS'])
    if set(expected) != set(hashes) - {'SHA256SUMS', 'PACKET_ID'} or any(hashes[name] != digest for name, digest in expected.items()):
        raise ClientError('Source file inventory/hashes differ.')
    target = parent / ('unpack-' + uuid.uuid4().hex)
    target.mkdir()
    copied = set()
    with tarfile.open(archive, 'r|*') as tar:
        for member in tar:
            if not member.isfile():
                continue
            if not member.name.startswith(root + '/'):
                raise ClientError('Source archive changed during extraction.')
            relative = member.name[len(root) + 1:]
            if relative not in hashes or relative in copied or member.size != sizes[relative]:
                raise ClientError('Source archive changed during extraction.')
            copied.add(relative)
            path = target / root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            h = hashlib.sha256()
            copied_bytes = 0
            with path.open('xb') as handle:
                stream = tar.extractfile(member)
                for block in iter(lambda: stream.read(1048576), b''):
                    copied_bytes += len(block)
                    if copied_bytes > sizes[relative]:
                        raise ClientError('Source exceeded its authenticated file size.')
                    handle.write(block)
                    h.update(block)
            if h.hexdigest() != hashes[relative]:
                raise ClientError('Source changed during extraction.')
            path.chmod(0o444)
    if copied != set(hashes) or digest_file(archive) != archive_sha:
        raise ClientError('Source archive changed during extraction.')
    source = target / root
    catalog(source, packet_id)
    return source


def review(output, packet_dir=None):
    """Run this client's trusted receiver; never import/execute downloaded code."""
    output = Path(output).resolve()
    fetch = loads((output / 'fetch.json').read_bytes())
    job = fetch['job']
    entries = {}
    for row in fetch['artifacts']:
        path = local_path(output, row['relative_path'])
        if digest_file(path) != row['metadata']['sha256'] or path.stat().st_size != row['metadata']['size_bytes']:
            raise ClientError('Retained artifact changed before review.')
        entries[row['metadata']['name']] = path
    archive = entries.get('Open64_GPU_Test_results.tar.gz')
    if archive is None:
        code = 5 if job['state'] == 'cancelled' else 2
        return dict(job_id=job['job_id'], state=job['state'], receiver_verdict='NOT_RUN', exit_code=code,
            reason='No assessable result archive; retained job/partial artifacts remain available.', output=str(output))
    source = Path(packet_dir).resolve() if packet_dir is not None else local_path(output, fetch['source_relative_path'])
    catalog(source, job['packet']['packet_id'])
    expected = next(x['metadata']['sha256'] for x in fetch['artifacts'] if x['metadata']['name'] == archive.name)
    target = output / ('review-' + uuid.uuid4().hex)
    command = [sys.executable, str(HERE / 'validate_gpu_test_suite.py'), '--archive', str(archive),
        '--expected-archive-sha256', expected, '--packet', str(source), '--expected-packet-sha256', job['packet']['packet_id'],
        '--output', str(target)]
    with (output / 'review.stdout').open('ab') as stdout, (output / 'review.stderr').open('ab') as stderr:
        result = subprocess.run(command, stdout=stdout, stderr=stderr, timeout=900,
            env=child_environment(dict(selected_gpu_uuid=None)))
    summary = loads((target / 'summary.json').read_bytes())
    verdict = {0: 'PASS_REPORTED', 1: 'FAIL', 2: 'INCOMPLETE', 3: 'INVALID'}.get(result.returncode)
    if verdict is None or summary.get('overall') != verdict:
        raise ClientError('Trusted receiver failed to produce a consistent review.')
    code = result.returncode
    if code == 0 and (job['state'] != 'completed' or job['runner_exit_code'] != 0 or job['lease_binding'] == 'fail'):
        code = 1
    report = dict(job_id=job['job_id'], state=job['state'], runner_exit_code=job['runner_exit_code'],
        server_receiver_verdict=job['receiver_verdict'], receiver_verdict=verdict, exit_code=code,
        review=str(target), output=str(output), scope='Original receiver evidence consistency; not independent GPU execution.')
    atomic_json(output / 'review.json', report)
    return report


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--server', default=os.environ.get('GPU_TEST_SERVER'))
    parser.add_argument('--token-file', type=Path, default=os.environ.get('GPU_TEST_TOKEN_FILE'))
    parser.add_argument('--json', action='store_true')
    commands = parser.add_subparsers(dest='command', required=True)
    commands.add_parser('doctor')
    submit = commands.add_parser('submit')
    submit.add_argument('--packet', required=True)
    submit.add_argument('--mode', choices=('qualify', 'check-only'), default='qualify')
    submit.add_argument('--gpu')
    submit.add_argument('--samples', type=int, default=20)
    submit.add_argument('--warmup', type=int, default=3)
    submit.add_argument('--profiling', choices=('off', 'auto'), default='off')
    submit.add_argument('--profile-timeline', action='store_true')
    submit.add_argument('--execution-timeout', type=int, default=21600)
    submit.add_argument('--retry-of')
    submit.add_argument('--key')
    submit.add_argument('--wait', action='store_true')
    for name in ('status', 'cancel', 'watch', 'fetch'):
        command = commands.add_parser(name)
        command.add_argument('job_id')
    commands.add_parser('review')
    for name in ('submit', 'watch', 'fetch', 'review'):
        command = commands.choices[name]
        command.add_argument('--output', type=Path, required=True)
        command.add_argument('--packet-dir', type=Path)
        if name in ('submit', 'watch'):
            command.add_argument('--wait-timeout', type=float, default=0)
    args = parser.parse_args(argv)
    try:
        if getattr(args, 'packet_dir', None) is not None:
            source = args.packet_dir.resolve()
            output = args.output.resolve()
            if output == source or source in output.parents:
                raise ClientError('Client output must be outside the immutable source packet.')
        if args.command == 'review':
            with output_lock(args.output) as output:
                result = review(output, args.packet_dir)
        else:
            if not args.server or not args.token_file:
                raise ClientError('Configure --server and --token-file (or GPU_TEST_SERVER/GPU_TEST_TOKEN_FILE).')
            client = Client(args.server, args.token_file)
            cap = client.discover()
            if args.command == 'doctor':
                result = cap
            elif args.command == 'status':
                result = client.job(args.job_id)
            elif args.command == 'cancel':
                client.job(args.job_id)
                result = client.json('POST', '/v1/jobs/' + args.job_id + '/cancel', {})
            else:
                with output_lock(args.output) as output:
                    if args.command == 'submit':
                        request = dict(schema='open64.gpu-test.job-request.v1', packet_id=args.packet, profile='combined',
                            mode=args.mode, retry_of=args.retry_of, target_gpu_uuid=args.gpu, samples=args.samples,
                            warmup=args.warmup, profiling=args.profiling, profile_timeline=args.profile_timeline,
                            execution_timeout_seconds=args.execution_timeout)
                        job = client.submit(request, output, args.key)
                        print('Job ' + job['job_id'] + ' accepted; receipt: ' + str(output / 'submission.json'), file=sys.stderr)
                        if not args.wait:
                            result = job
                        else:
                            job = client.watch(job['job_id'], output, args.wait_timeout, progress=not args.json)
                            result = client.fetch(job, output, args.packet_dir)
                    elif args.command == 'watch':
                        job = client.watch(args.job_id, output, args.wait_timeout, progress=not args.json)
                        result = client.fetch(job, output, args.packet_dir)
                    else:
                        result = client.fetch(client.job(args.job_id), output, args.packet_dir)
        print(json.dumps(result, indent=2, allow_nan=False))
        return result.get('exit_code', 0)
    except (ClientError, OSError, ValueError, KeyError, TypeError, tarfile.TarError, subprocess.SubprocessError) as error:
        print('GPU client: ' + str(error), file=sys.stderr)
        return 4
    except KeyboardInterrupt:
        print('Client interrupted; accepted work continues. Resume using the saved output/job ID.', file=sys.stderr)
        return 4


if __name__ == '__main__':
    raise SystemExit(main())
