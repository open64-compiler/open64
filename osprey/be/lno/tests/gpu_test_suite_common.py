"""Combined GPU-suite identities and safe data snapshots; no GPU execution.

Existing scaffold/Q7 workload and shared result contracts remain authoritative.
Controlling specification: doc/GPU-TEST-COMBINED-PACKAGE-v1.0.md.
"""
import json
import hashlib
from pathlib import Path
import os
import re
from package_mla_q7_results import authenticate_packet, digest_file, snapshot_file
from gpu_test_results import loads
from validate_gpu_results import Packet

SCHEMA='open64.gpu-test.suite.v1'
RETURN_SCHEMA='open64.gpu-test.suite-return.v1'
ROOT='Open64_GPU_Test_results/'
ROLES=('gemm','mla-core','mla-q7')


def write(path,value):
    Path(path).write_text(json.dumps(value,indent=2,allow_nan=False)+'\n')


def stage_scaffold(source,target):
    """Copy only authenticated scaffold files to a fresh writable run tree.

    The existing server owns .build relative to its code root. This staging
    boundary preserves its scripts and source identities while keeping caches
    outside a potentially read-only installation and isolating each run's logs.
    """
    from validate_mla_q7_results import manifest
    source=Path(source);target=Path(target);target.mkdir(parents=True,exist_ok=False)
    entries=manifest((source/'SHA256SUMS').read_bytes())
    for name in list(entries)+['SHA256SUMS','PACKET_ID']:
        destination=target/name
        snapshot_file(source/name,destination)
        if name in entries and digest_file(destination)!=entries[name]:raise ValueError('scaffold changed during staging: '+name)
    return target


def catalog(packet,expected,full=True):
    """Bind the outer catalog and both unchanged inner packet identities."""
    packet=Path(packet).resolve()
    if full:authenticate_packet(packet,expected)
    source=Packet(packet,expected)
    table=loads(source.read('suite.json'))
    if table.get('schema')!=SCHEMA or set(table.get('packets',{}))!={'scaffold','mla-q7'}:
        raise ValueError('unsupported combined source catalog')
    for name in ('scaffold','mla-q7'):
        entry=table['packets'][name]
        if entry.get('directory')!=name or not re.fullmatch('[0-9a-f]{64}',entry.get('packet_sha256','')):
            raise ValueError('invalid inner packet binding')
        if hashlib.sha256(source.read(name+'/SHA256SUMS')).hexdigest()!=entry['packet_sha256']:
            raise ValueError('inner packet identity differs: '+name)
    return table


def snapshot(source,destination,notes):
    """Snapshot only explicit run directories, retaining partial/malformed data."""
    source=Path(source)
    if source.is_symlink() or not source.is_dir():
        notes.append(dict(path=str(source),reason='MISSING_OR_NOT_A_REAL_DIRECTORY'));return
    for parent,dirs,files in os.walk(source,followlinks=False,onerror=lambda error:notes.append(dict(path=str(error.filename),reason=str(error)))):
        dirs.sort();files.sort()
        for name in list(dirs):
            if (Path(parent)/name).is_symlink():
                dirs.remove(name);notes.append(dict(path=str(Path(parent)/name),reason='symlink directory skipped'))
        for name in files:
            path=Path(parent)/name;relative=path.relative_to(source)
            if path.is_symlink() or not path.is_file() or any(x in str(relative) for x in '\n\r\\'):
                notes.append(dict(path=str(path),reason='unsafe/non-regular file skipped'));continue
            try:
                if snapshot_file(path,destination/relative):notes.append(dict(path=str(path),reason='file changed during snapshot'))
            except (OSError,ValueError) as error:
                target=destination/relative
                if target.exists():target.unlink()
                notes.append(dict(path=str(path),reason=str(error)))
