"""Always-return collector for the combined GPU suite, without GPU execution.

Preserve shared results, native Q7 evidence, independent diagnostic files and
read-only GPU/CUDA environment. Malformed child JSON remains returnable.
Contract: doc/GPU-TEST-COMBINED-PACKAGE-v1.0.md.
"""
import argparse
import datetime
import os
from pathlib import Path
import tarfile
import tempfile
import sys
from gpu_test_suite_common import catalog,snapshot,write,ROOT,RETURN_SCHEMA,ROLES
from package_mla_q7_results import digest_file,capture_environment,snapshot_file


def collect(args):
    packet=args.packet.resolve();run=args.run.resolve();output=args.output.absolute();checksum=Path(str(output)+'.sha256')
    if run==packet or packet in run.parents:raise ValueError('run outputs must be outside the source packet')
    if output.exists() or output.is_symlink() or checksum.exists() or checksum.is_symlink():raise ValueError('return files already exist')
    if packet==output.resolve() or packet in output.resolve().parents:raise ValueError('return archive must be outside the source packet')
    if any(x in output.name for x in '\n\r\\'):raise ValueError('unsafe output filename')
    table=catalog(packet,args.expected_sha256);output.parent.mkdir(parents=True,exist_ok=True)
    notes=[]
    with tempfile.TemporaryDirectory(prefix='.gpu-suite-return-',dir=output.parent) as temporary:
        work=Path(temporary);bundle=work/ROOT[:-1];bundle.mkdir()
        for name in ('workflow','gemm','mla-core'):snapshot(run/name,bundle/('workflow' if name=='workflow' else 'results/'+name),notes)
        # Only this run's writable staging cache is eligible; never sweep a
        # shared installation's historical build cache or return executables.
        cache=run/'scaffold-work/.build';build_evidence=[]
        if cache.is_dir() and not cache.is_symlink():
            for directory in sorted(cache.iterdir()):
                if not directory.is_dir() or directory.is_symlink():continue
                for name in ('build.log','receipt.json'):
                    source=directory/name
                    if source.is_file() and not source.is_symlink():
                        destination=bundle/'build-evidence'/directory.name/name
                        if snapshot_file(source,destination):notes.append(dict(path=str(source),reason='build evidence changed during snapshot'))
                        build_evidence.append(destination.relative_to(bundle).as_posix())
        native=run/'mla-q7';inner=native/'Open64_MLA_Q7_results.tar.gz'
        if inner.is_file() and not inner.is_symlink() and Path(str(inner)+'.sha256').is_file():
            for source in (inner,Path(str(inner)+'.sha256')):
                if snapshot_file(source,bundle/'results/mla-q7'/source.name):notes.append(dict(path=str(source),reason='native Q7 archive changed during snapshot'))
        else:snapshot(native,bundle/'results/mla-q7/raw-partial',notes)
        environment=capture_environment(bundle/'environment',args.command_timeout)
        profiles={role:dict(kind='q7_return' if role=='mla-q7' else 'shared_json',
            path='results/mla-q7/Open64_MLA_Q7_results.tar.gz' if role=='mla-q7' else 'results/'+role+'/result.json') for role in ROLES}
        for entry in profiles.values():entry['present']=(bundle/entry['path']).is_file()
        write(bundle/'suite-results.json',dict(schema='open64.gpu-test.suite-results.v1',profiles=profiles,
            assessment='NOT_ASSESSED_BY_COLLECTOR',timings_pooled=False,profiling_separate=True))
        write(bundle/'bundle.json',dict(schema=RETURN_SCHEMA,packet_sha256=args.expected_sha256,packets=table['packets'],
            created_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),collection_notes=notes,
            packaging_status='PACKAGED_WITH_NOTES' if notes else 'PACKAGED',GPU_workloads_launched_by_collector=0,
            search_invocations=0,environment_commands=environment['commands'],scaffold_build_evidence=build_evidence))
        (bundle/'README.md').write_text('''# Combined GPU test return

This is collected evidence, not test acceptance. Three workloads retain their
own numerical contracts, timing scopes, ratios and independent profiling files.
Use validate_gpu_test_suite.py with the exact original combined source packet.
Native Q7 evidence is preserved in its authenticated inner tar. If native
collection failed, raw-partial files remain available and qualification cannot
be complete. Failed/partial reports are preserved, not rewritten as success.
''')
        manifest=''.join(digest_file(p)+'  '+p.relative_to(bundle).as_posix()+'\n' for p in sorted(bundle.rglob('*')) if p.is_file())
        (bundle/'SHA256SUMS').write_text(manifest)
        archive=work/'return.tar.gz'
        def normalize(info):
            info.uid=info.gid=0;info.uname=info.gname='';info.mode=0o755 if info.isdir() else 0o644
            return info
        with tarfile.open(archive,'w:gz') as tar:
            for path in sorted(bundle.rglob('*')):
                tar.add(path,arcname=ROOT+path.relative_to(bundle).as_posix(),recursive=False,filter=normalize)
        digest=digest_file(archive);sidecar=work/'checksum';sidecar.write_text(digest+'  '+output.name+'\n')
        os.link(archive,output);os.link(sidecar,checksum)
    return dict(archive=str(output),sha256=digest,collection_notes=len(notes),qualification='NOT_ASSESSED_BY_COLLECTOR')


def main(argv=None):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--packet',type=Path,required=True);parser.add_argument('--expected-sha256',required=True)
    parser.add_argument('--run',type=Path,required=True);parser.add_argument('--output',type=Path,required=True)
    parser.add_argument('--command-timeout',type=int,default=15)
    args=parser.parse_args(argv)
    try:
        if not 1<=args.command_timeout<=120:raise ValueError('invalid environment capture timeout')
        result=collect(args);print(result);return 0
    except (OSError,ValueError) as error:
        print('GPU suite collection failed: '+str(error),file=sys.stderr);return 1


if __name__=='__main__':raise SystemExit(main())
