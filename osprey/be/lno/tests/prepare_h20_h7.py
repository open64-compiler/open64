"""Compose H7 from authenticated H4/H5/H6 selected artifacts and H3 goldens.

Preparation copies existing native/CUDA/COMMENT products and creates server-owned
input/oracle/launch metadata. It invokes no compiler, search or GPU. Existing
combined inner packets/scripts are preserved. See doc/AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys
import tarfile
from h20_h7_common import ROLES, write, sha
from gpu_test_suite_common import catalog


def seal(root):
    """Create the normal immutable source packet identity, excluding its own ID."""
    # Inner manifests ARE covered by outer packet seals.
    text = ''.join(sha(p) + '  ' + p.relative_to(root).as_posix() + '\n' for p in sorted(root.rglob('*'))
                   if p.is_file() and p not in (root / 'SHA256SUMS', root / 'PACKET_ID'))
    (root / 'SHA256SUMS').write_text(text)
    identity = hashlib.sha256(text.encode()).hexdigest(); (root / 'PACKET_ID').write_text(identity + '\n')
    return identity


class RECEIPT:
    """Authenticate every reused input against its retained manifest on demand."""
    def __init__(self, path):
        """Pin an immutable upstream receipt and its producer reports before reuse."""
        self.root = path.resolve()
        self.files = dict((n, d) for d, n in (line.split('  ', 1) for line in (path / 'SHA256SUMS').read_text().splitlines()))
        self.read('receipt.json'); self.read('report.json')

    def read(self, name):
        """No source file may silently substitute another receipt's artifact."""
        path = self.root / name
        if path.is_symlink() or sha(path) != self.files[name]:
            raise ValueError('receipt artifact hash differs: ' + name)
        return path.read_bytes()

    def copy(self, name, target):
        """Copy authenticated bytes without changing the original receipt."""
        target.parent.mkdir(parents=True, exist_ok=True); target.write_bytes(self.read(name))


def main(argv=None):
    """Build one explicitly selected device packet and a combined deployment tar."""
    p = argparse.ArgumentParser(description=__doc__)
    for name in ('h3', 'h4', 'h5', 'h6', 'base', 'output'):
        p.add_argument('--' + name, type=Path, required=True)
    p.add_argument('--gpu-device', choices=('h20_96gb', 'h20_141gb'), required=True)
    args = p.parse_args(argv)
    base = args.base.resolve(); table = catalog(base, (base / 'PACKET_ID').read_text().strip())
    out = args.output.resolve(); out.mkdir(parents=True, exist_ok=False)
    root = out / ('Open64_H20_GPU_Test_Suite_' + args.gpu_device + '_v1.0')
    shutil.copytree(base, root)
    tools = root / 'tools'; scripts = Path(__file__).resolve().parent
    # Preserve historical outer tools as well as every original inner script.
    shutil.copytree(tools, root / 'prior-tools')
    for source in scripts.glob('*.py'):
        shutil.copy2(source, tools / source.name)
    for source in ('run_h20_h7.py', 'h20_h7_common.py'):
        if not (tools / source).is_file(): raise ValueError('H7 tool missing: ' + source)
    packet = root / 'h20'; packet.mkdir()
    receipts = {key: RECEIPT(getattr(args, key)) for key in ('h3', 'h4', 'h5', 'h6')}
    target = args.gpu_device; profiles = {role: [] for role in ROLES}
    opt = scripts.parents[1] / 'opt/tests'; sys.path.insert(0, str(opt))
    from normalization_guard_trace_report import decode
    from normalization_oracle import reference_row, add

    def artifact(receipt, path, folder, name=None):
        """Copy source/native provenance without synthesizing candidate arithmetic."""
        destination = folder / (name or Path(path).name)
        receipt.copy(path, destination); return destination.relative_to(packet).as_posix()

    def data(folder, name, raw):
        """Write fixed server input/golden bytes under the authenticated corpus."""
        path = folder / name; path.write_bytes(raw); return path.relative_to(packet).as_posix()

    def u32(values):
        """Keep all exceptional binary32 words and signed zeros exact."""
        return struct.pack('<%dI' % len(values), *values)

    for family, indices in [('layernorm', (0, 2, 3)), ('reduction', (4, 5, 6, 7))]:
        for index in indices:
            role = 'h20-' + family; old = 'complete-native/' + target + ('-ln-' if family == 'layernorm' else '-reduce-') + str(index)
            receipt = receipts['h6']; folder = packet / (family + '-' + str(index)); folder.mkdir()
            phase = receipt.read(old + '/selected.phase.t').decode()
            chosen = dict(x.split('=', 1) for x in next(s for s in phase.splitlines() if s.startswith('AI_H6_KERNEL ')).split()[2:])
            rows, width, threads, kind = [int(chosen[n]) for n in ('rows', 'width', 'threads', 'kind')]
            case = dict(id=folder.name, gpu_device=target, source=artifact(receipt, old + '/selected.cu', folder, 'candidate.cu'),
                native_trace=artifact(receipt, old + '/selected.device.after.T', folder),
                buffers=[], kernels=[], variants=[], numerical=dict(kind='f32-bits-nan-class'), compiler_profile='synthetic',
                registers_estimate=32, useful_flops=None, baseline=None)
            artifact(receipt, old + '/selected.device.after.B', folder); artifact(receipt, old + '/selected.source.before.B', folder)
            sizes = [rows * width * 4, width * 4, width * 4, rows * width * 4] if family == 'layernorm' else [rows * width * 4, rows * 4]
            case['buffers'] = [dict(id=str(i), bytes=n, role='output' if i == len(sizes)-1 else 'input') for i, n in enumerate(sizes)]
            shared = decode(receipt.read(old + '/selected.guard'))['reserved_shared'] if family == 'layernorm' else int(chosen['chunk']) * 4
            case['kernels'] = [dict(name='h6_layernorm' if family == 'layernorm' else 'h6_reduction',
                grid=[rows if family == 'layernorm' or kind == 2 else (rows + threads-1)//threads, 1, 1], threads=threads,
                shared_bytes=shared, arguments=[b['id'] for b in case['buffers']])]
            lines = receipt.read(old + '/inputs.txt').decode().splitlines()
            if int(lines[0]) != len(lines)-1: raise ValueError('retained corpus row count differs')
            guard = decode(receipt.read(old + '/selected.guard')) if family == 'layernorm' else None
            for v, line in enumerate(lines[1:]):
                words = [int(x, 16) for x in line.split()]; inputs = {}; cursor = 0
                for b in case['buffers'][:-1]:
                    count = b['bytes']//4; inputs[b['id']] = data(folder, 'v%d-in%s.bin' % (v,b['id']), u32(words[cursor:cursor+count])); cursor += count
                if cursor != len(words): raise ValueError('retained corpus input extent differs')
                expected = []
                for row in range(rows):
                    if family == 'layernorm':
                        expected += reference_row(words[row*width:(row+1)*width], words[rows*width:rows*width+width], words[rows*width+width:], guard['epsilon_bits'])['y']
                    else:
                        total = 0
                        for word in words[row*width:(row+1)*width]: total = add(total,word)
                        expected.append(total)
                case['variants'].append(dict(id='v'+str(v), inputs=inputs, outputs={str(len(sizes)-1):data(folder,'v%d-golden.bin'%v,u32(expected))}))
            profiles[role].append(case)
    for context in ('short', 'long'):
        receipt = receipts['h5']; old='final-native-accepted/'+target+'-'+context+'/device/pu-0'; folder=packet/('mla-'+context);folder.mkdir()
        launch=json.loads(receipt.read(old+'/selected.launch.json')); artifact(receipt,old+'/selected.launch.json',folder)
        case=dict(id=folder.name,gpu_device=target,source=artifact(receipt,'final-native-accepted/'+target+'-'+context+'/kernels.cu',folder,'candidate.cu'),
            native_trace=artifact(receipt,old+'/selected.low.T',folder),
            buffers=[dict(id=s['storage'],bytes=int(s['bytes']),role={'0':'scratch','1':'input','2':'output'}[s['role']]) for s in launch['storages']],
            kernels=[dict(name=k['name'],grid=[int(k['grid_x']),1,1],threads=int(k['block_x']),shared_bytes=int(k['shared_bytes']),arguments=[a['storage'] for a in k['arguments']]) for k in launch['kernels']],
            variants=[],numerical=dict(kind='f32-vs-f64',rtol=2e-5,atol=2e-6),compiler_profile='synthetic',registers_estimate=None,useful_flops=None,baseline=None)
        artifact(receipt,old+'/selected.low.B',folder)
        ordered=sorted(launch['storages'],key=lambda s:int(s['ordinal']))
        for variant in ('original','one-masked-head','all-masked','tail-only','empty-first-chunk','equal-scores','large-scores'):
            original='inputs/MR2/'+context+'/'+variant;payload=receipts['h3'].read(original+'.inputs.bin');reference=json.loads(receipts['h3'].read(original+'.reference.json'))
            if reference['rtol']!=case['numerical']['rtol'] or reference['atol']!=case['numerical']['atol']:raise ValueError('MLA numerical rules differ')
            inputs={};outputs={};cursor=0
            for storage in ordered:
                if storage['role']=='1':
                    n=int(storage['bytes']);inputs[storage['storage']]=data(folder,variant+'-input-'+storage['storage']+'.bin',payload[cursor:cursor+n]);cursor+=n
                if storage['role']=='2':
                    values=reference['values'][int(storage['ordinal'])];outputs[storage['storage']]=data(folder,variant+'-golden-'+storage['storage']+'.bin',struct.pack('<%dd'%len(values),*[float(v) for v in values]))
            if cursor!=len(payload):raise ValueError('MLA input extent differs from selected native launch')
            case['variants'].append(dict(id=variant,inputs=inputs,outputs=outputs))
        profiles['h20-mla'].append(case)
    receipt=receipts['h4']; commands=json.loads(receipt.read('native-final-acceptance/commands.json'))
    for label in ('aligned-sync','ragged'):
        old='native-final-acceptance/'+target+'-'+label;folder=packet/('gemm-'+label);folder.mkdir()
        command=next(c for c in commands if c['label']=='host-link' and c['cwd'].endswith('/'+target+'-'+label))
        params={k:int(v) for arg in command['argv'] if arg.startswith('-DAI_TEST_') for k,v in [arg[len('-DAI_TEST_'):].split('=',1)]}
        m,n,k=[params[x] for x in ('M','N','K')]; aa=[(i*13)%17-8 for i in range(m*k)];bb=[(i*7)%19-9 for i in range(k*n)]
        golden=[sum(aa[r*k+j]*bb[j*n+c] for j in range(k))/64 for r in range(m) for c in range(n)]
        f32=lambda values:struct.pack('<%df'%len(values),*values)
        case=dict(id=folder.name,gpu_device=target,source=artifact(receipt,old+'/selected.cu',folder,'candidate.cu'),
            native_trace=artifact(receipt,old+'/selected.T',folder),buffers=[dict(id='0',bytes=m*k*4,role='input'),dict(id='1',bytes=k*n*4,role='input'),dict(id='2',bytes=m*n*4,role='output')],
            kernels=[dict(name='ai_matmul_tile_f32',grid=[(n+params['BN']-1)//params['BN'],(m+params['BM']-1)//params['BM'],1],threads=params['THREADS'],shared_bytes=(params['BM']*params['BK']+params['BK']*params['BN'])*4,arguments=['0','1','2'])],
            variants=[dict(id='binary-exact',inputs={'0':data(folder,'a.bin',f32([x/8 for x in aa])),'1':data(folder,'b.bin',f32([x/8 for x in bb]))},outputs={'2':data(folder,'golden.bin',f32(golden))})],
            numerical=dict(kind='f32-bits-nan-class'),compiler_profile='synthetic',registers_estimate=None,useful_flops=2*m*n*k,baseline=None)
        artifact(receipt,old+'/selected.B',folder);artifact(receipt,old+'/phase.t',folder);profiles['h20-gemm'].append(case)
    profile_path='compiler-device-profile.json'
    receipts['h3'].copy('inputs/H1c/'+target+'-base.json',packet/profile_path)
    for role,cases in profiles.items():
        owner='h4' if role=='h20-gemm' else 'h3'
        model_path='compiler-'+owner+'-model.json'
        original_model='native-final-acceptance/'+target+'.model.json' if owner=='h4' else 'native-corrected/'+target+'.h3.json'
        receipts[owner].copy(original_model,packet/model_path)
        for case in cases:
            case['compiler_binding']=dict(gpu_device=target,profile=profile_path,profile_sha256=sha(packet/profile_path),
                model=model_path,model_sha256=sha(packet/model_path),qualification='synthetic-inputs-not-physical-calibration')
            if role in ('h20-layernorm','h20-reduction'):
                index=case['id'].rsplit('-',1)[1];old='complete-native/'+target+('-ln-' if role=='h20-layernorm' else '-reduce-')+index
                artifact(receipts['h6'],old+'/selected.phase.t',packet/case['id'])
                artifact(receipts['h6'],old+'/selected.source.before.T',packet/case['id'])
    metadata=dict(schema='open64.h20-h7.packet.v1',gpu_device=target,profiles=profiles,search_invocations=0,
        upstream={k:dict(path=str(r.root),manifest_sha256=sha(r.root/'SHA256SUMS')) for k,r in receipts.items()},
        limitations=['H4 TMA launch provider not included; synchronous H4 candidates only', 'No matched native GPU baselines; ratios remain null', 'Compiler H3 profiles are synthetic; timing is observed, model calibration remains unqualified'])
    write(packet/'h20.json',metadata);h20_id=seal(packet)
    table['producer_commit']=subprocess.check_output(['git','rev-parse','HEAD'],cwd=scripts.parents[3],text=True).strip()
    table['h20_target']=target
    table['schema']='open64.gpu-test.suite.v2';table['packets']['h20']=dict(directory='h20',packet_sha256=h20_id)
    write(root/'suite.json',table)
    (root/'H20_README.md').write_text('H7 H20 fixed-artifact qualification. Run bash RUN_COMMANDS.sh, optionally --profiling auto. Target '+target+'. Read h20/h20.json for case/limitations. Correctness, four sanitizer tools and resource admission precede separate wall/event batches. No search/model changes. Unknown baselines/calibration stay unknown. Prior inner scripts and numeric contracts remain unchanged.\n')
    design=scripts.parents[3]/'doc/AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md'
    shutil.copyfile(design,root/'H7_QUALIFICATION.md')
    for name in ('README.md','RELEASE_NOTES.md'):
        path=root/name;older=path.read_text() if path.exists() else ''
        path.write_text('# H20 H7 extension: '+target+'\n\nRun `bash RUN_COMMANDS.sh`, optionally `--profiling auto`. Read [H7 qualification](H7_QUALIFICATION.md). Eleven selected native CUDA cases add four independent F32 profiles to the unchanged original suites. Wrong/unknown H20 identity or resource evidence denies H7. Missing GPU/tools, native baselines and synthetic-model calibration remain explicit INCOMPLETE gates, even if code correctness passes. No search, tuning, model update or serving/AOT selection runs here. H4 TMA-provider qualification is separate; original Q7 TMA/Q3 checks remain.\n\n## Preserved earlier release documentation\n\n'+older)
    identity=seal(root); archive=out/(root.name+'.tar.gz')
    with tarfile.open(archive,'w:gz') as tar:tar.add(root,arcname=root.name)
    Path(str(archive)+'.sha256').write_text(sha(archive)+'  '+archive.name+'\n')
    write(out/'preparation.json',dict(packet=str(root),packet_sha256=identity,h20_sha256=h20_id,archive=str(archive),archive_sha256=sha(archive),case_counts={k:len(v) for k,v in profiles.items()},search_invocations=0,GPU=False))
    print(archive);return 0


if __name__=='__main__':raise SystemExit(main())
