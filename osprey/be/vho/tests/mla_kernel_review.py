"""Certify native MLA -> device WHIRL -> whirl2c CUDA kernels.

Design: doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md. Launch adapters
consume audited metadata from the mapped device image. All kernel arithmetic
comes from unmodified whirl2c output. Host simulation is separate from CUDA
execution and never counts as a GPU correctness result.
"""
import argparse
import difflib
import hashlib
import json
import math
from pathlib import Path
import re
import shutil
import struct
import subprocess


def digest(path):
    """Bind retained verification to actual file bytes."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def runner(manifest, device, reverse=False):
    """Generate allocation/launch/IO only, from audited device-image bindings."""
    storages = sorted(manifest["storages"], key=lambda x: int(x["storage"]))
    inputs = sorted((x for x in storages if x["role"] == "1"), key=lambda x: int(x["ordinal"]))
    outputs = sorted((x for x in storages if x["role"] == "2"), key=lambda x: int(x["ordinal"]))
    lines = ['#include "kernels.cu"', '#include <vector>', '#include <cstdio>',
             '#include <cstring>', '#include <cstdlib>']
    if device:
        lines += ['#define CK(call) do { cudaError_t e=(call); if(e!=cudaSuccess) { '
                  'fprintf(stderr,"CUDA error %d: %s\\n",int(e),cudaGetErrorString(e)); return 20;} } while(0)']
    lines += ['int main(int argc,char **argv) {']
    if device:
        lines += ['if(argc==2 && !strcmp(argv[1],"--probe")) { int n=0; cudaError_t e=cudaGetDeviceCount(&n); '
                  'printf("cuda_probe_error=%d devices=%d\\n",int(e),n); return e==cudaSuccess && n>0 ? 0 : 77; }',
                  'cudaDeviceProp prop; CK(cudaGetDeviceProperties(&prop,0));']
    lines += ['if(argc!=3) return 2; FILE *in=fopen(argv[1],"rb"); if(!in) return 3;']
    for s in storages:
        n, size = int(s["storage"]), int(s["bytes"])
        lines += ['std::vector<unsigned char> s%d(%d+32,0xcd);' % (n, size)]
        if device:
            lines += ['unsigned char *d%d; CK(cudaMalloc((void**)&d%d,%d+32)); CK(cudaMemset(d%d,0xcd,%d+32));'
                      % (n, n, size, n, size)]
    for s in inputs:
        n, size = int(s["storage"]), int(s["bytes"])
        lines += ['if(fread(s%d.data()+16,1,%d,in)!=%d) return 4; auto original%d=s%d;' % (n, size, size, n, n)]
        if device:
            lines += ['CK(cudaMemcpy(d%d+16,s%d.data()+16,%d,cudaMemcpyHostToDevice));' % (n, n, size)]
    lines += ['if(fgetc(in)!=EOF) return 5; fclose(in);']
    for k in manifest["kernels"]:
        assert re.fullmatch(r"tensor_[0-9]+_op_[0-9]+", k["name"])
        grid, block = int(k["grid_x"]), int(k["block_x"])
        args = []
        for a in k["arguments"]:
            ty = "unsigned char" if a["dtype"] == "bool" else "float"
            n = int(a["storage"])
            args.append('(%s*)(%s%d%s+16)' % (ty, 'd' if device else 's', n, '' if device else '.data()'))
        if device:
            lines += ['if(%d>prop.maxGridSize[0] || %d>prop.maxThreadsPerBlock) return 21;' % (grid, block),
                      'if(%d>prop.sharedMemPerBlock) return 22;' % int(k.get('shared_bytes','0')),
                      '%s<<<%d,%d>>>(%s); CK(cudaGetLastError());' % (k["name"], grid, block, ','.join(args))]
        elif int(k.get("mapping", "0")):
            lines += ['open64_host_launch(%d,%d,[&] { %s(%s); },%s);'
                      % (grid, block, k["name"], ','.join(args), 'true' if reverse else 'false')]
        elif reverse:
            lines += ['for(int bx=%d-1;bx>=0;--bx) for(int tx=%d-1;tx>=0;--tx) { blockIdx.x=bx; threadIdx.x=tx; %s(%s); }'
                      % (grid, block, k["name"], ','.join(args))]
        else:
            lines += ['for(blockIdx.x=0;blockIdx.x<%d;++blockIdx.x) for(threadIdx.x=0;threadIdx.x<%d;++threadIdx.x) %s(%s);'
                      % (grid, block, k["name"], ','.join(args))]
    if device:
        lines += ['CK(cudaDeviceSynchronize());']
        for s in storages:
            n, size = int(s["storage"]), int(s["bytes"])
            lines += ['CK(cudaMemcpy(s%d.data(),d%d,%d+32,cudaMemcpyDeviceToHost));' % (n, n, size)]
    for s in storages:
        n, size = int(s["storage"]), int(s["bytes"])
        lines += ['for(int j=0;j<16;++j) if(s%d[j]!=0xcd || s%d[%d+16+j]!=0xcd) return 30;' % (n, n, size)]
    for s in inputs:
        n, size = int(s["storage"]), int(s["bytes"])
        lines += ['if(memcmp(s%d.data(),original%d.data(),%d+32)) return 31;' % (n, n, size)]
    lines += ['FILE *out=fopen(argv[2],"wb"); if(!out) return 6;']
    for s in outputs:
        n, size = int(s["storage"]), int(s["bytes"])
        lines += ['if(fwrite(s%d.data()+16,1,%d,out)!=%d) return 7;' % (n, size, size)]
    if device:
        for s in storages:
            lines += ['CK(cudaFree(d%d));' % int(s["storage"])]
    lines += ['return fclose(out)!=0;', '}']
    return '\n'.join(lines) + '\n'


def review(args):
    """Check both context sizes and level boundaries with retained native images."""
    import torch
    from models.absorbed_mla import INPUT_NAMES, make_fixture
    from models.mla_reference import expanded_reference
    root = args.artifact_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    (root / "logs").mkdir()
    commands = []

    def run(label, argv, codes=(0,)):
        result = subprocess.run(list(map(str, argv)), capture_output=True, text=True)
        (root / "logs" / (label + ".stdout")).write_text(result.stdout)
        (root / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=result.returncode))
        (root / "commands.json").write_text(json.dumps(commands, indent=2) + '\n')
        if result.returncode not in codes:
            raise RuntimeError(label + ': ' + result.stderr[-5000:])
        return result

    reports = {}
    for context in ("short", "long"):
        source = root / context
        source.mkdir()
        for name in ("mla.B", "inputs.pt", "reference.pt"):
            shutil.copyfile(args.native_review / context / name, source / name)
        model, _ = make_fixture(context)
        saved = torch.load(source / "inputs.pt", map_location="cpu", weights_only=False)
        base = tuple(saved[name] for name in INPUT_NAMES)
        scenarios = {}
        variants = ["original", "one-masked-head", "all-masked"]
        if args.optimize:
            variants += ['tail-only', 'empty-first-chunk', 'equal-scores', 'large-scores']
        for variant in variants:
            inputs = tuple(x.clone() for x in base)
            if variant == "one-masked-head": inputs[-1][:, 0, :] = False
            if variant == "all-masked": inputs[-1].fill_(False)
            if variant == 'tail-only':
                inputs[-1].fill_(False)
                inputs[-1][..., -1] = True
            if variant == 'empty-first-chunk': inputs[-1][..., :min(args.split_width, inputs[-1].shape[-1] // 2)] = False
            if variant == 'equal-scores':
                inputs[0].zero_()
                inputs[1].zero_()
            if variant == 'large-scores':
                inputs[0].mul_(64.)
                inputs[1].mul_(64.)
            (source / (variant + '.inputs.bin')).write_bytes(b''.join(x.numpy().tobytes() for x in inputs))
            scenarios[variant] = expanded_reference(*(x.tolist() for x in inputs), model.alpha)
            groups = []
            for tensor in scenarios[variant]:
                flat = torch.tensor(tensor, dtype=torch.float64).flatten().tolist()
                groups.append([x if math.isfinite(x) else ('-inf' if x < 0 else '+inf') for x in flat])
            (source / (variant + '.reference.json')).write_text(json.dumps(dict(
                schema='open64.mla-output-oracle.v1', values=groups, rtol=2e-5, atol=2e-6), indent=2) + '\n')
        retained = torch.load(source / 'reference.pt', map_location='cpu', weights_only=False)
        for value, key in zip(scenarios['original'], ('output', 'lse')):
            torch.testing.assert_close(torch.tensor(value, dtype=torch.float64),
                torch.as_tensor(retained[key], dtype=torch.float64), rtol=0, atol=0)
        source_digest = digest(source / 'mla.B')
        run(context + '-source-before', [args.ir_b2a, '-st', '-src', source / 'mla.B', source / 'mla.T'])
        reports[context] = {}
        for level in args.levels:
            case = source / ('O' + str(level))
            case.mkdir()
            tag = context + '-O' + str(level)
            optimize = args.optimize and level == 3
            prepared = case / ('baseline.json' if optimize else 'recipe.json')
            run(tag + '-prepare', [args.lowerer, 'prepare', source / 'mla.B', prepared, '--verify', level])
            if optimize:
                options = ['--split-width', args.split_width, '--allow-reassociate'] if context == 'long' else []
                run(tag + '-optimize', [args.lowerer, 'optimize', prepared, case / 'recipe.json', '--fuse', *options])
            run(tag + '-device', [args.lowerer, 'device', case / 'recipe.json', case / 'device.before.B', case / 'device.after.B'])
            for boundary in ('before', 'after'):
                run(tag + '-' + boundary, [args.ir_b2a, '-st', '-src', case / ('device.' + boundary + '.B'), case / ('device.' + boundary + '.T')])
            before = (case / 'device.before.T').read_text()
            after = (case / 'device.after.T').read_text()
            (case / 'device.before-after.diff').write_text(''.join(difflib.unified_diff(
                before.splitlines(True), after.splitlines(True), fromfile='device.before.T', tofile='device.after.T')))
            if optimize:
                baseline = case / 'baseline'
                baseline.mkdir()
                run(tag + '-baseline-device', [args.lowerer, 'device', prepared, baseline / 'device.before.B', baseline / 'device.after.B'])
                for boundary in ('before', 'after'):
                    run(tag + '-baseline-' + boundary, [args.ir_b2a, '-st', '-src', baseline / ('device.' + boundary + '.B'), baseline / ('device.' + boundary + '.T')])
                run(tag + '-baseline-audit', [args.lowerer, 'audit', baseline / 'device.after.B', prepared, baseline / 'launch.json'])
                run(tag + '-baseline-cuda', [args.whirl2c, '-cuda', '-TARG:abi=n64', '-O0',
                    '-CLIST:dotc_file=' + str(baseline / 'kernels.cu'), '-CLIST:doth_file=' + str(baseline / 'kernels.h'), baseline / 'device.after.B'])
                assert len(json.loads((baseline / 'launch.json').read_text())['kernels']) == 34
                (case / 'baseline-to-selected.diff').write_text(''.join(difflib.unified_diff(
                    (baseline / 'device.after.T').read_text().splitlines(True), after.splitlines(True),
                    fromfile='baseline/device.after.T', tofile='device.after.T')))
            assert 'OPR_DSL' not in after and 'DO_LOOP' in after and 'ARRAY' in after
            run(tag + '-audit', [args.lowerer, 'audit', case / 'device.after.B', case / 'recipe.json', case / 'launch.json'])
            device_digest = digest(case / 'device.after.B')
            manifest = json.loads((case / 'launch.json').read_text())
            count = 12 if optimize and context == 'long' else 9 if optimize else 34
            assert len(manifest['kernels']) == count, len(manifest['kernels'])
            if optimize:
                partials = [k for k in manifest['kernels'] if k['split_stage'] == '1']
                merges = [k for k in manifest['kernels'] if k['split_stage'] == '2']
                assert len(partials) == len(merges) == (3 if context == 'long' else 0)
            assert all(int(x['block_x']) == (128 if level == 3 else 1) for x in manifest['kernels'])
            run(tag + '-cuda-source', [args.whirl2c, '-cuda', '-TARG:abi=n64', '-O0',
                '-CLIST:dotc_file=' + str(case / 'kernels.cu'), '-CLIST:doth_file=' + str(case / 'kernels.h'), case / 'device.after.B'])
            text = (case / 'kernels.cu').read_text()
            assert text.count('__global__ void') == count
            assert 'F4EXP(' not in text and '_F4ABS(' not in text and 'infF' not in text
            stub = case / 'host-include'
            stub.mkdir()
            (stub / 'cuda_runtime.h').write_text('#include <cmath>\n#include <cstring>\n#define __global__\n'
                'struct dim3 { unsigned int x=0,y=0,z=0; };\nstatic dim3 threadIdx,blockIdx;\n'
                'static float __uint_as_float(unsigned int x) { float f; memcpy(&f,&x,4); return f; }\n')
            (case / 'host_runner.cxx').write_text(runner(manifest, False))
            host_flags = ['-fsanitize=address,undefined', '-fno-omit-frame-pointer'] if args.sanitize else []
            run(tag + '-host-compile', [args.cxx, '-std=c++17', '-O1', '-ffp-contract=off', '-fno-fast-math',
                *host_flags, '-I' + str(stub), case / 'host_runner.cxx', '-o', case / 'host_runner'])
            if optimize:
                (case / 'reverse_runner.cxx').write_text(runner(manifest, False, reverse=True))
                run(tag + '-reverse-compile', [args.cxx, '-std=c++17', '-O1', '-ffp-contract=off', '-fno-fast-math',
                    *host_flags, '-I' + str(stub), case / 'reverse_runner.cxx', '-o', case / 'reverse_runner'])
            gpu = False
            cuda = 'NOT_RUN'
            if args.nvcc:
                flags = ['-std=c++17', '-arch=sm_90', '--fmad=false', '--ftz=false', '--prec-div=true', '--prec-sqrt=true']
                run(tag + '-cubin', [args.nvcc, *flags, '-cubin', '-Xptxas=-v', case / 'kernels.cu', '-o', case / 'kernels.cubin'])
                (case / 'gpu_runner.cu').write_text(runner(manifest, True))
                run(tag + '-gpu-compile', [args.nvcc, *flags, '--cudart=shared', '-L' + str(args.nvcc.parent.parent / 'lib'),
                    '-Xlinker', '-rpath', '-Xlinker', str(args.nvcc.parent.parent / 'lib'), case / 'gpu_runner.cu', '-o', case / 'gpu_runner'])
                probe = run(tag + '-gpu-probe', [case / 'gpu_runner', '--probe'], codes=(0, 77))
                gpu = probe.returncode == 0 and args.gpu_run
                cuda = 'PASS'
            checks = {}
            for variant, expected in scenarios.items():
                checks[variant] = {}
                for lane in (['host', 'gpu'] if gpu else ['host']):
                    output = case / (variant + '.' + lane + '.bin')
                    run(tag + '-' + lane + '-' + variant, [case / (lane + '_runner'), source / (variant + '.inputs.bin'), output])
                    raw = output.read_bytes()
                    errors = []
                    offset = 0
                    for reference in expected:
                        ref = torch.tensor(reference, dtype=torch.float64)
                        n = ref.numel()
                        values = struct.unpack('<' + str(n) + 'f', raw[offset:offset + n * 4])
                        offset += n * 4
                        got = torch.tensor(values, dtype=torch.float64).reshape(ref.shape)
                        torch.testing.assert_close(got, ref, rtol=2e-5, atol=2e-6)
                        finite = torch.isfinite(ref)
                        errors.append(float((got[finite] - ref[finite]).abs().max()) if finite.any() else 0.)
                    assert offset == len(raw)
                    checks[variant][lane] = dict(status='PASS', max_abs_output=errors[0], max_abs_lse=errors[1],
                        buffer_canaries='PASS', inputs_unchanged='PASS')
                if optimize:
                    reverse = case / (variant + '.reverse.bin')
                    run(tag + '-reverse-' + variant, [case / 'reverse_runner', source / (variant + '.inputs.bin'), reverse])
                    assert reverse.read_bytes() == (case / (variant + '.host.bin')).read_bytes()
                    checks[variant]['thread_order_invariance'] = 'PASS'
            # Both altered executable structure and stale binding must refuse.
            for mode in ('audit-mutate', 'audit-binding', 'audit-prototype', 'audit-policy'):
                rejected = case / (mode + '.json')
                run(tag + '-' + mode, [args.lowerer, mode, case / 'device.after.B', case / 'recipe.json', rejected], codes=(1,))
                assert not rejected.exists()
            assert digest(case / 'device.after.B') == device_digest
            reports[context]['O' + str(level)] = dict(kernels=count,
                temporary_bytes=sum(int(s['bytes']) for s in manifest['storages'] if s['role'] == '0'),
                device_image_sha256=digest(case / 'device.after.B'), cuda_cubin_and_runner=cuda,
                gpu_execution='PASS' if gpu else 'NOT_RUN', host_simulation=checks,
                source_unchanged=digest(source / 'mla.B') == source_digest)
        shutil.copyfile(source / 'mla.B', source / 'mla.after.B')
        run(context + '-source-after', [args.ir_b2a, '-st', '-src', source / 'mla.after.B', source / 'mla.after.T'])
        assert (source / 'mla.T').read_bytes() == (source / 'mla.after.T').read_bytes()
    if args.nvcc and 3 in args.levels:
        (root / 'combined.cu').write_text('#include "short/O3/kernels.cu"\n#include "long/O3/kernels.cu"\n')
        run('short-long-aot-coexistence', [args.nvcc, '-std=c++17', '-arch=sm_90', '--fmad=false', '--ftz=false',
            '--prec-div=true', '--prec-sqrt=true', '-cubin', root / 'combined.cu', '-o', root / 'combined.cubin'])
    report = dict(status='PASS', cases=reports, aot_coexistence='PASS' if args.nvcc and 3 in args.levels else 'NOT_RUN', tool_sha256={
        'lowerer': digest(args.lowerer), 'whirl2c': digest(args.whirl2c),
        'whirl2c.so': digest(args.whirl2c.parent / 'whirl2c.so'), 'ir_b2a': digest(args.ir_b2a)})
    (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (root / 'SHA256SUMS').write_text(''.join(digest(p) + '  ' + str(p.relative_to(root)) + '\n'
        for p in sorted(root.rglob('*')) if p.is_file()))
    print(json.dumps(report, indent=2))


def main():
    """Require fresh artifact and explicit tool paths; GPU results stay opt-in."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('artifact-dir', 'native-review', 'lowerer', 'whirl2c', 'ir-b2a'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--nvcc', type=Path)
    parser.add_argument('--levels', type=int, nargs='+', choices=(0, 1, 2, 3), default=(0, 3))
    parser.add_argument('--cxx', default='g++')
    parser.add_argument('--sanitize', action='store_true')
    parser.add_argument('--gpu-run', action='store_true')
    parser.add_argument('--optimize', action='store_true', help='Fuse O3 producers; split long reductions with explicit permission')
    parser.add_argument('--split-width', type=int, default=256)
    review(parser.parse_args())


if __name__ == '__main__':
    main()
