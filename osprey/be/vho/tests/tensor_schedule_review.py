"""Validate executable, readable CUDA views of the existing selected MLA plans.

Design: doc/AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.1.md. This harness consumes
saved inputs/oracles and the actual audited low-level products. It selects no
optimization and does not claim GPU execution from host or NVCC checks.
"""
import argparse
import ctypes
import difflib
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess

from validate_mla_gpu import check_output
from mla_kernel_review import runner


def explanation_blocks(text):
    """Compare COMMENT payloads across view and normal whirl2c formatting."""
    return ['\n'.join(line.strip() for line in block.splitlines()).strip()
            for block in re.findall(r'/\*\s*(OPEN64 GENERATED EXPLANATION:[\s\S]*?)\*/', text)]


def without_generated_explanations(text):
    """Recover the former readable file exactly by removing only new blocks."""
    return re.sub(r'(?m)^/\* OPEN64 GENERATED EXPLANATION:[\s\S]*? \*/\n', '', text)


def executable_tokens(text):
    """Ignore comments/spacing while retaining quoted strings and C++ tokens."""
    pattern = r'//[^\n]*|/\*[\s\S]*?\*/|"(?:\\.|[^"\\])*"|\x27(?:\\.|[^\x27\\])*\x27'
    text = re.sub(pattern, lambda m: ' ' if m.group().startswith(('//','/*')) else m.group(), text)
    return re.findall(r'"(?:\\.|[^"\\])*"|\x27(?:\\.|[^\x27\\])*\x27|[A-Za-z_][A-Za-z0-9_]*|[0-9]+|[^\s]', text)


def digest(path):
    """Identify immutable source, policy, and generated products by their bytes."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def execute_source(library, function, recipe, raw):
    """Invoke a compiled source view with packed inputs and guarded ABI buffers."""
    values = [v for role in (1, 2) for v in recipe['values'] if v['role'] == role]
    buffers, originals, offset = [], [], 0
    for value in values:
        size = 1 if value['dtype'] == 'bool' else 4
        for extent in value['shape']:
            size *= extent
        buf = ctypes.create_string_buffer(b'\xcd' * (size + 32), size + 32)
        if value['role'] == 1:
            if offset + size > len(raw):
                raise ValueError('truncated packed input')
            ctypes.memmove(ctypes.addressof(buf) + 16, raw[offset:offset + size], size)
            offset += size
        buffers.append(buf)
        originals.append(buf.raw)
    if offset != len(raw):
        raise ValueError('extra packed input')
    handle = ctypes.CDLL(str(library))
    entry = getattr(handle, function)
    entry.restype = None
    entry.argtypes = [ctypes.c_void_p] * len(values)
    entry(*(ctypes.addressof(buf) + 16 for buf in buffers))
    output = []
    for value, buf, original in zip(values, buffers, originals):
        if buf.raw[:16] != original[:16] or buf.raw[-16:] != original[-16:]:
            raise ValueError('source-view buffer guard changed')
        if value['role'] == 1 and buf.raw != original:
            raise ValueError('source-view input changed')
        if value['role'] == 2:
            output.append(buf.raw[16:-16])
    return b''.join(output)


def review(args):
    """Compare both selected views to FP64 oracles and the actual lowered path."""
    root = args.artifact_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    (root / 'logs').mkdir()
    for name in ('open64_tensor_reference.cuh', 'open64_tensor_schedule.cuh'):
        shutil.copyfile(args.header_dir / name, root / name)
    commands = []

    def run(label, argv, codes=(0,)):
        result = subprocess.run(list(map(str, argv)), capture_output=True, text=True, timeout=600)
        (root / 'logs' / (label + '.stdout')).write_text(result.stdout)
        (root / 'logs' / (label + '.stderr')).write_text(result.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=result.returncode))
        (root / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if result.returncode not in codes:
            raise RuntimeError(label + ': ' + result.stderr[-4000:])
        return result

    cases = {}
    for context in ('short', 'long'):
        source = args.selected_review / context
        case = root / context
        case.mkdir()
        for name in ('recipe.json', 'launch.json', 'device.after.B', 'device.after.T'):
            shutil.copyfile(source / 'O3' / name, case / name)
        if args.cooperative or args.refresh_explanations or args.fuse_attention:
            assert sum(map(bool,(args.cooperative,args.refresh_explanations,args.fuse_attention)))==1
            assert args.ir_b2a and args.whirl2c
            for name in ('recipe.json','device.after.B','device.after.T'):
                (case / name).rename(case / ('baseline.' + name))
            if args.fuse_attention:
                run(context + '-fuse-attention',[args.lowerer,'fuse-attention',case / 'baseline.recipe.json',
                    case / 'recipe.json','--allow-reassociate'])
            elif args.cooperative:
                run(context + '-cooperate', [args.lowerer,'cooperate',case / 'baseline.recipe.json',
                    case / 'recipe.json','--allow-reassociate'])
            else:
                shutil.copyfile(case / 'baseline.recipe.json',case / 'recipe.json')
            run(context + '-cooperative-device', [args.lowerer,'device',case / 'recipe.json',
                case / 'device.before.B',case / 'device.after.B'])
            for boundary in ('before','after'):
                run(context + '-cooperative-' + boundary,[args.ir_b2a,'-st','-src',
                    case / ('device.' + boundary + '.B'),case / ('device.' + boundary + '.T')])
            for a,b,name in [('device.before.T','device.after.T','device.before-after.diff'),
                             ('baseline.device.after.T','device.after.T','baseline-to-fused-attention.diff' if args.fuse_attention else 'baseline-to-cooperative.diff')]:
                (case / name).write_text(''.join(difflib.unified_diff(
                    (case / a).read_text().splitlines(True),(case / b).read_text().splitlines(True),fromfile=a,tofile=b)))
        recipe = json.loads((case / 'recipe.json').read_text())
        before = digest(case / 'recipe.json')
        run(context + '-mapped-audit', [args.lowerer, 'audit', case / 'device.after.B',
            case / 'recipe.json', case / 'launch.reopened.json'])
        if args.cooperative or args.fuse_attention: shutil.copyfile(case / 'launch.reopened.json',case / 'launch.json')
        launch = json.loads((case / 'launch.json').read_text())
        assert launch == json.loads((case / 'launch.reopened.json').read_text())
        cuda = case / ('mla_' + context + '_optimized.cu')
        run(context + '-source', [args.lowerer, 'schedule-source', case / 'recipe.json', cuda])
        run(context + '-source-again', [args.lowerer, 'schedule-source', case / 'recipe.json', case / 'deterministic.cu'])
        assert cuda.read_bytes() == (case / 'deterministic.cu').read_bytes()
        run(context + '-no-overwrite', [args.lowerer, 'schedule-source', case / 'recipe.json', cuda], (1,))
        assert cuda.read_bytes() == (case / 'deterministic.cu').read_bytes()
        assert before == digest(case / 'recipe.json')
        text = cuda.read_text()
        blocks = explanation_blocks(text)
        assert blocks and any('FUSED PRODUCER' in b for b in blocks)
        if args.previous_readable:
            prior = args.previous_readable / ('mla_' + context + '_optimized.cu')
            assert without_generated_explanations(text) == without_generated_explanations(prior.read_text())
        stages = list(map(int, re.findall(r'kernel_id=(\d+);', text)))
        assert stages == [int(k['operation']) for k in launch['kernels']]
        assert 'threadIdx' not in text and 'blockIdx' not in text and '__global__' not in text
        function = re.search(r'void (open64_optimized_[A-Za-z0-9_]+)\(', text).group(1)
        lib = case / 'optimized.so'
        run(context + '-host-compile', [args.cxx, '-x', 'c++', '-std=c++17', '-O1', '-fPIC', '-shared',
            '-ffp-contract=off', '-fno-fast-math', '-I' + str(root), cuda, '-o', lib])
        mapped = None
        gpu = False
        if args.whirl2c:
            assert args.ir_b2a
            run(context + '-comments-produce', [args.lowerer, 'device-explain', case / 'recipe.json',
                case / 'comments.before.B', case / 'comments.after.B'])
            for boundary in ('before','after'):
                run(context + '-comments-' + boundary, [args.ir_b2a, '-st', '-src', case / ('comments.' + boundary + '.B'), case / ('comments.' + boundary + '.T')])
            (case / 'comments.before-after.diff').write_text(''.join(difflib.unified_diff(
                (case / 'comments.before.T').read_text().splitlines(True),
                (case / 'comments.after.T').read_text().splitlines(True),
                fromfile='comments.before.T',tofile='comments.after.T')))
            run(context + '-comments-audit', [args.lowerer, 'audit', case / 'comments.after.B', case / 'recipe.json', case / 'comments.launch.json'])
            annotated_launch = json.loads((case / 'comments.launch.json').read_text())
            assert annotated_launch == launch
            mapped = case / 'mapped'; mapped.mkdir()
            for boundary, output in [('before',case / 'mapped.before.cu'),('after',mapped / 'kernels.cu')]:
                run(context + '-mapped-' + boundary, [args.whirl2c, '-cuda', '-TARG:abi=n64', '-O0',
                    '-CLIST:dotc_file=' + str(output), '-CLIST:doth_file=' + str(output.with_suffix('.h')),
                    case / ('comments.' + boundary + '.B')])
            emitted = (mapped / 'kernels.cu').read_text()
            assert explanation_blocks(emitted) == blocks
            if args.previous_mapped:
                previous = args.previous_mapped / ('mla_' + context + '_mapped.cu')
                assert executable_tokens(emitted) == executable_tokens(previous.read_text())
            assert executable_tokens(emitted) == executable_tokens((case / 'mapped.before.cu').read_text())
            assert len(re.findall(r'^\s*COMMENT\b',(case / 'comments.after.T').read_text(),re.M)) == len(blocks)
            for mode in ('audit-comment','audit-comment-missing'):
                rejected = case / (mode + '.json')
                run(context + '-' + mode, [args.lowerer, mode, case / 'comments.after.B', case / 'recipe.json', rejected], (1,))
                assert not rejected.exists()
            stub = mapped / 'include'; stub.mkdir()
            (stub / 'cuda_runtime.h').write_text('#include <cmath>\n#include <cstring>\n#define __global__\n'
                'struct dim3 {unsigned x=0,y=0,z=0;}; static dim3 threadIdx,blockIdx;\n'
                'static float __uint_as_float(unsigned x) {float f; memcpy(&f,&x,4); return f;}\n')
            if recipe.get('cooperative',False):
                shutil.copyfile(Path(__file__).parent / 'tensor_collective_stub/cuda_runtime.h',stub / 'cuda_runtime.h')
            (mapped / 'runner.cxx').write_text(runner(annotated_launch,False))
            run(context + '-mapped-host-compile', [args.cxx, '-std=c++17','-O1','-pthread','-ffp-contract=off','-fno-fast-math',
                '-I' + str(stub),mapped / 'runner.cxx','-o',mapped / 'runner'])
        if args.nvcc:
            run(context + '-cuda-compile', [args.nvcc, '-std=c++17', '-arch=sm_90', '--fmad=false',
                '--ftz=false', '--prec-div=true', '--prec-sqrt=true', '-dc', '-I' + str(root), cuda,
                '-o', case / 'optimized.cuda.o'])
            if mapped:
                run(context + '-mapped-cubin', [args.nvcc,'-std=c++17','-arch=sm_90','--fmad=false','--ftz=false',
                    '--prec-div=true','--prec-sqrt=true','-Xptxas=-v','-cubin',mapped / 'kernels.cu','-o',mapped / 'kernels.cubin'])
                (mapped / 'gpu_runner.cu').write_text(runner(annotated_launch,True))
                run(context + '-gpu-link',[args.nvcc,'-std=c++17','-arch=sm_90','--fmad=false','--ftz=false',
                    '--prec-div=true','--prec-sqrt=true','--cudart=shared','-L' + str(args.nvcc.parent.parent / 'lib'),
                    '-Xlinker','-rpath','-Xlinker',str(args.nvcc.parent.parent / 'lib'),mapped / 'gpu_runner.cu','-o',mapped / 'gpu_runner'])
                gpu = run(context + '-gpu-probe',[mapped / 'gpu_runner','--probe'],(0,77)).returncode == 0
        checks = {}
        for oracle in sorted(source.glob('*.reference.json')):
            variant = oracle.name[:-len('.reference.json')]
            for suffix in ('.reference.json', '.inputs.bin'):
                shutil.copyfile(source / (variant + suffix), case / (variant + suffix))
            raw = execute_source(lib, function, recipe, (case / (variant + '.inputs.bin')).read_bytes())
            output = case / (variant + '.outputs.bin')
            output.write_bytes(raw)
            check = check_output(case / (variant + '.reference.json'), output)
            lowered = source / 'O3' / (variant + '.host.bin')
            if not (args.cooperative or args.fuse_attention):
                assert raw == lowered.read_bytes(), context + ': source/lowered difference: ' + variant
            if mapped:
                mapped_output = mapped / (variant + '.outputs.bin')
                run(context + '-mapped-' + variant,[mapped / 'runner',case / (variant + '.inputs.bin'),mapped_output])
                assert raw == mapped_output.read_bytes()
                check['mapped_host_reference'] = check_output(case / (variant + '.reference.json'),mapped_output)
                if gpu:
                    gpu_output=mapped / (variant + '.gpu.bin')
                    run(context + '-gpu-' + variant,[mapped / 'gpu_runner',case / (variant + '.inputs.bin'),gpu_output])
                    check['gpu_reference']=check_output(case / (variant + '.reference.json'),gpu_output)
            check.update(lowered_host_bitwise_parity='PASS', buffer_canaries='PASS', inputs_unchanged='PASS')
            checks[variant] = check
        if not (args.cooperative or args.refresh_explanations or args.fuse_attention):
            assert digest(case / 'device.after.B') == digest(source / 'O3/device.after.B')
        assert not list(case.glob('*.tmp-*'))
        if recipe.get('fuse_attention',False):
            assert len(stages)==(6 if int(recipe['split_width']) and int(recipe['values'][2]['shape'][1])>int(recipe['split_width']) else 5)
            assert not set(stages).intersection({6,11,19,25,4115,4121})
            assert not {15,20}.intersection(int(s['storage']) for s in launch['storages'])
        cases[context] = dict(stages=len(stages), function=function, recipe_sha256=before,
            device_image_sha256=digest(case / 'device.after.B'), source_sha256=digest(cuda),
            host_execution=checks, cuda_compilation='PASS' if args.nvcc else 'NOT_RUN', gpu_execution='PASS' if gpu else 'UNAVAILABLE')
        cases[context].update(internal_temporary_bytes=sum(int(s['bytes']) for s in launch['storages'] if s['role']=='0'),explanation_blocks=len(blocks),whirl_comment_emission='PASS' if mapped else 'NOT_RUN',
            previous_readable_code_unchanged=bool(args.previous_readable),
            previous_mapped_code_unchanged=bool(args.previous_mapped))
    report = dict(status='PASS', schema='open64.tensor-schedule-source-review.v1', cases=cases,
        lowerer_sha256=digest(args.lowerer), optimization_selected_by_printer=False,
        low_level_whirl_changed=bool(args.whirl2c), executable_code_changed=bool(args.cooperative or args.fuse_attention), gpu_execution='PASS' if all(c['gpu_execution']=='PASS' for c in cases.values()) else 'UNAVAILABLE')
    (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (root / 'SHA256SUMS').write_text(''.join(digest(p) + '  ' + str(p.relative_to(root)) + '\n'
        for p in sorted(root.rglob('*')) if p.is_file()))
    print(json.dumps(report, indent=2))


def main():
    """Require a fresh review directory and explicit selected-plan/tool inputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('artifact-dir', 'selected-review', 'lowerer', 'header-dir'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--cxx', default='g++')
    parser.add_argument('--nvcc', type=Path)
    parser.add_argument('--whirl2c', type=Path)
    parser.add_argument('--ir-b2a', type=Path)
    parser.add_argument('--previous-readable', type=Path)
    parser.add_argument('--previous-mapped', type=Path)
    parser.add_argument('--refresh-explanations',action='store_true',help='Rebuild the unchanged selected plan with current COMMENT text')
    parser.add_argument('--fuse-attention',action='store_true',help='Select fused score/softmax/latent with stable rescaled chunk merge')
    parser.add_argument('--cooperative', action='store_true',help='Select explicit warp/shared schedule with reassociation permission')
    review(parser.parse_args())


if __name__ == '__main__':
    main()
