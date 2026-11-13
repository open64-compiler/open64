"""H6 selected-only native/WHIRL-derived reference and refusal qualification.

Uses authenticated H3 fixture profiles and synthetic rates. Search performs no
execution; this harness runs only published winners afterward. Neither host
simulation nor offline CUDA/PTX compilation claims NVIDIA execution or speedup.
Design: doc/AI-COMPILER-H6-NORM-REDUCTION-v1.0.md.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys


def sha(path):
    """Bind exact retained bytes, without editing any prior receipt."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def reduction_runner(rows, width, kind, threads):
    """Wrap actual whirl2c CUDA; substitute CTA mechanics only, never math."""
    grid = (rows + threads - 1) // threads if kind == 1 else rows
    return r'''#include <cstdint>
#include <cstring>
#include <cfenv>
#include <iomanip>
#include <iostream>
#include <vector>
#include <xmmintrin.h>
#include "selected.cu"
static uint32_t bits(float x) {uint32_t b;std::memcpy(&b,&x,4);return b;}
static void assign(float &x,uint32_t b) {std::memcpy(&x,&b,4);}
int main() {
  if(std::fesetround(FE_TONEAREST))return 2;
  _mm_setcsr(_mm_getcsr() & ~((1u<<15)|(1u<<6)));
  unsigned cases;if(!(std::cin>>cases))return 2;
  for(unsigned c=0;c<cases;++c) {
    std::vector<float> x(ROWS*WIDTH+32),y(ROWS+32);
    for(auto *v:{&x,&y})for(float &f:*v)assign(f,0xdeadbeef);
    for(unsigned i=16;i<x.size()-16;++i) {uint32_t word;if(!(std::cin>>std::hex>>word>>std::dec))return 2;assign(x[i],word);}
    auto original=x;
    for(bool reverse:{false,true}) {
      for(float &f:y)assign(f,0xdeadbeef);
      open64_host_launch(GRID,THREADS,[&]{h6_reduction(x.data()+16,y.data()+16);},reverse);
      for(unsigned i=0;i<x.size();++i)if(bits(x[i])!=bits(original[i]))return 3;
      for(unsigned i=0;i<16;++i)if(bits(y[i])!=0xdeadbeef||bits(y[y.size()-1-i])!=0xdeadbeef)return 4;
      for(unsigned i=16;i<y.size()-16;++i)std::cout<<std::hex<<std::setw(8)<<std::setfill('0')<<bits(y[i])<<' ';
      std::cout<<std::dec<<'\n';
    }
  }
}
'''.replace('ROWS', str(rows)).replace('WIDTH', str(width)).replace('GRID', str(grid)).replace('THREADS', str(threads))


def audit_work(text, rows, width):
    """Independently enumerate warp-instruction sectors; verify logical counts.

    Deliberately enumerate every column rather than production's periodic
    shortcut. No modeled transaction value is relabeled physical HBM traffic.
    """
    def contiguous(n, quantum):
        """Worst F32 base residue across contiguous warp instructions."""
        return max(sum(len({(offset + 4 * i) // quantum for i in range(k, min(k + 32, n))}) * quantum
                       for k in range(0, n, 32)) for offset in range(0, quantum, 4))

    for line in text.splitlines():
        if not line.startswith('AI_H6_WORK '):
            continue
        fields = dict(word.split('=', 1) for word in line.split()[2:])
        name = fields['plan']
        read, write, read_tx, write_tx = [int(fields[k]) for k in ['read', 'write', 'read_tx', 'write_tx']]
        quantum = 32  # Pinned input model below; varied quanta have separate fixtures.
        assert read_tx >= (read + quantum - 1) // quantum * quantum
        assert write_tx >= (write + quantum - 1) // quantum * quantum
        if '/Reduction/' in name:
            kind, threads, _ = map(int, re.search(r'/K(\d+)/T(\d+)/C(\d+)', name).groups())
            active = (rows % threads if fields['stage'] == '1' else min(rows, threads)) if kind == 1 else 1
            assert read == active * width * 4 and write == active * 4
            if kind == 1:
                expected = max(sum(len({(offset + 4 * (lane * width + k)) // quantum
                                        for lane in range(warp, min(warp + 32, active))}) * quantum
                                   for warp in range(0, active, 32) for k in range(width))
                               for offset in range(0, quantum, 4))
            else:
                expected = contiguous(width, quantum)
            assert read_tx == expected, (name, read_tx, expected)
        else:
            kind, threads = map(int, re.search(r'/C(\d+)/T(\d+)', name).groups())
            binary = [8, 7, 7][kind] * width + 4
            carrier_accesses = 3 * binary + 2
            assert read == [5, 4, 3][kind] * width * 4 + 4 * carrier_accesses
            assert write == 4 * width + 4 * carrier_accesses
            assert read_tx == carrier_accesses * quantum + (5 * width * quantum if kind == 0 else
                               (width * quantum if kind == 1 else 0) + 3 * contiguous(width, quantum))
            assert int(fields['shared']) == (0 if kind == 0 else [0, 12, 24][kind] * width + 8 + 8 * min(width, threads))
            assert int(fields['barriers']) == [0, 4, 5][kind]


def main():
    """Retain commands, raw mapped boundaries, generated CUDA and oracle output."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build', type=Path, required=True)
    parser.add_argument('--previous-h3', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--cuda', type=Path)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    source = Path(__file__).resolve().parents[4]
    tools = args.build / 'osprey/targdir'
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE='1', LD_LIBRARY_PATH=':'.join(str(tools / x) for x in ['be', 'wopt', 'lno', 'whirl2c']))
    previous = args.previous_h3
    manifest = dict((name, digest) for digest, name in (row.split('  ', 1) for row in (previous / 'SHA256SUMS').read_text().splitlines()))
    commands, cases = [], []
    counterexample_outputs = 0

    def run(label, argv, case, input_text=None, success=True):
        """Persist process evidence before enforcing acceptance or refusal."""
        process = subprocess.run(list(map(str, argv)), cwd=case, env=env, input=input_text, capture_output=True, text=True, timeout=300)
        (case / (label + '.stdout')).write_text(process.stdout)
        (case / (label + '.stderr')).write_text(process.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), cwd=str(case), returncode=process.returncode, expected_success=success))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (process.returncode == 0) == success, (label, process.stderr[-4000:])
        return process

    def view(image, case):
        """Exact-stem, full symbol and original-source interleave in a fresh process."""
        target = image.with_suffix('.T')
        run(image.stem + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', image, target], case)
        return target

    sys.path.insert(0, str(source / 'osprey/be/opt/tests'))
    from normalization_lower_host import adapter, datasets, protocol, audit_output
    from normalization_guard_trace_report import decode
    from normalization_oracle import add, equivalent
    for device in ['h20_96gb', 'h20_141gb']:
        profile = previous / 'inputs/H1c' / (device + '-base.json')
        model = previous / 'native-corrected' / (device + '.h3.json')
        for p in [profile, model]:
            assert sha(p) == manifest[str(p.relative_to(previous))]
        workloads = [('ln', 'unit', 1, 1), ('ln', 'default', 3, 17), ('ln', 'wide', 7, 65), ('ln', 'feature-tail', 2, 257),
                     ('reduce', 'ordered', 129, 1), ('reduce', 'ordered', 1, 17), ('reduce', 'ordered', 2, 65), ('reduce', 'ordered', 7, 257)]
        for index, (family, variant, rows, width) in enumerate(workloads):
            case = out / (device + '-' + family + '-' + str(index))
            case.mkdir()
            prefix = case / 'selected'
            run('native', [tools / 'ir_tools/dsl_h6_test', prefix, family, variant, device, profile, sha(profile), model, sha(model), rows, width], case)
            phase = (case / 'selected.phase.t').read_text()
            assert 'complete=1' in phase and 'selected_faults=9' in phase and 'native_search_mutation=none' in phase
            audit_work(phase, rows, width)
            for boundary in ['source', 'device']:
                a, b = [view(case / ('selected.' + boundary + '.' + point + '.B'), case) for point in ['before', 'after']]
                difference = ''.join(difflib.unified_diff(a.read_text().splitlines(True), b.read_text().splitlines(True), fromfile=a.name, tofile=b.name))
                (case / ('selected.' + boundary + '.before-after.diff')).write_text(difference)
                assert bool(difference) == (boundary == 'device')
            run('cuda', [tools / 'whirl2c/whirl2c', '-cuda', '-O0', '-TARG:abi=n64', '-ft,' + str(case / 'cuda.phase.t'),
                '-CLIST:dotc_file=' + str(case / 'selected.cu'), '-CLIST:doth_file=' + str(case / 'selected.h'), case / 'selected.device.after.B'], case)
            cuda = (case / 'selected.cu').read_text()
            assert 'H6' in cuda and 'COMMENT' in (case / 'selected.device.after.T').read_text()
            kernel = dict(word.split('=', 1) for word in next(line for line in phase.splitlines() if line.startswith('AI_H6_KERNEL ')).split()[2:])
            if family == 'ln':
                run('native-key-reopen', [tools / 'ir_tools/dsl_h6_test', 'reopen-ln', case / 'selected.device.after.B', case / 'selected.plan', case / 'selected.product',
                    device, profile, sha(profile), model, sha(model), case / 'reopened.phase.t'], case)
                guard = decode((case / 'selected.guard').read_bytes())
                facts = list(map(int, next(line for line in phase.splitlines() if line.startswith('AI_NORM_LOWER 1 MATERIALIZE')).split()[3:]))
                record = dict(guard=guard, facts=facts, kernel_name='h6_layernorm')
                (case / 'host.cpp').write_text(adapter(cuda, record))
                sets = datasets(record)
                wire = protocol(record, sets)
            else:
                run('native-key-reopen', [tools / 'ir_tools/dsl_h6_test', 'reopen', case / 'selected.device.after.B', case / 'selected.native-key'], case)
                (case / 'host.cpp').write_text(reduction_runner(rows, width, int(kernel['kind']), int(kernel['threads'])))
                shutil.copyfile(source / 'osprey/be/vho/tests/tensor_collective_stub/cuda_runtime.h', case / 'cuda_runtime.h')
                pools = [[0, 0x80000000, 0x3f800000, 0xbf800000], [1, 2, 0x80000001, 0x007fffff],
                         [0x60ad78ec, 0xe0ad78ec, 0x3f800000, 0x40000000], [0x7f800000, 0xff800000, 0x7fc12345],
                         [0x3f800001, 0x33800000, 0xbf800000]]
                sets = [[pool[(i + i // width) % len(pool)] for i in range(rows * width)] for pool in pools]
                wire = str(len(sets)) + '\n' + '\n'.join(' '.join(f'{v:08x}' for v in values) for values in sets) + '\n'
            (case / 'inputs.txt').write_text(wire)
            run('host-compile', ['g++', '-std=c++17', '-O2', '-pthread', '-ffp-contract=off', '-fno-fast-math', '-I' + str(case), case / 'host.cpp', '-o', case / 'host'], case)
            result = run('reference', [case / 'host'], case, wire)
            if family == 'ln':
                checks = audit_output(record, sets, result.stdout)
            else:
                lines = result.stdout.splitlines()
                assert len(lines) == len(sets) * 2
                checks = 0
                for c, values in enumerate(sets):
                    expected = []
                    for row in range(rows):
                        total = 0
                        for v in values[row * width:(row + 1) * width]:
                            total = add(total, v)
                        expected.append(total)
                    for line in lines[2 * c:2 * c + 2]:
                        actual = [int(v, 16) for v in line.split()]
                        assert len(actual) == rows and all(equivalent(a, b) for a, b in zip(actual, expected))
                        checks += rows
            if args.cuda and index in (2, 7):
                static = case / 'offline'
                static.mkdir()
                shutil.copyfile(case / 'selected.cu', static / 'selected.cu')
                if (case / 'selected.h').exists():
                    shutil.copyfile(case / 'selected.h', static / 'selected.h')
                flags = ['-std=c++17', '-arch=sm_90', '--fmad=false', '--ftz=false', '--prec-div=true', '--prec-sqrt=true']
                run('nvcc', [args.cuda / 'bin/nvcc', *flags, '-dc', static / 'selected.cu', '-o', static / 'selected.o'], static)
                run('ptx', [args.cuda / 'bin/nvcc', *flags, '-ptx', static / 'selected.cu', '-o', static / 'selected.ptx'], static)
            cases.append(dict(device=device, family=family, rows=rows, width=width, selected=kernel, reference_outputs=checks, measured_GPU=False))
            (out / 'report.json').write_text(json.dumps(dict(status='RUNNING', cases=cases), indent=2) + '\n')
            print('PASS', case.name, 'reference_outputs', checks, flush=True)
        for control, value in [('site_limit', 1), ('request_limit', 1), ('memory_limit', 4096), ('deadline_us', 1), ('barrier_ns', 1000000000)]:
            case = out / (device + '-' + control)
            case.mkdir()
            supplement = json.loads(model.read_text())
            supplement[control] = value
            pinned = case / 'model.json'
            pinned.write_text(json.dumps(supplement) + '\n')
            run('native', [tools / 'ir_tools/dsl_h6_test', case / 'selected', 'ln', 'wide', device, profile, sha(profile), pinned, sha(pinned), 7, 65], case)
            phase = (case / 'selected.phase.t').read_text()
            if control != 'barrier_ns':
                assert not (case / 'selected.device.after.B').exists()
            else:
                assert 'kind=1 ' in phase
            for image in case.glob('*.B'):
                view(image, case)
            a, b = [case / ('selected.source.' + point + '.T') for point in ['before', 'after']]
            difference = ''.join(difflib.unified_diff(a.read_text().splitlines(True), b.read_text().splitlines(True)))
            (case / 'selected.source.before-after.diff').write_text(difference)
            assert not difference
            if control == 'barrier_ns':
                run('native-key-reopen', [tools / 'ir_tools/dsl_h6_test', 'reopen-ln', case / 'selected.device.after.B', case / 'selected.plan', case / 'selected.product',
                    device, profile, sha(profile), pinned, sha(pinned), case / 'reopened.phase.t'], case)
                run('cuda', [tools / 'whirl2c/whirl2c', '-cuda', '-O0', '-TARG:abi=n64', '-ft,' + str(case / 'cuda.phase.t'),
                    '-CLIST:dotc_file=' + str(case / 'selected.cu'), '-CLIST:doth_file=' + str(case / 'selected.h'), case / 'selected.device.after.B'], case)
                guard = decode((case / 'selected.guard').read_bytes())
                facts = list(map(int, next(line for line in phase.splitlines() if line.startswith('AI_NORM_LOWER 1 MATERIALIZE')).split()[3:]))
                record = dict(guard=guard, facts=facts, kernel_name='h6_layernorm')
                (case / 'host.cpp').write_text(adapter((case / 'selected.cu').read_text(), record))
                run('host-compile', ['g++', '-std=c++17', '-O2', '-ffp-contract=off', '-fno-fast-math', case / 'host.cpp', '-o', case / 'host'], case)
                sets = datasets(record)
                wire = protocol(record, sets)
                (case / 'inputs.txt').write_text(wire)
                result = run('reference', [case / 'host'], case, wire)
                counterexample_outputs += audit_output(record, sets, result.stdout)
                a, b = [case / ('selected.device.' + point + '.T') for point in ['before', 'after']]
                (case / 'selected.device.before-after.diff').write_text(''.join(difflib.unified_diff(a.read_text().splitlines(True), b.read_text().splitlines(True), fromfile=a.name, tofile=b.name)))
                print('PASS C0 selected reference', device, flush=True)
    (out / 'report.json').write_text(json.dumps(dict(status='PASS', cases=cases, reference_outputs=sum(c['reference_outputs'] for c in cases) + counterexample_outputs, counterexample_reference_outputs=counterexample_outputs, budget_refusals=8, high_barrier_C0=2,
        measured_GPU=False, timing='not-run', profiling='not-run', input_receipt=str(previous)), indent=2) + '\n')


if __name__ == '__main__':
    main()
