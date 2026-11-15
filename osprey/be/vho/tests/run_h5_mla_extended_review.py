"""H5 native shape/payload and selected-binding tests, outside compiler search.

Synthetic profiles deliberately expose a memory-dominated ranking case. The
reference is unoptimized semantic High WHIRL, independently emitted as C. No
GPU performance/correctness conclusion follows from host simulation.
Design: doc/AI-COMPILER-H5-MLA-HIERARCHY-v1.0.md.
"""
import argparse
import copy
import difflib
import hashlib
import json
import math
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys

from run_h5_mla_review import sha, version4, check_trace
from mla_kernel_review import runner
from run_common_high_review import execute
from run_high_observer_review import binding

# Fixture-only JSON canonicalization uses the existing standalone JsonCpp ABI;
# compiler table/type construction remains in the production native builder.
CANONICALIZER = r'''/* H5 test-input canonicalizer, no compiler or IR mutation.
 * Design: doc/AI-COMPILER-H5-MLA-HIERARCHY-v1.0.md. */
#include <json/json.h>
#include <fstream>
#include <sstream>
#include <stdint.h>
/* Canonicalize a fresh test recipe and its existing numeric corruption seal;
 * this is not an authentication or compiler-planning service. */
int main(int argc,char **argv) {
    if(argc!=3) return 2;
    std::ifstream input(argv[1]);Json::Value value;input>>value;
    value.removeMember("recipe_fingerprint");Json::StyledWriter writer;
    std::string bytes=writer.write(value);uint64_t seal=1469598103934665603ULL;
    for(size_t i=0;i<bytes.size();++i) seal=(seal^(unsigned char)bytes[i])*1099511628211ULL;
    std::ostringstream text;text<<seal;value["recipe_fingerprint"]=text.str();
    std::ofstream output(argv[2]);output<<writer.write(value);return output.good()?0:1;
}
'''


def compare(expected, actual):
    """Check F32 finite tolerance and exact exceptional classes/signs."""
    assert len(expected) == len(actual)
    errors = []
    for ref, got in zip(struct.iter_unpack('<f', expected), struct.iter_unpack('<f', actual)):
        x, y = ref[0], got[0]
        if math.isnan(x):
            assert math.isnan(y)
        elif math.isinf(x):
            assert x == y
        else:
            assert math.isfinite(y) and math.isclose(x, y, rel_tol=3e-5, abs_tol=3e-6), (x, y)
            errors.append(abs(x - y))
    return max(errors or [0])


def main():
    """Build new semantic native fixtures and qualify only their actual winners."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('build', 'previous', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    a = parser.parse_args()
    out = a.output.resolve()
    out.mkdir(exist_ok=False, parents=True)
    for line in (a.previous / 'SHA256SUMS').read_text().splitlines():
        digest, name = line.split('  ', 1)
        assert sha(a.previous / name) == digest
    tools = a.build / 'osprey/targdir'
    src = Path(__file__).resolve().parents[4]
    env = dict(os.environ, LD_LIBRARY_PATH=':'.join(str(tools / x) for x in ('be', 'wopt', 'lno', 'whirl2c')))
    commands, records = [], []

    def run(label, argv, case, ok=True):
        """Retain exact fixture/producer/consumer commands and all diagnostics."""
        args = list(map(str, argv))
        p = subprocess.run(args, cwd=case, env=env, capture_output=True, text=True, timeout=300)
        (case / (label + '.stdout')).write_text(p.stdout)
        (case / (label + '.stderr')).write_text(p.stderr)
        commands.append(dict(argv=args, cwd=str(case), returncode=p.returncode, expected_success=ok))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert (not p.returncode) == ok, (label, p.stderr[-3000:])
        return p

    def save(path, value):
        """Write a new fixture; old receipts are read-only authenticated inputs."""
        path.write_text(json.dumps(value, indent=2) + '\n')
        return path

    def reopen(image, case):
        """Read mapped WHIRL independently with original source paths mounted."""
        run(image.name + '-reopen', [tools / 'ir_tools/ir_b2a', '-st', '-src', image, image.with_suffix('.T')], case)
        return image.with_suffix('.T').read_text()

    cpp = out / 'canonicalizer.cxx'
    cpp.write_text(CANONICALIZER)
    run('canonicalizer', ['g++', '-std=c++98', '-I' + str(src / 'osprey/libjson/include'), cpp, tools / 'libjson/libjsoncpp.a', '-o', out / 'canonicalizer'], out)
    for name, batch, length, rank in [('column-tail', 2, 65, 37), ('rank512', 1, 2048, 512), ('G2', 2, 513, 5)]:
        case = out / name
        case.mkdir()
        previous = 'inputs/H1c/mla.high.json' if name == 'G2' else 'inputs/MR2/short.high.json'
        recipe = copy.deepcopy(json.loads((a.previous / previous).read_text())['recipe'])
        old_length, old_rank = recipe['values'][2]['shape'][1:]
        for index, value in enumerate(recipe['values']):
            value['shape'] = [length if n == old_length else rank if n == old_rank else n for n in value['shape']]
            if name != 'G2' and index not in (4, 5, 45):
                value['shape'][0] = batch
        for op in recipe['operations']:
            op['grid_x'] = (math.prod(recipe['values'][op['output']]['shape']) + 127) // 128
            if op['name'] == 'common.reshape':
                op['attributes']['attr.target_shape'] = ','.join(map(str, recipe['values'][op['output']]['shape']))
        raw = save(case / 'recipe.input.json', recipe)
        run('seal', [out / 'canonicalizer', raw, case / 'recipe.json'], case)
        run('high', [tools / 'ir_tools/tensor_kernel_lower', 'high', case / 'recipe.json', case / 'source.before.B', case / 'source.high.B', case / 'source.high.json'], case)
        manifest = json.loads((case / 'source.high.json').read_text())
        for image in case.glob('source.*.B'):
            reopen(image, case)
        profile = json.loads((a.previous / 'inputs/H1c/h20_96gb-base.json').read_text())
        # Explicit toy service rates, not H20 specifications or measurements.
        profile['execution']['id'] = 'H5-synthetic-memory-dominated'
        for key in ('shared_rate_per_sm', 'simt_rate_per_sm', 'l2_rate'):
            profile['execution']['values'][key].update(value=1e13, lower=1e13, upper=1e13)
        for key in ('hbm_read_rate', 'hbm_mixed_rate', 'hbm_write_rate'):
            profile['execution']['values'][key].update(value=1e8, lower=1e8, upper=1e8)
        pp = save(case / 'profile.json', profile)
        base = json.loads((a.previous / 'inputs/MR2/short.v3.json').read_text())
        model = json.loads((a.previous / 'native-corrected/h20_96gb.h3.json').read_text())
        profile_opts = ['-DSL:gpu_device=h20_96gb', '-DSL:gpu_profile=' + str(pp), '-DSL:gpu_profile_sha256=' + sha(pp), '-DSL:gpu_profile_toolchain=fixture-toolchain-v1']

        def backend(label, bp=None, mp=None, apply=False, extra=()):
            """Separate source capture, pure search and selected application invocations."""
            lane = case / label
            lane.mkdir()
            args = [tools / 'be/be', '-fB,' + str(case / 'source.high.B'), '-fo,result.o', '-ft,phase.t', '-O3',
                '-PHASE:l=on:w=off:c=off:lpath=' + str(tools / 'lno') + ':wpath=' + str(tools / 'wopt'),
                '-DSL:partition_axis=on:ai_opt_trace=on', '-tr25', '-tt25:1', '-OPT:IEEE_arith=3:roundoff=2', *profile_opts,
                '-LNO:high_search=on:high_apply=' + ('on' if apply else 'off') + ':high_apply_checkpoint=' + str(lane / 'selected')]
            if bp:
                args += ['-LNO:high_search_bindings=' + str(bp)]
            if mp:
                args += ['-DSL:h3_model=' + str(mp), '-DSL:h3_model_sha256=' + sha(mp)]
            run(label, [*args, *extra, src / 'osprey/torch2whirl/python/tests/models/absorbed_mla.py'], lane)
            return lane, (lane / 'phase.t').read_text()

        _, trace = backend('capture')
        bound = binding(manifest, trace, base['gpu'], base['model'])
        bound['search_controls'] = base['search_controls']
        bp = save(case / 'binding.json', version4(bound))
        model['device_binding'] = re.search(r'DSL_GPU_PLAN 1 family=MLA-snapshot .*?binding=(\S+)', trace)[1]
        model['barrier_ns'] = 0.01
        mp = save(case / 'model.json', model)
        lane, trace = backend('selected', bp, mp, True, [
            '-LNO:high_device_output=' + str(case / 'device') + ':high_device_tool=' + str(tools / 'ir_tools/tensor_selected_bridge')])
        assert 'AI_HIGH_APPLY 1 COMMITTED' in trace
        winner = int(re.search(r'AI_HIGH_SEARCH 2 OBSERVED .*?winner=(\d+)', trace)[1])
        assert winner >= 12, (name, winner)
        heads, rope = recipe['values'][0]['shape'][1], recipe['values'][3]['shape'][2]
        count = check_trace(trace, batch, heads, length, rank, rope)
        product = case / 'device/pu-0'
        selected = json.loads((product / 'selected.selected.json').read_text())
        if name == 'G2':
            assert selected['head_group'] == 2
        for before in case.rglob('*.before.B'):
            after = before.with_name(before.name.replace('.before.B', '.after.B'))
            if after.exists():
                x, z = reopen(before, case), reopen(after, case)
                before.with_name(before.name.replace('.before.B', '.before-after.diff')).write_text(''.join(difflib.unified_diff(x.splitlines(True), z.splitlines(True))))
        for image in product.glob('selected.*.B'):
            reopen(image, case)
        # The original common High source is the independent ordered reference.
        run('reference-c', [tools / 'whirl2c/whirl2c', '-TARG:abi=n64', '-O0', '-ft,' + str(case / 'reference.phase.t'),
            '-CLIST:dotc_file=' + str(case / 'reference.c'), '-CLIST:doth_file=' + str(case / 'reference.h'), case / 'source.high.B'], case)
        run('reference-compile', ['gcc', '-std=c99', '-O2', '-shared', '-fPIC', '-ffp-contract=off', '-fno-fast-math',
            '-I' + str(src / 'osprey/include'), '-include', src / 'osprey/be/lno/tests/common_high_host.h', case / 'reference.c', '-lm', '-o', case / 'reference.so'], case)
        run('cuda', [tools / 'whirl2c/whirl2c', '-cuda', '-TARG:abi=n64', '-O0', '-ft,' + str(case / 'cuda.phase.t'),
            '-CLIST:dotc_file=' + str(case / 'kernels.cu'), '-CLIST:doth_file=' + str(case / 'kernels.h'), product / 'selected.low.B'], case)
        shutil.copyfile(src / 'osprey/be/vho/tests/tensor_collective_stub/cuda_runtime.h', case / 'cuda_runtime.h')
        launch = json.loads((product / 'selected.launch.json').read_text())
        host = case / 'host.cpp'
        host.write_text(runner(launch, False, reverse=True))
        run('compile', ['g++', '-std=c++17', '-O1', '-pthread', '-ffp-contract=off', '-fno-fast-math', '-I' + str(case), host, '-o', case / 'host'], case)
        raw_inputs, cache_offset = bytearray(), 0
        for value in recipe['values']:
            if value['role'] != 1:
                continue
            elements = math.prod(value['shape'])
            if value['name'] == 'cache':
                cache_offset = len(raw_inputs)
            if value['dtype'] == 'bool':
                raw_inputs.extend(bytes(int(i % length > 0 and i // length % heads != 0) for i in range(elements)))
            else:
                raw_inputs.extend(b''.join(struct.pack('<f', ((i * 17) % 37 - 18) * .005) for i in range(elements)))
        checks = {}
        variants = [('finite', None)] if name == 'rank512' else [('finite', None), ('NaN', 0x7fc00000), ('infinity', 0x7f800000), ('subnormal', 1), ('negative-zero', 0x80000000)]
        for variant, bits in variants:
            raw = bytearray(raw_inputs)
            if bits is not None:
                raw[cache_offset:cache_offset + 4] = struct.pack('<I', bits)
            ip = case / (variant + '.inputs.bin')
            ip.write_bytes(raw)
            expected = execute(case / 'reference.so', manifest, raw)
            (case / (variant + '.reference.bin')).write_bytes(expected)
            output = case / (variant + '.outputs.bin')
            run(variant, [case / 'host', ip, output], case)
            checks[variant] = compare(expected, output.read_bytes())
        records.append(dict(name=name, batch=batch, length=length, rank=rank, group=selected['head_group'],
            width=selected['split_width'], winner=winner, production_count_oracle_stages=count, reference_errors=checks, GPU=False))
        print('PASS extended', records[-1], flush=True)
    save(out / 'report.json', dict(status='PASS', cases=records, GPU=False))


if __name__ == '__main__':
    main()
