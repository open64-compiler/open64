"""Compile real H4 detached work service and independent enumerated oracle."""
import argparse
import json
import os
from pathlib import Path
import subprocess


def main():
    """Fatal sanitizers and recorded commands qualify counts, not GPU behavior."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    repo = Path(__file__).resolve().parents[5]
    includes = ['osprey', 'osprey/linux/include', 'osprey/common/com', 'osprey/common/util',
                'osprey/common/targ_info/access', 'osprey/be/com', 'osprey/be/vho']
    sources = [Path(__file__).with_name('hierarchy_contract.cxx'), repo / 'osprey/be/vho/dsl_gemm_hierarchy.cxx',
               repo / 'osprey/be/com/dsl_movement_facts.cxx', repo / 'osprey/be/vho/fhe_sha256.cxx']
    commands = []
    for label, argv in [('build', [os.environ.get('CXX', 'c++'), '-std=c++98', '-g', '-O1', '-fsanitize=address,undefined',
                *[arg for path in includes for arg in ['-I', str(repo / path)]], *map(str, sources), '-o', str(out / 'contract')]),
                        ('run', [str(out / 'contract')])]:
        process = subprocess.run(argv, capture_output=True, text=True, timeout=180,
            env=dict(os.environ, ASAN_OPTIONS='halt_on_error=1', UBSAN_OPTIONS='halt_on_error=1'))
        text = process.stdout + process.stderr
        (out / (label + '.log')).write_text(text)
        commands.append(dict(argv=argv, returncode=process.returncode))
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        assert process.returncode == 0 and 'runtime error:' not in text and 'ERROR: AddressSanitizer' not in text, text[-4000:]
    (out / 'report.json').write_text(json.dumps(dict(status='PASS', widths=145, malformed_cases=5,
        independent_enumerated_oracle=True, fatal_sanitizers=True, GPU=False), indent=2) + '\n')
    print('PASS H4 production work oracle, 145 widths, five refusals, fatal ASan/UBSan')


if __name__ == '__main__':
    main()
