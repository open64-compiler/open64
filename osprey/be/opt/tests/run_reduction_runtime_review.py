"""Build a separate runtime and exercise actual native-exported guard files.

Design: doc/AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md. Retains raw .t logs.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
from reduction_runtime_trace_report import write_reports


def main():
    """Never replace an existing output directory or claim device execution."""
    parser = argparse.ArgumentParser()
    parser.add_argument('--native-dir', required=True, type=Path)
    parser.add_argument('--output-dir', required=True, type=Path)
    parser.add_argument('--sanitize', action='store_true')
    args = parser.parse_args()
    output = args.output_dir.resolve()
    output.mkdir(parents=True, exist_ok=False)
    repo = Path(__file__).resolve().parents[3]
    commands = []

    def run(label, command):
        """Record exact commands and keep all diagnostics after completion."""
        command = list(map(str, command))
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        (output / (label + '.log')).write_text('# What it summarizes: ' + label + ' command output.\n' + result.stdout)
        commands.append(dict(label=label, argv=command, returncode=result.returncode))
        (output / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        if result.returncode:
            raise RuntimeError(label + ': ' + result.stdout[-4000:])
        return result.stdout

    compiler = ['c++', '-std=c++11', '-pthread', '-Wall', '-Wextra', '-Werror',
                '-I' + str(repo / 'include')]
    if args.sanitize:
        compiler += ['-fsanitize=address,undefined', '-fno-omit-frame-pointer']
    run('build', compiler + [repo / 'libopen64rt/reduction_runtime.cxx',
        repo / 'libopen64rt/tests/reduction_runtime_test.cxx', '-o', output / 'runtime-test'])
    # Compile the public ABI as C too, independently of C++ implementation.
    header = output / 'header.c'
    header.write_text('#include "open64_reduction_runtime.h"\nint main(void) { return 0; }\n')
    run('c-header', ['cc', '-std=c11', '-Wall', '-Wextra', '-Werror',
        '-I' + str(repo / 'include'), '-c', header, '-o', output / 'header.o'])
    cases = []
    for case in ('hopper', 'hopper-a', 'blackwell', 'blackwell-a', 'unit-width', 'wide'):
        native = args.native_dir.resolve() / case
        trace = output / (case + '.t')
        on = run(case, [output / 'runtime-test', native / 'guard.bin', trace])
        off = run(case + '-trace-off', [output / 'runtime-test', native / 'guard.bin', 'off'])
        assert on == off and 'gpu=0' in on
        counts = write_reports(trace, native / 'trace/direct.t', output / case)
        cases.append(dict(case=case, counts=counts, result=on.strip()))
    for name in ('optimization.log', 'optimization-summary.log'):
        (output / name).write_text('# What it summarizes: all six runtime guard review cases.\n' +
            ''.join('case=' + c['case'] + '\n' + (output / c['case'] / name).read_text() for c in cases))
    (output / 'receipt.json').write_text(json.dumps(dict(schema=1, cases=cases,
        runtime_enforcement='host_mock', transformation='none', selected=False,
        gpu=False, sanitizers=args.sanitize), indent=2) + '\n')
    files = sorted(path for path in output.rglob('*') if path.is_file())
    (output / 'SHA256SUMS').write_text(''.join(hashlib.sha256(path.read_bytes()).hexdigest() +
        '  ' + str(path.relative_to(output)) + '\n' for path in files))
    print('runtime guard review passed: six native-exported contracts; GPU not run')


if __name__ == '__main__':
    main()
