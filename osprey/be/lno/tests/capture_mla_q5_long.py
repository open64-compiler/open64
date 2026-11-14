"""Capture genuine native 8193-token Q5 inputs; retain source and baseline High.

The existing native frontend alone builds mapped WHIRL. No backend implementation
is imported into ingestion. Later native lowering runs in separate processes.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import shutil
import sys
import torch
from open64_dsc import export_to_whirl, save_as_whirl
from open64_dsc.options import WhirlExportOptions
from mla_q5_long_fixture import CASES, make_fixture


def capture(args):
    """Produce immutable inputs and a source-compatible baseline for Q3 leases."""
    root = args.output.resolve()
    root.mkdir(exist_ok=False)
    if args.case is None:
        # Native builder/interpreter state is process-owned. Capture each model
        # in a fresh process, then authenticate the complete combined bundle.
        commands = []
        for name in CASES:
            child = root/name
            argv = [sys.executable, __file__, '--source', str(args.source), '--build', str(args.build), '--output', str(child), '--case', name]
            result = subprocess.run(argv, capture_output=True, text=True, timeout=600)
            (root/(name+'.stdout')).write_text(result.stdout)
            (root/(name+'.stderr')).write_text(result.stderr)
            commands.append(dict(argv=argv, returncode=result.returncode))
            (root/'commands.json').write_text(json.dumps(commands, indent=2)+'\n')
            assert result.returncode == 0, result.stderr[-4000:]
            destination = root/'production'/(name+'-O3')
            destination.parent.mkdir(exist_ok=True)
            shutil.copytree(child/'production'/(name+'-O3'), destination)
        (root/'SHA256SUMS').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p.relative_to(root))+'\n' for p in sorted(root.rglob('*')) if p.is_file()))
        return
    commands = []
    tools = args.build/'osprey/targdir'
    def run(argv, cwd):
        p = subprocess.run(list(map(str, argv)), cwd=cwd, capture_output=True, text=True, timeout=300)
        number = len(commands)
        (root/('command-'+str(number)+'.stdout')).write_text(p.stdout)
        (root/('command-'+str(number)+'.stderr')).write_text(p.stderr)
        commands.append(dict(argv=list(map(str, argv)), returncode=p.returncode))
        (root/'commands.json').write_text(json.dumps(commands, indent=2)+'\n')
        assert p.returncode == 0, p.stderr[-4000:]
    for name in ([args.case] if args.case else CASES):
        case = root/'production'/(name+'-O3')
        case.mkdir(parents=True)
        model, inputs, meta = make_fixture(name)
        with torch.no_grad():
            expected = model(*inputs)
        module = export_to_whirl(model, inputs, WhirlExportOptions(backend='native', model_name=name))
        save_as_whirl(module, str(case/'source.B'))
        torch.save(dict(inputs=inputs, source=expected), case/'inputs-and-source.pt')
        (case/'fixture.json').write_text(json.dumps(meta, indent=2)+'\n')
        run([tools/'ir_tools/ir_b2a', '-st', '-src', case/'source.B', case/'source.T'], case)
        run([tools/'ir_tools/tensor_kernel_lower', 'q1-prepare', case/'source.B', case/'recipe.json'], case)
        run([tools/'ir_tools/tensor_kernel_lower', 'q1-high', case/'recipe.json', case/'empty.before.B', case/'q1.high.after.B', case/'q1.high.after.manifest.json'], case)
        run([tools/'ir_tools/ir_b2a', '-st', '-src', case/'q1.high.after.B', case/'q1.high.after.T'], case)
        c = case/'q1.high.after.c'
        run([tools/'whirl2c/whirl2c', '-TARG:abi=n64', '-O0', '-ft,'+str(case/'w2c.phase.t'), '-CLIST:dotc_file='+str(c), '-CLIST:doth_file='+str(c.with_suffix('.h')), case/'q1.high.after.B'], case)
        run(['gcc', '-O2', '-std=c99', '-shared', '-fPIC', '-ffp-contract=off', '-fno-fast-math', '-I'+str(args.source/'osprey/include'), '-include', args.source/'osprey/be/lno/tests/common_high_host.h', c, '-lm', '-o', case/'q1.high.after.so'], case)
        print('PASS native capture '+name, flush=True)
    (root/'SHA256SUMS').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p.relative_to(root))+'\n' for p in sorted(root.rglob('*')) if p.is_file()))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source', 'build', 'output'):
        parser.add_argument('--'+name, type=Path, required=True)
    parser.add_argument('--case', choices=CASES)
    capture(parser.parse_args())
