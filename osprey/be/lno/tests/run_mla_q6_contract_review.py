"""Review Q6 detached selected-plan transport contracts against genuine Q5 images.

Runs after selection. It never lowers a candidate, launches a kernel or treats
the Q3 host snapshot as a device lease. Prior immutable artifacts remain intact.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--previous', type=Path, required=True)
    parser.add_argument('--tool', type=Path, required=True)
    parser.add_argument('--test-tool', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    commands, results, references = [], [], []

    def command(label, tool, image, control, output, success=True):
        argv = list(map(str, [tool, 'q6-contract', image, control, output]))
        run = subprocess.run(argv, capture_output=True, text=True, timeout=180)
        (args.output/(label+'.stdout')).write_text(run.stdout)
        (args.output/(label+'.stderr')).write_text(run.stderr)
        commands.append(dict(label=label, argv=argv, returncode=run.returncode))
        (args.output/'commands.json').write_text(json.dumps(commands, indent=2)+'\n')
        assert (run.returncode == 0) == success, (label, run.returncode, run.stderr[-3000:])
        if not success:
            assert not output.exists(), label

    def write_control(path, control):
        path.write_text(json.dumps(control, sort_keys=True, ensure_ascii=False, separators=(',', ':'))+'\n')

    checked = 0
    for family in ('selected', 'selected-long'):
        for prior in sorted((args.previous/family).glob('*.q5')):
            name = prior.name[:-3]
            plan = json.loads((prior/'selected.plan.json').read_text())
            original = (prior/'selected.after.B').read_bytes()
            for delivery in (1, 2):
                label = name+('-sync' if delivery == 1 else '-bulk')
                control = dict(schema='open64.q6.placement-control.v1', delivery=delivery, arch=90,
                               ptx=806, toolkit=12080, max_threads=1024, shared_limit=48*1024,
                               q3_binding_key=plan['binding_key'], scope='offline-selected-contract')
                config, target = args.output/(label+'.control.json'), args.output/(label+'.contract.json')
                write_control(config, control)
                command(label, args.tool, prior/'selected.after.B', config, target, bool(plan['width']))
                if not plan['width']:
                    results.append(dict(case=name, delivery=delivery, outcome='source-baseline-fallback'))
                    continue
                report = json.loads(target.read_text())
                assert report['schema'] == 'open64.q6.selected-placement-contract.v1'
                assert report['block_x'] == 32*plan['group']
                assert report['raw_slots'] == (2 if delivery == 2 else 1)
                assert report['q3_binding_key'] == plan['binding_key']
                assert report['source_unchanged'] and report['native_mutations'] == 0 and report['search_invocations'] == 0
                assert report['aot_selection'] == 'deferred'
                assert not any(report[k] for k in ('gpu_execution_eligible', 'device_bindings_implemented', 'kernel_emitted', 'Q5_cost_reused_for_async_pipeline'))
                assert len(report['pipeline']) == 21*report['tiles_per_pass']
                assert report['shared_bytes'] <= control['shared_limit']
                # Independent layout arithmetic: four raw streams have padded
                # 16-byte starts; the decoded tile contains W*(R+Ro) F32 values.
                rows = [s['row_bytes'] for s in report['streams']]
                align = lambda value: (value+15)//16*16
                running = 0
                for stream in report['streams']:
                    assert stream['offset'] == running
                    running += align(plan['width']*stream['row_bytes'])
                assert running == report['raw_stride']
                assert report['decoded_offset'] == running*report['raw_slots']
                scale = 2 if 'u4' in name else 1
                decoded = sum(align(plan['width']*rows[i]*scale*4) for i in (0, 2))
                assert decoded == report['decoded_bytes']
                assert report['barrier_offset'] == report['decoded_offset']+decoded
                assert report['shared_bytes'] == align(report['barrier_offset']+(16 if delivery == 2 else 0))
                # Both a single-tile case and a 33-tile tail case run the native
                # exhaustive residue oracle and schedule corruption suite.
                if name in ('dense_s8_decode', 'paged_u4_prefill_long'):
                    tests = args.output/(label+'.tests.json')
                    command(label+'-native-negatives', args.test_tool, prior/'selected.after.B', config, tests)
                    test_report = json.loads(tests.read_text())
                    assert test_report['mapping_key'] == report['mapping_key']
                    checked += test_report['contract_checks']
                for suffix, image, changed in (
                    ('old-source', prior/'selected.before.B', control),
                    ('stale-native', prior/'native.N', control),
                    ('stale-Q3', prior/'selected.after.B', dict(control, q3_binding_key=plan['binding_key']+'stale')),
                    ('resource', prior/'selected.after.B', dict(control, shared_limit=1)),
                    ('arch', prior/'selected.after.B', dict(control, arch=80)),
                ):
                    bad_config = args.output/(label+'-'+suffix+'.json')
                    write_control(bad_config, changed)
                    command(label+'-'+suffix, args.tool, image, bad_config,
                            args.output/(label+'-'+suffix+'.invalid-contract.json'), False)
                assert (prior/'selected.after.B').read_bytes() == original
                results.append(dict(case=name, delivery=delivery, outcome='qualified-detached-contract',
                                    shared_bytes=report['shared_bytes'], grid_x=report['grid_x'],
                                    tiles_per_pass=report['tiles_per_pass'], contract=target.name))
            references.append(dict(path=str(prior/'selected.after.B'), sha256=hashlib.sha256(original).hexdigest()))
    assert len(results) == 26 and checked > 130000
    result = dict(status='PASS', results=results, detached_native_checks=checked,
                  selected_inputs=len(references), input_hashes=references, selected_inputs_unchanged=True,
                  mappings=24, baseline_fallbacks=2, reference_execution='Q5 retained, not rerun by mapping',
                  native_state_mutations=0, search_invocations=0, gpu_execution=False,
                  q6_status='placement/transport contract only; native kernel and device integration pending',
                  aot_selection='deferred')
    (args.output/'report.json').write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps({k: v for k, v in result.items() if k not in ('results', 'input_hashes')}, indent=2))


if __name__ == '__main__':
    main()
