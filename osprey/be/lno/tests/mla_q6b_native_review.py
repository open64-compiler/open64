"""Selected-only Q6b construction, native reopening and delayed-copy references.

Reuse Q5 selection/Q3 qualification; no new estimator or runtime dispatcher.
Only explicitly requested transport fixtures are compiled after one W/G wins.
"""
import ctypes as C
import difflib
import json
import re
from pathlib import Path
from mla_q5_plan_review import Q5_REVIEW


class Q6B_REVIEW(Q5_REVIEW):
    """Qualify one selected High staging body, retaining Q3's exact callback ABI."""
    def __init__(self, api, args, out):
        super().__init__(api, args, out)
        self.stage_cases = {}

    def Before(self, c, check, name=None):
        """Selection finishes in Q5 before any staging WN or reference execution."""
        super().Before(c, check, name)
        name = name or c.case
        selected = self.out/(name+'.q5')
        plan = json.loads(c.q5_plan)
        bridge = self.tools/'ir_tools/tensor_selected_bridge'
        test_bridge = self.tools/'ir_tools/tensor_selected_bridge_test'
        if not plan['width']:
            control = self.Control(plan, 2)
            config = selected/'q6b.fallback-control.json'
            config.write_text(json.dumps(control, sort_keys=True, separators=(',', ':'))+'\n')
            self.Command('q6b-source-fallback', [bridge, 'q6-apply', selected/'selected.after.B', config,
                         selected/'fallback.before.B', selected/'fallback.after.B', selected/'fallback.manifest.json', '--opt-level=3'], selected, False)
            check(name+' Q6b source fallback has no candidate', not (selected/'fallback.before.B').exists() and not (selected/'fallback.after.B').exists())
            self.stage_cases[name] = dict(fallback='qualified-Q5-source', transformed=False)
            self.Report()
            return
        import torch
        from run_quantized_high_review import execute
        expected = execute(selected/'native.so', json.loads((selected/'selected.manifest.json').read_text()), c.inputs)
        # Async is the requested selected transport. Two explicit synchronous
        # fixtures exercise its implementation/fallback boundary, not a search
        # over performance candidates or timing-based variant selection.
        deliveries = (1, 2) if name in ('dense_s8_decode', 'paged_u4_prefill_long') else (2,)
        results = []
        for delivery in deliveries:
            target = selected/('q6b-sync' if delivery == 1 else 'q6b-async')
            target.mkdir()
            control = self.Control(plan, delivery)
            config = target/'control.json'
            config.write_text(json.dumps(control, sort_keys=True, separators=(',', ':'))+'\n')
            self.Command('q6b-contract', [bridge, 'q6-contract', selected/'selected.after.B', config, target/'contract.json'], target)
            layout = json.loads((target/'contract.json').read_text())
            for fault, option, change in (
                ('O0', '--opt-level=0', None),
                ('Q3', '--opt-level=3', dict(control, q3_binding_key=plan['binding_key']+'stale')),
                ('resources', '--opt-level=3', dict(control, shared_limit=1)),
            ):
                bad_config = config
                if change:
                    bad_config = target/(fault+'.control.json')
                    bad_config.write_text(json.dumps(change, sort_keys=True, separators=(',', ':'))+'\n')
                self.Command('q6b-'+fault+'-refusal', [bridge, 'q6-apply', selected/'selected.after.B', bad_config,
                             target/(fault+'.before.B'), target/(fault+'.after.B'), target/(fault+'.manifest.json'), option], target, False)
                check(name+' Q6b '+fault+' creates no staging image', not (target/(fault+'.before.B')).exists() and not (target/(fault+'.after.B')).exists())
            self.Command('q6b-apply', [bridge, 'q6-apply', selected/'selected.after.B', config,
                         target/'staged.before.B', target/'staged.after.B', target/'manifest.json', '--opt-level=3'], target)
            manifest = json.loads((target/'manifest.json').read_text())
            assert manifest['selected_plan'].encode() == c.q5_plan
            assert manifest['q3_binding_key'] == plan['binding_key'] and manifest['shared_bytes'] == layout['shared_bytes']
            assert manifest['new_TCONs'] == 0 and manifest['exploration_native_mutations'] == 0 and manifest['candidates_materialized'] == 1
            self.Command('q6b-reopen-audit', [bridge, 'q6-audit', target/'staged.after.B', target/'keys.json'], target)
            check(name+' Q6b exact reopened native key', json.loads((target/'keys.json').read_text())['key'] == manifest['native_key'])
            for operation in ('init', 'acquire', 'copy', 'wait', 'publish', 'consume', 'release', 'end', 'view-type'):
                failed = self.Command('q6b-missing-'+operation, [test_bridge, 'q6-audit', target/'staged.after.B', target/(operation+'.invalid.keys.json'), '--test-remove', operation], target, False)
                assert 'q6b_stale_native_key' in failed.stderr
                check(name+' Q6b stale '+operation+' rejects before emission', not (target/(operation+'.invalid.keys.json')).exists())
            if name in ('dense_s8_decode', 'paged_u4_prefill_long'):
                self.Command('q6b-post-build-refusal', [test_bridge, 'q6-apply', selected/'selected.after.B', config,
                             target/'rejected.before.B', target/'rejected.after.B', target/'rejected.manifest.json', '--opt-level=3', '--test-reject-after-build'], target, False)
                self.Command('q6b-rejected-before-dump', [self.tools/'ir_tools/ir_b2a', '-st', '-src', target/'rejected.before.B', target/'rejected.before.T'], target)
                check(name+' Q6b failed construction retains before only', not (target/'rejected.after.B').exists() and not (target/'rejected.manifest.json').exists()
                      and self.Input_before(selected/'selected.after.T', target/'rejected.before.T'))
            for stem in ('staged.before', 'staged.after'):
                self.Command(stem+'-dump', [self.tools/'ir_tools/ir_b2a', '-st', '-src', target/(stem+'.B'), target/(stem+'.T')], target)
            (target/'staged.before-after.diff').write_text(''.join(difflib.unified_diff((target/'staged.before.T').read_text().splitlines(True), (target/'staged.after.T').read_text().splitlines(True))))
            assert self.Input_before(selected/'selected.after.T', target/'staged.before.T')
            backend = self.tools/'be/be'
            self.Command('q6b-backend', [backend, '-fB,'+str(target/'staged.after.B'), '-fo,'+str(target/'native.o'), '-ft,'+str(target/'backend.phase.t'),
                         '-PHASE:l=on:w=off:c=off:lpath='+str(self.tools/'lno')+':wpath='+str(self.tools/'wopt'), '-O3', '-LNO:simd=0:vintr=0',
                         '-OPT:IEEE_arith=1:roundoff=0', '-DSL:q1_checkpoint='+str(target/'q6b')+':ai_opt_trace=on', '-tr26', '-tt26:1',
                         self.args.source/'osprey/torch2whirl/python/tests/models/mla_quantized_paged.py'], target)
            for image in (target/'q6b.preopt-0.before.B', target/'q6b.preopt-0.after.B', target/'native.N'):
                self.Command(image.stem+'-dump', [self.tools/'ir_tools/ir_b2a', '-st', '-src', image, image.with_suffix('.T')], target)
            executions = []
            for stem, image in (('staged.after', target/'staged.after.B'), ('native', target/'native.N')):
                source = target/(stem+'.c')
                self.Command(stem+'-emit', [self.tools/'whirl2c/whirl2c', '-TARG:abi=n64', '-O0', '-ft,'+str(target/(stem+'.w2c.phase.t')),
                             '-CLIST:dotc_file='+str(source), '-CLIST:doth_file='+str(source.with_suffix('.h')), image], target)
                self.Command(stem+'-cc', ['gcc', '-std=c99', '-O2', '-fPIC', '-ffp-contract=off', '-fno-fast-math', '-fno-strict-aliasing',
                             '-I'+str(self.args.source/'osprey/include'), '-include', self.args.source/'osprey/be/lno/tests/common_high_host.h',
                             '-c', source, '-o', target/(stem+'.host.o')], target)
                self.Command(stem+'-link-model', ['g++', '-std=c++11', '-O2', '-fPIC', '-shared', self.args.source/'osprey/be/lno/tests/mla_q6b_stage_model.cxx',
                             target/(stem+'.host.o'), '-lm', '-o', target/(stem+'.so')], target)
                library = C.CDLL(str(target/(stem+'.so')))
                self.Model(library)
                library.open64_q6b_model_reset()
                actual = execute(target/(stem+'.so'), manifest, c.inputs)
                for a, b in zip(actual, expected):
                    torch.testing.assert_close(a, b, rtol=0, atol=0, equal_nan=True)
                events = json.loads(library.open64_q6b_model_report())
                self.Events(c, plan, layout, events)
                executions.append(dict(image=stem, events=events, exact_source_outputs=6))
                text = source.read_text()
                assert 'Q6b selected native MLA staging:' in text and '__open64_mla_stage_wait' in text and '__open64_mla_stage_consume' in text
                assert image.with_suffix('.T').stat().st_size > 1000
                # Existing guard/nonfinite fixtures exercise the new staged
                # body as well, not just its unchanged Q5 predecessor.
                for bad in self.Bad_inputs(c):
                    library.open64_q6b_model_reset()
                    execute(target/(stem+'.so'), manifest, bad, 1)
                    assert json.loads(library.open64_q6b_model_report())['quiescent']
                if c.schema['paged']:
                    bad = [x.clone() for x in c.inputs]
                    bad[3].fill_(torch.finfo(torch.float32).max)
                    bad[9].zero_()
                    original = execute(c.prior/'q1.high.after.so', c.manifest, bad)
                    library.open64_q6b_model_reset()
                    actual = execute(target/(stem+'.so'), manifest, bad)
                    for a, b in zip(actual, original):
                        torch.testing.assert_close(a, b, rtol=0, atol=0, equal_nan=True)
                    assert json.loads(library.open64_q6b_model_report())['quiescent']
            (target/'execution.json').write_text(json.dumps(executions, indent=2)+'\n')
            check(name+' Q6b '+str(delivery)+' exact High/backend reference and drain', True)
            results.append(dict(delivery=delivery, selected_width=plan['width'], selected_group=plan['group'], executions=executions,
                                shared_bytes=manifest['shared_bytes'], native_image=str(target/'staged.after.B')))
        # The chosen transport callback retains the inherited Q5/Q3 exact
        # snapshot join, leases, status, parent-CAS and atomic bundle publication.
        raw = getattr(library, manifest['function'])
        raw.argtypes, raw.restype = [C.c_void_p]*len(c.views), None
        def invoke(*args):
            library.open64_q6b_model_reset()
            raw(*args)
            assert json.loads(library.open64_q6b_model_report())['quiescent']
        c.function, c.q6b_library = invoke, library
        self.stage_cases[name] = dict(transformed=True, transports=results, q3_snapshot_join=True, physical_gpu_mapping=False)
        self.Report()

    def Model(self, library):
        """Bind test-only event observation; never called by the estimator."""
        library.open64_q6b_model_reset.argtypes, library.open64_q6b_model_reset.restype = [], None
        library.open64_q6b_model_report.argtypes, library.open64_q6b_model_report.restype = [], C.c_char_p

    def Input_before(self, source, snapshot):
        """Audit backend-linked reader bootstrap without hiding native changes.

        Its existing Initialize_Symbol_Tables(FALSE) reserves one empty BLK
        before global-table transfer. Permit exactly that unused appended row;
        preserve the full raw diff and require every other byte to match.
        """
        old, new = source.read_text(), snapshot.read_text()
        diff = ''.join(difflib.unified_diff(old.splitlines(True), new.splitlines(True)))
        snapshot.with_suffix('.input-before.raw.diff').write_text(diff)
        if old == new:
            return True
        begin, end = new.index('Blocks:\n'), new.index('Tcons:\n')
        rows = list(re.finditer(r'^\[(\d+)\]: size 0, align 0, flags 0x0000, section 0, scninfo 0\n', new[begin:end], re.M))
        previous = list(re.finditer(r'^\[(\d+)\]: size 0, align 0, flags 0x0000, section 0, scninfo 0\n', old[old.index('Blocks:\n'):old.index('Tcons:\n')], re.M))
        old_ids = re.findall(r'^\[(\d+)\]:', old[old.index('Blocks:\n'):old.index('Tcons:\n')], re.M)
        last = max(map(int, old_ids)) if old_ids else 0
        assert len(rows) == len(previous)+1 and int(rows[-1][1]) == last+1
        row = rows[-1]
        assert new[:begin+row.start()]+new[begin+row.end():] == old
        snapshot.with_suffix('.bootstrap.json').write_text(json.dumps(dict(change='one existing reader-bootstrap empty BLK row', row=row[0].strip(), other_full_trace_bytes_identical=True), indent=2)+'\n')
        return True

    def Events(self, c, plan, layout, events):
        """Independent full-source geometry oracle checks actual executed stages."""
        g = c.schema['geometry']
        groups = g['B']*g['Q']*((g['H']+plan['group']-1)//plan['group'])
        tiles = (g['L']+plan['width']-1)//plan['width']
        count = groups*3*tiles
        assert events['quiescent']
        for key in ('acquired', 'waited', 'published', 'consumed', 'released'):
            assert events[key] == count, (key, events, count)
        assert events['ended'] == groups*layout['raw_slots']
        expected_bytes = groups*3*g['L']*sum(s['row_bytes'] for s in layout['streams'])
        assert events['delayed_bytes'] == (expected_bytes if layout['delivery'] == 2 else 0)
        assert events['sync_bytes'] == (expected_bytes if layout['delivery'] == 1 else 0)
        assert events['prefetch_while_current_owned'] == (groups*3*(tiles-1) if layout['raw_slots'] == 2 else 0)
        assert events['max_live_raw_slots'] == (2 if tiles > 1 and layout['raw_slots'] == 2 else 1)

    def Control(self, plan, delivery):
        """Use the same explicit supported envelope as the accepted Q6a review."""
        return dict(schema='open64.q6.placement-control.v1', scope='offline-selected-contract', q3_binding_key=plan['binding_key'],
                    delivery=delivery, arch=90, ptx=806, toolkit=12080, max_threads=1024, shared_limit=48*1024)

    def Bad_inputs(self, c):
        """Reject source guards before staging begins; preserve existing fallback."""
        if c.case.startswith(('dense', 'fake')):
            for value in (float('nan'), float('inf')):
                bad = [x.clone() for x in c.inputs]
                bad[2].reshape(-1)[0] = value
                yield bad
        if c.schema['paged']:
            for value in (-1, 999999, 2**63-1):
                bad = [x.clone() for x in c.inputs]
                bad[6][0, 0] = value
                yield bad
        if c.case.startswith('append'):
            bad = [x.clone() for x in c.inputs]
            bad[12][1, 0] = bad[12][0, 0]
            yield bad

    def Report(self):
        """Record the exact implemented High boundary without a GPU claim."""
        (self.out/'q6b.report.json').write_text(json.dumps(dict(status='PASS', cases=self.stage_cases, native_high_staging=True,
             two_phase_search=True, transport_scope='explicit qualification fixtures, no timing-based selection',
             gpu_execution=False, device_bindings=False, aot_selection='deferred'), indent=2)+'\n')
