"""Q5 selected-only native High construction and Q3-bound reference execution.

Exploration finishes before invoking a compiler. Only the one selected complete
plan is materialized; legacy Q3 tests still use their original source callbacks.
"""
import ctypes as C
import difflib
import hashlib
import json
from pathlib import Path
import subprocess
from mla_q4_costing_review import request


class Q5_REVIEW:
    """Own selected artifact qualification, outside the native estimator."""
    def __init__(self, api, args, out):
        self.args, self.out, self.api = args, out, api
        self.commands, self.cases = [], {}
        self.study = api.open64_q5_study
        self.study.argtypes, self.study.restype = [C.c_void_p, C.c_uint64, C.c_char_p, C.c_uint64], C.c_char_p
        from run_mla_serving_review import CALLBACK
        self.run = api.open64_q5_run
        self.run.argtypes = [C.c_void_p, C.c_uint64, C.c_char_p, C.c_char_p, CALLBACK, C.c_void_p, C.POINTER(C.c_uint64)]
        self.run.restype = C.c_int
        self.contract = api.open64_q5_contract_test
        self.contract.argtypes, self.contract.restype = [C.c_void_p], C.c_char_p
        self.tools = args.lowerer.parent.parent

    def Command(self, label, argv, cwd, success=True):
        """Retain exact commands and negative no-publication checks."""
        p = subprocess.run(list(map(str, argv)), capture_output=True, text=True, cwd=cwd, timeout=300)
        (cwd/(label+'.stdout')).write_text(p.stdout)
        (cwd/(label+'.stderr')).write_text(p.stderr)
        self.commands.append(dict(label=label, argv=list(map(str, argv)), returncode=p.returncode))
        (self.out/'q5.commands.json').write_text(json.dumps(self.commands, indent=2)+'\n')
        assert (p.returncode == 0) == success, (label, p.returncode, p.stderr[-4000:])
        return p

    def Before(self, c, check, name=None):
        """Freeze, explore, qualify exactly one native plan, then bind its entry."""
        name = name or c.case
        case = self.out/(name+'.q5')
        case.mkdir()
        contract = self.contract(c.session)
        assert contract, c.error()
        (case/'contract.json').write_text(json.dumps(json.loads(contract), indent=2)+'\n')
        check(name+' Q5 detached contract negatives', json.loads(contract)['source_unchanged'])
        q = request()
        # Explicit synthetic bandwidth-bound scenario; no GPU measurement.
        q['hierarchy'][0] = 8
        if self.args.q5_long:
            # Separate explicit synthetic latency envelope for multi-tile qualification.
            q['timings'][3] = 1
        (case/'request.json').write_text(json.dumps(q, indent=2)+'\n')
        wire = json.dumps(q).encode()
        buffers = [b.raw for b in c.buffers]
        raw = self.study(c.session, c.ticket, wire, 16)
        assert raw, c.error()
        d = json.loads(raw)
        check(name+' Q5 deterministic estimation', raw == self.study(c.session, c.ticket, wire, 16))
        check(name+' Q5 native-buffer isolation', buffers == [b.raw for b in c.buffers] and c.callback_count == 0)
        check(name+' Q5 full model catalog', d['complete'] and d['evaluations'] == 17 and d['scope'].startswith('complete source'))
        check(name+' Q5 missing log timing refuses', not self.study(c.session, c.ticket, wire, 0))
        (case/'study.json').write_text(json.dumps(d, indent=2)+'\n')
        assert d['has_model_winner'] and not d['gpu_execution_eligible']
        plan = json.loads(d['selected_plan'])
        source_image = c.prior/'q1.high.after.B'
        self.Command('input-key', [self.args.lowerer, 'q5-key', source_image, case/'input.keys.json'], case)
        key = json.loads((case/'input.keys.json').read_text())
        plan['native_key'] = key['native_key']
        assert plan['source_key'] == key['source_key']
        plan_wire = json.dumps(plan, separators=(',', ':'), ensure_ascii=False).encode()
        (case/'selected.plan.json').write_bytes(plan_wire)
        bad = dict(plan)
        bad['native_key'] += 'x'
        (case/'stale.plan.json').write_text(json.dumps(bad, separators=(',', ':'), ensure_ascii=False))
        self.Command('stale-refusal', [self.args.lowerer, 'q5-apply', source_image, case/'stale.plan.json', case/'stale.before.B', case/'stale.after.B', case/'stale.manifest.json', '--opt-level=3'], case, False)
        check(name+' Q5 stale request creates no candidate', not (case/'stale.before.B').exists() and not (case/'stale.after.B').exists())
        self.Command('O0-refusal', [self.args.lowerer, 'q5-apply', source_image, case/'selected.plan.json', case/'O0.before.B', case/'O0.after.B', case/'O0.manifest.json', '--opt-level=0'], case, False)
        check(name+' Q5 O0 preserves source', not (case/'O0.before.B').exists() and not (case/'O0.after.B').exists())
        self.Command('selected-apply', [self.args.lowerer, 'q5-apply', source_image, case/'selected.plan.json', case/'selected.before.B', case/'selected.after.B', case/'selected.manifest.json', '--opt-level=3'], case)
        manifest = json.loads((case/'selected.manifest.json').read_text())
        check(name+' Q5 exactly one materialization', manifest['candidates_materialized'] == 1 and manifest['exploration_native_mutations'] == 0 and manifest['old_facts_expired'])
        for image in (case/'selected.before.B', case/'selected.after.B'):
            self.Command(image.stem+'-dump', [self.tools/'ir_tools/ir_b2a', '-st', '-src', image, image.with_suffix('.T')], case)
        (case/'selected.before-after.diff').write_text(''.join(difflib.unified_diff((case/'selected.before.T').read_text().splitlines(True), (case/'selected.after.T').read_text().splitlines(True))))
        if c.meta['fake']:
            check(name+' Q5 floating control fallback', plan['width'] == 0)
        backend = self.tools/'be/be'
        self.Command('backend', [backend, '-fB,'+str(case/'selected.after.B'), '-fo,'+str(case/'native.o'), '-ft,'+str(case/'backend.phase.t'),
                     '-PHASE:l=on:w=off:c=off:lpath='+str(self.tools/'lno')+':wpath='+str(self.tools/'wopt'), '-O3', '-LNO:simd=0:vintr=0',
                     '-OPT:IEEE_arith=1:roundoff=0', '-DSL:q1_checkpoint='+str(case/'q5')+':ai_opt_trace=on', '-tr26', '-tt26:1',
                     self.args.source/'osprey/torch2whirl/python/tests/models/mla_quantized_paged.py'], case)
        for image in (case/'q5.preopt-0.after.B', case/'native.N'):
            self.Command(image.stem+'-dump', [self.tools/'ir_tools/ir_b2a', '-st', '-src', image, image.with_suffix('.T')], case)
        # Both semantic High and the normal backend's final memory form execute.
        for image in (case/'selected.after.B', case/'native.N'):
            source = image.with_suffix('.c')
            self.Command(image.stem+'-emit', [self.tools/'whirl2c/whirl2c', '-TARG:abi=n64', '-O0', '-ft,'+str(case/(image.stem+'.w2c.phase.t')),
                         '-CLIST:dotc_file='+str(source), '-CLIST:doth_file='+str(source.with_suffix('.h')), image], case)
            self.Command(image.stem+'-cc', ['gcc', '-std=c99', '-O2', '-shared', '-fPIC', '-ffp-contract=off', '-fno-fast-math',
                         '-I'+str(self.args.source/'osprey/include'), '-include', self.args.source/'osprey/be/lno/tests/common_high_host.h', source, '-lm', '-o', source.with_suffix('.so')], case)
            check(name+' Q5 COMMENT in '+image.stem, 'Q5 selected quantized MLA:' in source.read_text() and image.with_suffix('.T').stat().st_size > 1000)
        from run_quantized_high_review import execute
        expected = c.model(*c.inputs)
        import torch
        original = execute(c.prior/'q1.high.after.so', c.manifest, c.inputs)
        for image in ('selected.after', 'native'):
            actual = execute(case/(image+'.so'), manifest, c.inputs)
            for i, (a, b) in enumerate(zip(actual, expected)):
                torch.testing.assert_close(a, b, rtol=2e-5 if i < 2 else 0, atol=2e-6 if i < 2 else 0, equal_nan=True)
                torch.testing.assert_close(a, original[i], rtol=0, atol=0, equal_nan=True)
        # Guards and nonfinite behavior are qualified after selection, never
        # by lowering a collection of candidates during estimation.
        for image in ('selected.after', 'native'):
            if c.case.startswith(('dense', 'fake')):
                for bad_value in (float('nan'), float('inf')):
                    bad = [x.clone() for x in c.inputs]
                    bad[2].reshape(-1)[0] = bad_value
                    execute(case/(image+'.so'), manifest, bad, 1)
            if c.schema['paged']:
                for bad_value in (-1, 999999, 2**63-1):
                    bad = [x.clone() for x in c.inputs]
                    bad[6][0, 0] = bad_value
                    execute(case/(image+'.so'), manifest, bad, 1)
                bad = [x.clone() for x in c.inputs]
                bad[3].fill_(torch.finfo(torch.float32).max)
                bad[9].zero_()
                source = execute(c.prior/'q1.high.after.so', c.manifest, bad)
                actual = execute(case/(image+'.so'), manifest, bad)
                for a, b in zip(actual, source):
                    torch.testing.assert_close(a, b, rtol=2e-5, atol=2e-6, equal_nan=True)
            if c.case.startswith('append'):
                bad = [x.clone() for x in c.inputs]
                bad[12][1, 0] = bad[12][0, 0]
                execute(case/(image+'.so'), manifest, bad, 1)
        check(name+' Q5 selected guard/nonfinite qualification', True)
        library = C.CDLL(str(case/'native.so'))
        c.function = getattr(library, manifest['function'])
        c.function.argtypes, c.function.restype = [C.c_void_p]*len(c.views), None
        # Keep the loaded library alive for the callback's full Q3 lease.
        c.q5_library, c.q5_plan = library, plan_wire
        snapshot = d['snapshot_key'].encode()
        bundle = C.c_uint64()
        check(name+' Q5 wrong snapshot join refuses', not self.run(c.session, c.ticket, plan_wire, snapshot+b'x', c.callback, None, C.byref(bundle)) and c.callback_count == 0)
        def selected_run(callback=None, ticket=None):
            b = C.c_uint64()
            ok = self.run(c.session, c.ticket if ticket is None else ticket, plan_wire, snapshot,
                          callback or c.callback, None, C.byref(b))
            return bool(ok), b.value, c.error()
        c.run = selected_run
        self.cases[name] = dict(width=plan['width'], group=plan['group'], candidates_materialized=1, reference_outputs=12, strict_f32=True, q3_snapshot_join=True)
        (self.out/'q5.report.json').write_text(json.dumps(dict(status='PASS', cases=self.cases, gpu_execution=False, aot_selection='deferred'), indent=2)+'\n')
