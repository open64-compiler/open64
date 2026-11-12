"""Q4 independent ledger oracle on genuine, leased Q3 source invocations.

Every study runs before the existing source callback/reference checks. These
synthetic rates qualify software contracts and do not measure a device.
"""
import copy
import ctypes as C
import json


def request():
    """Supply every model rate/resource explicitly, including cold/warm scenarios."""
    return dict(schema='open64.q4-cost-study-request.v1', architecture='sm_90',
                hardware=[132, 32, 2, 1 << 28, 32, 64, 2048, 1024, 65536, 255, 256, 4,
                          233472, 227328, 128, 0],
                timings=[64, 64, 128, 4, 12, 16, 4, 16, 4, 500, 100, 16],
                hierarchy=[64, 256, 32, 1000000, 1000000, 64, 64, 4],
                scenarios=[['cold', 1, 0, 0], ['warm', 1, 900000, 500000]],
                objective='worst', provenance='synthetic', strategies=['local', 'random', 'exhaustive'],
                stopping=['budget', 'exhausted'], seed=17, proposals=51, unique=17,
                time_us=0, patience=17, target=0, minimum_gain=0)


class Q4_REVIEW:
    """Owned test coordinator; never implements an MLA execution algorithm."""
    def __init__(self, api, out):
        self.out = out
        self.study = api.open64_q4_study
        self.study.argtypes, self.study.restype = [C.c_void_p, C.c_uint64, C.c_char_p], C.c_char_p
        self.validate = api.open64_q4_revalidate
        self.validate.argtypes, self.validate.restype = [C.c_void_p, C.c_uint64, C.c_char_p], C.c_int
        self.prior = {}
        (out/'q4.request.json').write_text(json.dumps(request(), indent=2)+'\n')

    def Before(self, c, check, name=None):
        """Recompute byte counts independently, verify budgets/freshness/isolation."""
        name = name or c.case
        q = request()
        before = [b.raw for b in c.buffers]
        calls = c.callback_count
        raw = self.study(c.session, c.ticket, json.dumps(q).encode())
        assert raw, c.error()
        d = json.loads(raw)
        (self.out/(name+'.q4.json')).write_text(json.dumps(d, indent=2)+'\n')
        check(name+' Q4 deterministic detached study', raw == self.study(c.session, c.ticket, json.dumps(q).encode()))
        check(name+' Q4 no native-buffer or callback changes', calls == c.callback_count and before == [b.raw for b in c.buffers])
        check(name+' Q4 exact finite census', d['complete'] and d['evaluations'] == 17 and len(d['candidates']) == 17)
        check(name+' Q4 shared budget/cache accounting', d['proposals'] == d['evaluations']+d['cache_hits'] and d['proposals'] <= 51)
        check(name+' Q4 live freshness', self.validate(c.session, c.ticket, d['snapshot_key'].encode()))
        check(name+' Q4 tampered snapshot refused', not self.validate(c.session, c.ticket, (d['snapshot_key']+'x').encode()))
        g = c.schema['geometry']
        b, h, qc, l = (g[n] for n in ('B', 'H', 'Q', 'L'))
        # Q1 fixtures share R=16 and Ro=4, both independently in fixture metadata.
        r, ro = c.meta['latent'], c.meta['rotary']
        compression = 0.5 if 'u4' in c.case else 4 if c.meta['fake'] else 1
        physical = set()
        if c.schema['paged']:
            f = json.loads((self.out/(c.case+'.facts.json')).read_text())
            for p in f['pages']:
                physical.update((p['physical_page'], t) for t in range(p['capacity_tokens']))
        for x in d['candidates']:
            group, width = x['group'], x['width']
            repeats = qc*((h+group-1)//group) if width else 1
            check(name+' ledger '+str(x['id']),
                  x['model_eligible'] and not x['application_eligible']
                  and x['packed_bytes'] == b*l*(r+ro)*compression*repeats
                  and x['scale_bytes'] == b*l*8*repeats
                  and x['conversions'] == b*l*(r+ro)*repeats
                  and x['unpacks'] == (b*l*(r+ro)*repeats if 'u4' in c.case else 0)
                  and x['logical_capacity_tokens'] == b*l
                  and x['unique_physical_tokens'] == (len(physical) if c.schema['paged'] else b*l))
        check(name+' Q4 fused global decoded buffers removed', all(x['decoded_global_bytes'] == 0 for x in d['candidates'][1:]))
        check(name+' Q4 grouping reuses decoded cache', d['candidates'][13]['packed_bytes'] <= d['candidates'][1]['packed_bytes'])
        if c.schema['paged']:
            check(name+' Q4 prefix not free reuse', len(physical) < b*l and d['candidates'][1]['logical_capacity_tokens'] == b*l)
        capped = copy.deepcopy(q)
        capped.update(unique=1, proposals=2)
        limited = json.loads(self.study(c.session, c.ticket, json.dumps(capped).encode()))
        check(name+' Q4 hard unique budget', limited['evaluations'] == 1 and not limited['complete'])
        q2 = copy.deepcopy(q)
        q2['hierarchy'][5] += 1
        changed = json.loads(self.study(c.session, c.ticket, json.dumps(q2).encode()))
        check(name+' Q4 same-ID rate binding', changed['candidates'][0]['model_key'] != d['candidates'][0]['model_key'])
        for field, value in [('hierarchy', [0]+q['hierarchy'][1:]),
                             ('stopping', ['relative-gap']), ('strategies', ['unknown'])]:
            bad = copy.deepcopy(q)
            bad[field] = value
            check(name+' Q4 refusal '+field, not self.study(c.session, c.ticket, json.dumps(bad).encode()))
        check(name+' Q4 refusal preserves source ticket', self.validate(c.session, c.ticket, d['snapshot_key'].encode()))
        # An uncooperative raw control edit is a negative test: valid enum,
        # different actual intent, no registry version update. Restore afterward.
        address = c.address(c.metadata)
        old = C.string_at(address, 8)
        phase = int.from_bytes(old, 'little', signed=True)
        C.memmove(address, (1 if phase == 0 else 0).to_bytes(8, 'little', signed=True), 8)
        check(name+' Q4 changed actual controls refused', not self.study(c.session, c.ticket, json.dumps(q).encode()))
        C.memmove(address, old, 8)
        check(name+' Q4 restored live controls validate', self.validate(c.session, c.ticket, d['snapshot_key'].encode()))
        self.prior[name] = d['snapshot_key']

    def Expired(self, c, check, name=None):
        """Expired cost facts cannot revive a completed/cancelled Q3 ticket."""
        name = name or c.case
        check(name+' Q4 expired study refused', not self.study(c.session, c.ticket, json.dumps(request()).encode()))
        check(name+' Q4 expired revalidation refused', not self.validate(c.session, c.ticket, self.prior[name].encode()))
