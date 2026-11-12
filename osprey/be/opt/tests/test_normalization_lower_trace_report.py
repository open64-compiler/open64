"""Synthetic LN4 schema/refusal tests; native/assembly evidence is separate.
Design: doc/AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md.
"""
from functools import lru_cache
import unittest
from normalization_lower_trace_report import audit,comparison
from normalization_lower_host import datasets,protocol,audit_output
from normalization_cost_trace_report import audit as cost_audit,COST,SELECT
from normalization_guard_trace_report import FIELDS
from test_normalization_cost_trace_report import synthetic
from test_normalization_guard_trace_report import export_fixture


@lru_cache(None)
def fixture(kind):
    """Explicit synthetic lowering rows; not a compiler execution transcript."""
    text,_,_,_=synthetic();name='shared_slow' if kind==1 else 'global_slow'
    chunk=next(b for b in text.split('AI_NORM_EXPERIMENT 1 CASE ') if b.startswith(name+' '))
    prefix='AI_NORM_EXPERIMENT 1 CASE '+chunk
    costs=[dict(zip(COST,map(int,s.split()[3:]))) for s in prefix.splitlines() if s.startswith('AI_NORM_COST 1 COST ')]
    selection=next(s for s in prefix.splitlines() if s.startswith('AI_NORM_SELECT 1 SELECT '))
    fields=dict(zip(SELECT,map(int,selection.split()[3:])));fields['selected']=fields['nominal']=kind;fields['common_plan']=kind
    prefix=prefix.replace(selection,'AI_NORM_SELECT 1 SELECT '+' '.join(str(fields[k]) for k in SELECT))
    guard,_,_,_=export_fixture(kind)
    old=next(s for s in prefix.splitlines() if s.startswith('AI_NORM_CHOICE_GUARD 1 EXPORT '))
    prefix=prefix.replace(old,'AI_NORM_CHOICE_GUARD 1 EXPORT 83 '+' '.join(str(guard[k]) for k in FIELDS)+' 211')
    prepare=f'AI_NORM_LOWER 1 PREPARE 11 7 {kind} 42 {costs[kind-1]["total"]} 211\n'
    facts=f'11 7 {kind} 42 99 400 {3 if kind==1 else 4 if kind==2 else 5} {13 if kind==1 else 12} {0 if kind==1 else 4 if kind==2 else 5} {guard["reserved_shared"]} {guard["threads"]} 211'
    actions=['FORMALS_SUBRANGE_X_GAMMA_BETA_Y','PRIVATE_CARRIER_3_F32']
    if kind>1:actions+=['SHARED_ARENA']
    if kind==3:actions+=['X_COPY_TO_SHARED','ROW_VISIBLE_BARRIER']
    actions+=['MEAN_ASCENDING_F32']
    if kind>1:actions+=['MU_PUBLISH_BARRIER','CENTER_FROM_X' if kind==2 else 'CENTER_X_TO_D_IN_PLACE','CENTER_VISIBLE_BARRIER']
    actions+=['VARIANCE_ASCENDING_F32']
    if kind>1:actions+=['Q_PUBLISH_BARRIER']
    actions+=['AFFINE_POINTWISE_F32']
    if kind>1:actions+=['READERS_RELEASE_BARRIER']
    rows=''.join(f'AI_NORM_LOWER 1 ACTION {kind} {a} {guard["reserved_shared"] if a=="SHARED_ARENA" else 0} 211\n' for a in actions)
    reopened='AI_NORM_LOWER 1 VERIFY '+facts+'\n'
    return prefix+prepare+'AI_NORM_LOWER 1 MATERIALIZE '+facts+'\n'+rows+reopened,reopened


class NormalizationLowerTests(unittest.TestCase):
    """Test exact joins, declined lowering and explicit host evidence boundaries."""
    def test_three_materializations(self):
        """Every selected kind has distinct loop, barrier and storage actions."""
        for kind in (1,2,3):
            text,reopened=fixture(kind);r=audit(text,reopened)
            self.assertTrue(r['materialized']);self.assertEqual(r['guard']['kind'],kind)

    def test_unknown_does_not_materialize(self):
        """Missing cost fields cannot be promoted by appending lowering rows."""
        text=synthetic()[0].split('AI_NORM_EXPERIMENT 1 CASE balanced')[0]
        self.assertFalse(audit(text)['materialized'])
        with self.assertRaises(ValueError):audit(text+'AI_NORM_LOWER 1 PREPARE 11 7 1 42 0 211\n')

    def test_action_and_identity_tampering(self):
        """Missing/reordered barriers, stale guards and changed bodies refuse."""
        text,reopened=fixture(3)
        for old,new in (('ROW_VISIBLE_BARRIER','READERS_RELEASE_BARRIER'),('MATERIALIZE','VERIFY'),
                        ('CENTER_X_TO_D_IN_PLACE','CENTER_FROM_X'),('PRIVATE_CARRIER_3_F32','IMPLICIT_CARRIER'),
                        ('EXPORT 83','EXPORT 84'),('42 99 400','42 100 400')):
            with self.subTest(old=old),self.assertRaises(ValueError):audit(text.replace(old,new),reopened)
        with self.assertRaises(ValueError):audit(text,reopened.replace('99','100'))
        with self.assertRaises(ValueError):audit(text+reopened,reopened)

    def test_scoped_cost_audit_preserves_default_census(self):
        """Explicit one-member callers do not weaken the default full-matrix audit."""
        text,plan,source,images=synthetic();prefix=comparison(fixture(1)[0])
        self.assertEqual(len(cost_audit(prefix,plan,source,images,scenarios=('shared_slow',),source_keys=((11,7,5),))),1)
        with self.assertRaises(ValueError):cost_audit(prefix,plan,source,images)
        with self.assertRaises(ValueError):cost_audit(prefix,plan,source,images,scenarios=())
        with self.assertRaises(ValueError):cost_audit(prefix,plan,source,images,scenarios=('shared_slow',),source_keys=((99,7,5),))

    def test_host_oracle_protocol(self):
        """Exact hex inputs and every output are checked independently of host code."""
        from normalization_oracle import reference_row
        r=audit(*fixture(2));sets=datasets(r);g=r['guard'];lines=[]
        for x,gamma,beta in sets:
            values=[]
            for row in range(g['rows']):values+=reference_row(x[row*g['width']:(row+1)*g['width']],gamma,beta,g['epsilon_bits'])['y']
            lines.append(' '.join(f'{v:08x}' for v in values))
        output='\n'.join(lines)+'\n'
        self.assertEqual(audit_output(r,sets,output),len(sets)*g['rows']*g['width'])
        self.assertTrue(protocol(r,sets).startswith('8\n'))
        with self.assertRaises(ValueError):audit_output(r,sets,'00000000 '+output)


if __name__=='__main__':unittest.main()
