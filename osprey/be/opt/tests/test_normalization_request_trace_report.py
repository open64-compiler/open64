"""Synthetic request/report joins, distinct from actual native driver evidence.
Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md.
"""
import copy
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
from normalization_request_trace_report import audit,refusal_reports
from test_normalization_lower_trace_report import fixture


def request_fixture(kind):
    """Attach a synthetic driver lifecycle to the existing synthetic LN4 rows."""
    text,reopened=fixture(kind)
    cost=int(next(s for s in text.splitlines() if s.startswith('AI_NORM_LOWER 1 PREPARE ')).split()[-2])
    m=dict(schema='open64.layernorm.driver-review.v1',request=dict(source=dict(owner_pu=11,node_id=7,source_line=211),
        target=dict(architecture='sm_90'),profile=dict(model_id=1002,description_revision=2)),kernel='driver_ln_kernel',
        selected_kind=kind,plan_seal=42,tree_seal=99,cost=cost,runtime_authorized=False,gpu_verified=False)
    text+='AI_NORM_REQUEST 1 BIND 11 7 sm_90 1002 2 211\n'
    text+=f'AI_NORM_REQUEST 1 SELECT 11 7 {kind} 42 {cost} 211\n'
    text+='AI_NORM_REQUEST 1 PUBLISH 11 7 driver_ln_kernel 99 /review/selected.B 211\n'
    return text,m,reopened


class NormalizationRequestTests(unittest.TestCase):
    """Strict request/selection/body joins and explicit runtime evidence limits."""
    @patch('normalization_request_trace_report.cuda_structure')
    def test_three_joined_plans(self,structure):
        """A driver-specific entry name is passed to the actual CUDA auditor."""
        for kind in (1,2,3):
            text,m,reopened=request_fixture(kind);r=audit(text,m,reopened,'cuda')
            self.assertEqual(r['kernel_name'],m['kernel']);self.assertEqual(r['guard']['kind'],kind)
            self.assertEqual(structure.call_args[0][1]['kernel_name'],m['kernel'])

    @patch('normalization_request_trace_report.cuda_structure')
    def test_manifest_tampering(self,structure):
        """No foreign profile, choice, identity or runtime grant is accepted."""
        text,m,reopened=request_fixture(3)
        for key,value in (('selected_kind',1),('plan_seal',43),('tree_seal',100),('kernel','foreign'),
                          ('cost',m['cost']+1),('runtime_authorized',True),('gpu_verified',True)):
            changed=copy.deepcopy(m);changed[key]=value
            with self.subTest(key=key),self.assertRaises(ValueError):audit(text,changed,reopened,'cuda')
        changed=copy.deepcopy(m);changed['request']['profile']['model_id']+=1
        with self.assertRaises(ValueError):audit(text,changed,reopened,'cuda')

    @patch('normalization_request_trace_report.cuda_structure')
    def test_missing_duplicate_and_foreign_lifecycle(self,structure):
        """Publication is required and cannot be borrowed from another source."""
        text,m,reopened=request_fixture(2)
        for old,new in (('PUBLISH','SELECT'),('BIND 11 7','BIND 12 7'),('1002 2 211','1002 3 211')):
            with self.subTest(old=old),self.assertRaises(ValueError):audit(text.replace(old,new),m,reopened,'cuda')
        with self.assertRaises(ValueError):audit(text+text.splitlines()[-1]+'\n',m,reopened,'cuda')

    def test_failure_after_cost_selection_is_not_execution(self):
        """A failed publication may follow a real selected/materialized body."""
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);case=root/'case';(case/'logs').mkdir(parents=True)
            (case/'driver.t').write_text(fixture(3)[0]);(case/'logs/driver.stderr').write_text('LN4-DRIVER: publication failed\n')
            command=dict(argv=['be'],cwd=str(case),label='driver',returncode=1)
            self.assertEqual(refusal_reports(root,[command]),1)
            content=(case/'driver.request.log').read_text()
            self.assertIn('materialized=true',content);self.assertIn('selected_kind=3 published=false',content)
            self.assertIn('driver_success=false runtime_authorized=false',content)


if __name__=='__main__':unittest.main()
