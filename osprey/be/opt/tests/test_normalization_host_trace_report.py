"""Focused refusal coverage for compact LN4 host-dispatch joins."""
import copy
import unittest
from normalization_host_trace_report import audit
from normalization_host_mode_evidence import body


class HostTraceTest(unittest.TestCase):
    """Mode/identity/source claims must agree even when other phases pass."""
    def setUp(self):
        """Minimal complete positional host observation, not native ABI evidence."""
        self.m=dict(execution_mode='standalone',host_dispatch='open64_ln_dispatch_2_3_c1',kernel='kernel',
                    plan_seal=17,tree_seal=23,selected_kind=2,request=dict(source=dict(owner_pu=2,node_id=3,source_line=51)))
        self.t='AI_NORM_HOST 1 DISPATCH standalone open64_ln_dispatch_2_3_c1 kernel 17 23 51\n'
        self.c='int open64_ln_dispatch_2_3_c1(void *a,void *b) { int dispatch_status = open64_normalization_execution_run_selected(a,17ULL,23ULL,b); return dispatch_status; }'

    def test_valid_standalone(self):
        """One exact compiler observation binds host source to the selected body."""
        self.assertEqual(audit(self.t,self.m,self.c)['mode'],'standalone')

    def test_valid_service(self):
        """Service absence is explicit, not a skipped host transformation."""
        m=copy.deepcopy(self.m);m.update(execution_mode='service',host_dispatch='')
        self.assertEqual(audit('AI_NORM_HOST 1 DISPATCH service none kernel 17 23 51\n',m)['host'],'')

    def test_each_column_is_bound(self):
        """Every positional column participates in identity, including source line."""
        for index in range(9):
            words=self.t.split();words[index]='wrong'
            with self.subTest(column=index),self.assertRaises(ValueError):audit(' '.join(words),self.m,self.c)

    def test_missing_duplicate_and_mode(self):
        """Incomplete/ambiguous records cannot inherit success from the device gate."""
        for text in ('',self.t+self.t):
            with self.assertRaises(ValueError):audit(text,self.m,self.c)
        m=copy.deepcopy(self.m);m['execution_mode']='performance'
        with self.assertRaises(ValueError):audit(self.t,m,self.c)

    def test_emitted_contract(self):
        """Missing/mismatched constants, duplicate calls and lost returns refuse."""
        for source in (self.c.replace('17ULL','18ULL'),self.c.replace('return dispatch_status;',''),
                       self.c+self.c,self.c+' __global__'):
            with self.subTest(source=source),self.assertRaises(ValueError):audit(self.t,self.m,source)
        prefix='/*******************************************************\n * C file translated from WHIRL Thu Oct 29 22:41:09 2026\n *******************************************************/\n'
        self.assertEqual(body(prefix+self.c),body(prefix.replace('22:41:09','23:00:01')+self.c))
        self.assertNotEqual(body(prefix+self.c),body(prefix+self.c.replace('17ULL','18ULL')))
        with self.assertRaises(ValueError):body(prefix.replace('Thu','*/INJECT/*')+self.c)


if __name__=='__main__':unittest.main()
