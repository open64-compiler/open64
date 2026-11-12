"""Offline resource-aware MLA selection, policy audit and native reference checks."""
import json
import os
import unittest
import test_tensor_attention_fusion as fusion


def gpu_profile(sms=132,registers=65536,shared=65536,scratch=1048576):
    """Explicit review envelope; never infer a real GPU's SM count from sm_90."""
    return dict(schema='open64.mla-gpu-profile.v1',id='sm90-review-estimate-v1',architecture='sm_90',
                provenance='provided-capacities-estimated-registers',sms=sms,
                registers_per_thread_estimate=48,target_ctas_per_sm=2,scratch_budget_bytes=scratch,
                resources=dict(max_blocks=32,max_warps=64,max_threads=2048,max_threads_per_cta=1024,
                               registers_per_sm=registers,max_registers_per_thread=255,register_quantum=256,
                               register_warp_quantum=4,shared_capacity=233472,shared_budget=shared,
                               shared_quantum=128,shared_reserved=0))


@unittest.skipUnless(os.environ.get('OPEN64_TENSOR_LOWERER'),'native lowering tools are not configured')
class AutoChunkTests(unittest.TestCase):
    setUp=fusion.AttentionFusionTests.setUp
    run_tool=fusion.AttentionFusionTests.run_tool
    fixture=fusion.AttentionFusionTests.fixture
    execute=fusion.AttentionFusionTests.execute

    def attention(self,length=8193):
        previous=self.fixture(length,split=128 if length>4096 else 0)
        out=self.root/'attention.json'
        self.run_tool(self.lowerer,'fuse-attention',previous,out,'--allow-reassociate')
        return out

    def select(self,source,profile,label,success=True,policy=None):
        path=self.root/(label+'.profile.json');path.write_text(json.dumps(profile))
        out=self.root/(label+'.json')
        self.run_tool(self.lowerer,'auto-attention',source,out,'--gpu-profile',path,'--allow-reassociate',
                      *(['--chunk-policy',policy] if policy else []),success=success)
        if not success:
            self.assertFalse(out.exists());return None
        return json.loads(out.read_text())

    def test_auto_short_native_reference(self):
        j=self.execute(65,gpu=gpu_profile())
        self.assertEqual(j['split_width'],0);self.assertTrue(j['chunk_selection']['short_direct'])

    def test_auto_short_resource_limit_forces_legal_split(self):
        j=self.execute(256,gpu=gpu_profile(sms=16,shared=1024))
        self.assertEqual(j['split_width'],32);self.assertFalse(j['chunk_selection']['short_direct'])

    def test_auto_long_native_reference(self):
        j=self.execute(513,gpu=gpu_profile(sms=16))
        self.assertEqual(j['split_width'],64)

    def test_auto_width32_native_reference(self):
        j=self.execute(513,gpu=gpu_profile())
        self.assertEqual(j['split_width'],32)
        self.assertEqual(j['chunk_selection']['policy'],'coverage-v3')

    def test_legacy_policy_remains_selectable_and_stable(self):
        source=self.attention()
        old=self.select(source,gpu_profile(),'legacy',policy='coverage-v1')
        new=self.select(source,gpu_profile(),'expanded')
        self.assertEqual(old['split_width'],128);self.assertEqual(new['split_width'],64)
        self.assertEqual(old['chunk_selection']['policy'],'coverage-v1')
        self.assertEqual([x['width'] for x in old['chunk_selection']['candidates']],[128,256,512,1024,2048,4096])
        self.assertEqual([x['width'] for x in new['chunk_selection']['candidates']],[32,64,128,256,512,1024,2048,4096])
        self.assertEqual(old,self.select(source,gpu_profile(),'legacy-again',policy='coverage-v1'))
        self.select(source,gpu_profile(),'unknown-policy',success=False,policy='coverage-v999')
        v2=self.select(source,gpu_profile(),'expanded-legacy',policy='coverage-v2')
        self.assertEqual(v2['split_width'],64)
        self.assertNotIn('block_x',v2['chunk_selection']['candidates'][0])
        self.assertEqual(v2,self.select(source,gpu_profile(),'expanded-legacy-again',policy='coverage-v2'))
        selected=next(x for x in new['chunk_selection']['candidates'] if x['width']==64)
        narrower=next(x for x in new['chunk_selection']['candidates'] if x['width']==32)
        self.assertEqual(selected['coverage_ctas'],selected['target_ctas'])
        self.assertEqual(narrower['coverage_ctas'],narrower['target_ctas'])
        self.assertLess(selected['chunks'],narrower['chunks'])

    def test_auto_larger_chunks_native_reference(self):
        j=self.execute(513,gpu=gpu_profile(sms=4))
        self.assertEqual(j['split_width'],512)

    def test_context_and_device_resource_sensitivity(self):
        source=self.attention()
        cases=[('large-device',gpu_profile(),64),
               ('small-device',gpu_profile(sms=8),2048),
               ('register-limited',gpu_profile(sms=8,registers=8192),4096),
               ('shared-limited',gpu_profile(sms=8,shared=1024),128),
               ('scratch-limited',gpu_profile(scratch=4096),256)]
        for name,g,w in cases:
            j=self.select(source,g,name);self.assertEqual(j['split_width'],w,name)
            rows=j['chunk_selection']['candidates']
            self.assertTrue(any(c['eligible'] and c['width']==w for c in rows))
            if name=='scratch-limited':self.assertTrue(any(c['reason']=='scratch_budget' for c in rows))
            if name=='shared-limited':
                self.assertTrue(any(c['reason']=='resource_fit' for c in rows))
                for c in rows:
                    if c['width'] in (32,64):
                        self.assertLessEqual(c['local_shared_bytes'],1024)
                        self.assertGreater(c['merge_shared_bytes'],1024)
                        self.assertEqual(c['reason'],'resource_fit')
        again=self.select(source,gpu_profile(),'deterministic')
        self.assertEqual(again,json.loads((self.root/'large-device.json').read_text()))
        # A valid v6 choice is overridden explicitly, clearing automatic policy.
        override=self.root/'manual.json'
        self.run_tool(self.lowerer,'stream-attention',self.root/'large-device.json',override,
                      '--split-width',256,'--allow-reassociate')
        j=json.loads(override.read_text());self.assertEqual(j['schema'],'open64.tensor-kernel.v5')
        self.assertNotIn('chunk_selection',j);self.assertEqual(j['split_width'],256)

    def test_incomplete_unknown_and_infeasible_resources_reject(self):
        source=self.attention()
        profiles=[gpu_profile(sms=0),gpu_profile(registers=4096),gpu_profile(scratch=0)]
        missing=gpu_profile();del missing['resources']['register_quantum'];profiles.append(missing)
        unknown=gpu_profile();unknown['architecture']='sm_999';profiles.append(unknown)
        malformed=gpu_profile();malformed['registers_per_thread_estimate']='48';profiles.append(malformed)
        overflow=gpu_profile();overflow['scratch_budget_bytes']=1<<63;profiles.append(overflow)
        for i,g in enumerate(profiles):self.select(source,g,'reject'+str(i),success=False)
        path=self.root/'permission.profile.json';path.write_text(json.dumps(gpu_profile()))
        out=self.root/'deny.json'
        self.run_tool(self.lowerer,'auto-attention',source,out,'--gpu-profile',path,'--deny-reassociate',success=False)
        self.assertFalse(out.exists())

    def test_stale_winner_and_forged_candidate_ledger_reject(self):
        source=self.attention();self.select(source,gpu_profile(),'selected')
        text=(self.root/'selected.json').read_text()
        changes=[('"split_width" : 64','"split_width" : 128'),
                 ('"policy" : "coverage-v3"','"policy" : "unknown"'),
                 ('"ctas" : 260','"ctas" : 261'),
                 ('"block_x" : 32','"block_x" : 64')]
        for i,(old,new) in enumerate(changes):
            self.assertIn(old,text)
            bad=self.root/('stale%d.json'%i);bad.write_text(fusion.reseal(text.replace(old,new,1)))
            out=self.root/('stale%d.cu'%i)
            self.run_tool(self.lowerer,'schedule-source',bad,out,success=False);self.assertFalse(out.exists())

    def test_width32_wide_column_tail(self):
        j=self.execute(513,variant='wide',gpu=gpu_profile())
        self.assertEqual(j['split_width'],32)

    def test_width64_empty_negative_chunks(self):
        j=self.execute(513,variant='empty-negative',gpu=gpu_profile(sms=16))
        self.assertEqual(j['split_width'],64)

    def test_width32_large_finite_values(self):
        j=self.execute(513,variant='large-values',gpu=gpu_profile())
        self.assertEqual(j['split_width'],32)

    def test_lower_optimization_level_rejects_automatic_selection(self):
        source=self.fixture(level=0)
        self.select(source,gpu_profile(),'O0',success=False)


if __name__=='__main__':unittest.main()
