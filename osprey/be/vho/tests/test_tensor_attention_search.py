"""Estimation-only exploration, strategy stops, sealed winner and selected CUDA.

No losing candidate is lowered/compiled/reference-tested during exploration.
Design: doc/AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md.
"""
import copy
import hashlib
import json
import os
from pathlib import Path
import unittest
import test_tensor_attention_fusion as fusion
from test_tensor_attention_chunks import gpu_profile


def cost_profile():
    """Explicit synthetic instruction/rate envelope, never hardware calibration."""
    return dict(schema='open64.mla-cost-profile.v1',id='sm90-synthetic-v1',architecture='sm_90',provenance='synthetic',unit='reference_cycles',
        parameters=dict(global_bytes_per_cycle=128,scalar_lanes_per_cycle=128,shared_bytes_per_cycle=128,dependent_scalar_cycles=4,
        exp_warp_cycles=16,div_warp_cycles=16,shuffle_warp_cycles=4,cta_barrier_cycles=32,warp_fence_cycles=4,
        launch_cycles=1000,load_latency_cycles=100,latency_hiding_warps=16))


def search_options(**changes):
    """Keep strategy/termination independent and reproducibly bounded."""
    result=dict(schema='open64.mla-search-options.v1',strategy='portfolio',stop='exhausted',max_evaluations=12,seed=17,patience=12,
        minimum_gain_cycles=0,target_cycles=0,gap_ppm=10000,remaining_invocations=0,evaluation_cost_cycles=0)
    result.update(changes);return result


@unittest.skipUnless(os.environ.get('OPEN64_TENSOR_LOWERER'),'native tools not configured')
class AttentionSearchTests(unittest.TestCase):
    setUp=fusion.AttentionFusionTests.setUp
    run_tool=fusion.AttentionFusionTests.run_tool
    fixture=fusion.AttentionFusionTests.fixture
    execute=fusion.AttentionFusionTests.execute

    def source(self,length=513):
        """Capture source once, before entering detached estimation."""
        prior=self.fixture(length,split=128 if length>4096 else 0);source=self.root/'attention.json'
        self.run_tool(self.lowerer,'fuse-attention',prior,source,'--allow-reassociate');return source

    def explore(self,source,label='search',cost=None,options=None,gpu=None,success=True):
        """Only JSON is passed to the estimation command; return the full census."""
        model=self.root/(label+'.cost.json');model.write_text(json.dumps(cost or cost_profile()))
        profile=self.root/(label+'.gpu.json');profile.write_text(json.dumps(gpu or gpu_profile()))
        argv=[self.lowerer,'explore-attention',source,self.root/(label+'.json'),'--gpu-profile',profile,'--cost-profile',model,'--allow-reassociate']
        if options is not None:
            p=self.root/(label+'.options.json');p.write_text(json.dumps(options));argv+=['--search-options',p]
        result=self.run_tool(*argv,success=success);path=self.root/(label+'.json')
        if not success:self.assertFalse(path.exists());return None
        self.assertIn('native_initializations=0 materialized_kernels=0',result.stdout)
        return json.loads(path.read_text())

    def test_portfolio_is_estimation_only_and_deterministic(self):
        source=self.source();image=self.root/'native/source.B';original=hashlib.sha256(image.read_bytes()).hexdigest()
        files=set(self.root.rglob('*.B'));before=source.read_bytes()
        j=self.explore(source);again=self.explore(source,'again');self.assertEqual(j,again)
        self.assertEqual(files,set(self.root.rglob('*.B')));self.assertFalse(list(self.root.rglob('*.cu')))
        self.assertFalse(list(self.root.rglob('*.o')));self.assertEqual(source.read_bytes(),before)
        self.assertEqual(hashlib.sha256(image.read_bytes()).hexdigest(),original)
        self.assertEqual(j['candidate_estimates'],12);self.assertEqual(j['cache_hits'],24)
        self.assertEqual(j['native_initializations'],0);self.assertEqual(j['materialized_kernels'],0)
        self.assertEqual(j['conclusion'],'best_estimated_in_declared_space')
        for run in j['runs']:
            self.assertEqual(len(set(run['visited'])),12);self.assertEqual(run['termination'],'exhausted_declared_space')
        eligible=[c for c in j['candidates'] if c['eligible']]
        self.assertEqual(j['winner'],min(eligible,key=lambda c:(c['estimated_cycles'],c['ordinal']))['ordinal'])

    def test_counts_and_literal_whole_stage_cost_oracle(self):
        j=self.explore(self.source());rows=j['candidates'];v=j['cost_profile']['parameters'];sms=j['gpu_profile']['sms']
        one=next(s for s in rows[2]['stages'] if s['operation']==33)
        self.assertEqual((one['shared'],one['barriers'],one['shuffles']),(56,1,108))  # R5: two groups/warp.
        old=next(s for s in rows[11]['stages'] if s['operation']==33)
        self.assertEqual((old['shared'],old['barriers'],old['shuffles']),(272,3,64))
        def ceildiv(a,b):return (a+b-1)//b
        for c in rows:
            if not c['eligible']:continue
            self.assertEqual(c['estimated_cycles'],sum(s['total'] for s in c['stages']))
            for s in c['stages']:
                # Independent literal enumeration of individual occupancy rounds,
                # rather than the compiler's full/tail-round multiplication.
                totals=dict(compute=0,memory=0,special=0,local=0,sync=0)
                left=s['ctas'];rounds=0
                while left:
                    active=min(left,sms*s['resident']);q=ceildiv(active,sms);warps=q*s['threads']//32;rounds+=1
                    totals['compute']+=max(q*ceildiv(s['scalar_ops'],v['scalar_lanes_per_cycle']),s['dependency']*v['dependent_scalar_cycles'])
                    totals['memory']+=ceildiv(active*s['global_bytes'],v['global_bytes_per_cycle'])+ceildiv(v['load_latency_cycles'],min(warps,v['latency_hiding_warps']))
                    totals['special']+=q*(s['shuffles']*v['shuffle_warp_cycles']+s['exps']*v['exp_warp_cycles']+s['divs']*v['div_warp_cycles'])
                    totals['local']+=q*ceildiv(s['shared_bytes'],v['shared_bytes_per_cycle'])
                    totals['sync']+=q*(s['barriers']*v['cta_barrier_cycles']+s['fences']*v['warp_fence_cycles']);left-=active
                self.assertEqual(s['rounds'],rounds)
                for k,value in totals.items():self.assertEqual(s[k],value)
                self.assertEqual(s['total'],sum(totals.values())+v['launch_cycles'])

    def test_stopping_mechanisms_have_scoped_claims(self):
        source=self.source()
        cases=[('budget',{},'evaluation_budget'),('plateau',dict(patience=1,minimum_gain_cycles=1000000000),'empirical_plateau'),
               ('target',dict(target_cycles=1000000000000),'estimated_target_reached'),
               ('cost-aware',dict(remaining_invocations=1,evaluation_cost_cycles=1000000000000),'estimator_benefit_bound')]
        for stop,changes,reason in cases:
            j=self.explore(source,stop,options=search_options(strategy='exhaustive',stop=stop,max_evaluations=2,**changes))
            self.assertFalse(j['complete_declared_space']);self.assertEqual(j['runs'][0]['termination'],reason)
            self.assertEqual(j['conclusion'],'best_estimated_among_explored')
        model=cost_profile();model['parameters']['launch_cycles']=1000000000
        j=self.explore(source,'gap',cost=model,options=search_options(strategy='exhaustive',stop='relative-gap'))
        self.assertEqual(j['runs'][0]['termination'],'estimator_gap_certificate')
        self.assertLessEqual(j['candidates'][j['winner']]['estimated_cycles']-j['estimator_lower_bound'],j['estimator_lower_bound']//100)
        self.assertFalse(j['gpu_performance_verified'])

    def test_no_unsupported_confidence_or_bad_inputs_publish(self):
        source=self.source()
        for i,stop in enumerate(('probabilistic','hardware-bound','unknown')):
            self.explore(source,'unsupported'+str(i),options=search_options(stop=stop),success=False)
        for i,change in enumerate(('missing','zero','unit','arch','overflow')):
            model=cost_profile()
            if change=='missing':del model['parameters']['exp_warp_cycles']
            elif change=='zero':model['parameters']['div_warp_cycles']=0
            elif change=='unit':model['unit']='nanoseconds'
            elif change=='arch':model['architecture']='sm_90a'
            else:model['parameters']['launch_cycles']=1<<63
            self.explore(source,'bad-model'+str(i),cost=model,success=False)
        self.explore(source,'no-fit',gpu=gpu_profile(registers=4096),success=False)
        self.explore(source,'bad-budget',options=search_options(max_evaluations=0),success=False)

    def test_stale_report_and_fixed_recipe_reject_before_images(self):
        source=self.source();j=self.explore(source)
        variants=[]
        bad=copy.deepcopy(j);bad['winner']=(j['winner']+1)%12;variants.append(bad)
        bad=copy.deepcopy(j);bad['candidates'][j['winner']]['estimated_cycles']+=1;variants.append(bad)
        bad=copy.deepcopy(j);bad['cost_profile']['parameters']['launch_cycles']+=1;variants.append(bad)
        bad=copy.deepcopy(j);bad['gpu_profile']['sms']+=1;variants.append(bad)
        bad=copy.deepcopy(j);bad['runs'][0]['visited'].reverse();variants.append(bad)
        for i,bad in enumerate(variants):
            path=self.root/('bad-report%d.json'%i);path.write_text(json.dumps(bad));dest=self.root/('bad-selected%d.json'%i)
            self.run_tool(self.lowerer,'select-attention',path,dest,success=False);self.assertFalse(dest.exists())
        selected=self.root/'selected.json';self.run_tool(self.lowerer,'select-attention',self.root/'search.json',selected)
        before=self.root/'never.before.B';after=self.root/'never.after.B';text=selected.read_text()
        current=json.loads(text)['split_width'];bad=self.root/'bad-fixed.json';bad.write_text(fusion.reseal(text.replace('"split_width" : '+str(current),'"split_width" : 4096',1)))
        self.run_tool(self.lowerer,'device',bad,before,after,success=False);self.assertFalse(before.exists() or after.exists())
        self.run_tool(self.lowerer,'select-attention',self.root/'search.json',selected,success=False);self.assertEqual(selected.read_text(),text)

    def test_permission_and_optimization_boundaries(self):
        source=self.fixture(65,level=0);self.explore(source,'low',success=False)
        model=self.root/'model.json';model.write_text(json.dumps(cost_profile()));profile=self.root/'gpu.json';profile.write_text(json.dumps(gpu_profile()))
        self.run_tool(self.lowerer,'explore-attention',source,self.root/'deny.json','--gpu-profile',profile,'--cost-profile',model,'--deny-reassociate',success=False)
        self.assertFalse((self.root/'deny.json').exists())

    def test_portfolio_budget_continues_after_a_run_has_no_legal_winner(self):
        j=self.explore(self.source(),options=search_options(stop='budget',max_evaluations=2))
        self.assertEqual(len(j['runs']),3)
        for run in j['runs']:self.assertEqual(len(run['visited']),2)
        self.assertFalse(j['runs'][0]['eligible_winner']);self.assertIsNone(j['runs'][0]['winner'])
        visited=set(i for r in j['runs'] for i in r['visited'])
        self.assertEqual(j['candidate_estimates'],len(visited));self.assertEqual(j['cache_hits'],6-len(visited))
        eligible=[c for c in j['candidates'] if c['evaluated'] and c['eligible']]
        self.assertEqual(j['winner'],min(eligible,key=lambda c:(c['estimated_cycles'],c['ordinal']))['ordinal'])
        self.assertFalse(j['complete_declared_space'])

    def test_explicit_override_clears_the_fixed_search_evidence(self):
        source=self.source();self.explore(source);selected=self.root/'selected.json'
        self.run_tool(self.lowerer,'select-attention',self.root/'search.json',selected)
        for command,flag in [('optimize','--fuse'),('cooperate','--allow-reassociate'),('fuse-attention','--allow-reassociate')]:
            dest=self.root/(command+'-stale.json');self.run_tool(self.lowerer,command,selected,dest,flag,success=False);self.assertFalse(dest.exists())
        manual=self.root/'manual.json';self.run_tool(self.lowerer,'stream-attention',selected,manual,'--split-width',128,'--allow-reassociate')
        j=json.loads(manual.read_text());self.assertEqual(j['schema'],'open64.tensor-kernel.v5');self.assertNotIn('attention_search',j)
        profile=self.root/'new-gpu.json';profile.write_text(json.dumps(gpu_profile()))
        automatic=self.root/'coverage.json';self.run_tool(self.lowerer,'auto-attention',selected,automatic,'--gpu-profile',profile,'--allow-reassociate')
        j=json.loads(automatic.read_text());self.assertEqual(j['schema'],'open64.tensor-kernel.v6');self.assertNotIn('attention_search',j)

    def test_selected_short_native_reference(self):
        model=cost_profile();model['parameters']['launch_cycles']=1000000000
        selected=self.execute(65,gpu=gpu_profile(sms=16),search_cost=model)
        self.assertEqual(selected['schema'],'open64.tensor-kernel.v7');self.assertEqual(selected['split_width'],0)

    def test_selected_long_native_reference(self):
        selected=self.execute(547,variant='wide',gpu=gpu_profile(sms=16),search_cost=cost_profile())
        self.assertEqual(selected['schema'],'open64.tensor-kernel.v7')
        evidence=selected['attention_search'];self.assertEqual(selected['split_width'],evidence['candidates'][evidence['winner']]['width'])


if __name__=='__main__':unittest.main()
