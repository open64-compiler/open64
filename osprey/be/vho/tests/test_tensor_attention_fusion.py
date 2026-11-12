"""Native fused attention admission, synchronization, tails and FP64 oracles.

The fixtures use the public PyTorch frontend in isolated processes and an
expanded (unabsorbed) independent reference. Design: FUSED-MLA-v0.1.md.
"""
import difflib
import json
import math
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys
import unittest

import test_tensor_kernel_lower as baseline
from mla_kernel_review import runner
from tensor_schedule_review import execute_source,explanation_blocks
from validate_mla_gpu import check_output


def produce(path,length,variant):
    """Capture the real source with unequal shapes and save an expanded oracle."""
    import torch
    from open64_dsc import WhirlExportOptions,export_to_whirl,save_as_whirl
    from models.absorbed_mla import AbsorbedMLA,validate_inputs
    from models.mla_reference import expanded_reference
    path.mkdir(exist_ok=False)
    b,h,dn,dr,r,dv=2,2,3,2,5,4
    shapes=[(b,h,dn),(b,h,dr),(b,length,r),(b,length,dr),(h,dn,r),(h,dv,r)]
    generator=torch.Generator().manual_seed(27182)
    inputs=[torch.randn(shape,generator=generator,dtype=torch.float32)*0.15 for shape in shapes]
    allowed=torch.ones(b,h,length,dtype=torch.bool)
    allowed[0,0,:]=False
    if variant in ('empty-negative','large-values'):
        inputs[0].zero_();inputs[1].fill_(1);inputs[3].fill_(-1000)
        allowed[...,:33]=False
        if variant=='large-values': inputs[2].fill_(3e37);inputs[5].mul_(0.1)
    inputs.append(allowed);inputs=tuple(inputs);validate_inputs(inputs)
    model=AbsorbedMLA(1/math.sqrt(dn+dr)).eval()
    module=export_to_whirl(model,inputs,WhirlExportOptions(backend='native',model_name='unrelated_attention_fixture'))
    assert len(module.graph_operators)==46
    save_as_whirl(module,str(path/'source.B'))
    (path/'input.bin').write_bytes(b''.join(x.numpy().tobytes() for x in inputs))
    values=expanded_reference(*(x.tolist() for x in inputs),model.alpha)
    groups=[]
    for v in values:
        groups.append([x if math.isfinite(x) else '-inf' if x<0 else '+inf'
                       for x in torch.tensor(v,dtype=torch.float64).flatten().tolist()])
    (path/'reference.json').write_text(json.dumps(dict(schema='open64.mla-output-oracle.v1',values=groups,rtol=2e-5,atol=2e-6),indent=2)+'\n')


def reseal(text):
    """Preserve canonical JsonCpp spelling while resealing deliberate test changes."""
    raw=re.sub(r'^   "recipe_fingerprint" : "[0-9]+",\n','',text,flags=re.M)
    assert raw!=text
    value=1469598103934665603
    for byte in raw.encode(): value=((value^byte)*1099511628211)&((1<<64)-1)
    return re.sub(r'("recipe_fingerprint" : ")[0-9]+(")',lambda m:m[1]+str(value)+m[2],text)


@unittest.skipUnless(os.environ.get('OPEN64_TENSOR_LOWERER'),'native lowering tools are not configured')
class AttentionFusionTests(unittest.TestCase):
    setUp=baseline.TensorKernelTests.setUp
    run_tool=baseline.TensorKernelTests.run_tool

    def fixture(self,length=65,variant='normal',level=3,split=0):
        """Construct source and the preceding cooperative recipe through real tools."""
        source=self.root/'native'
        self.run_tool(sys.executable,__file__,'--produce',source,length,variant)
        p=self.root/'baseline.json'
        self.run_tool(self.lowerer,'prepare',source/'source.B',p,'--verify',level)
        if level!=3:return p
        fused=self.root/'fused.json';coop=self.root/'cooperative.json'
        options=['--split-width',split,'--allow-reassociate'] if split else []
        self.run_tool(self.lowerer,'optimize',p,fused,'--fuse',*options)
        self.run_tool(self.lowerer,'cooperate',fused,coop,'--allow-reassociate')
        return coop

    def execute(self,length,split=0,variant='normal',rename=False):
        """Run actual mapped WHIRL CUDA on synchronized host workers and compare FP64."""
        previous=self.fixture(length,variant,split=split)
        if rename:
            text=previous.read_text().replace('AbsorbedMLA','RenamedKernel').replace('query_nope','opaque_parameter')
            previous.write_text(reseal(text))
        recipe=self.root/'attention.json'
        self.run_tool(self.lowerer,'fuse-attention',previous,recipe,'--allow-reassociate')
        before,after=self.root/'attention.before.B',self.root/'attention.after.B'
        self.run_tool(self.lowerer,'device',recipe,before,after)
        for image in (before,after): self.run_tool(self.reader,'-st','-src',image,image.with_suffix('.T'))
        (self.root/'attention.before-after.diff').write_text(''.join(difflib.unified_diff(
            before.with_suffix('.T').read_text().splitlines(True),after.with_suffix('.T').read_text().splitlines(True))))
        manifest=self.root/'launch.json';self.run_tool(self.lowerer,'audit',after,recipe,manifest)
        launch=json.loads(manifest.read_text());self.assertEqual(len(launch['kernels']),6 if split and length>split else 5)
        self.assertFalse({15,20}.intersection(int(s['storage']) for s in launch['storages']))
        self.run_tool(self.w2c,'-cuda','-TARG:abi=n64','-O0','-CLIST:dotc_file='+str(self.root/'kernels.cu'),
                      '-CLIST:doth_file='+str(self.root/'kernels.h'),after)
        shutil.copyfile(Path(__file__).parent/'tensor_collective_stub/cuda_runtime.h',self.root/'cuda_runtime.h')
        for name,reverse in [('host',False),('reverse',True)]:
            (self.root/(name+'.cxx')).write_text(runner(launch,False,reverse=reverse))
            self.run_tool('g++','-std=c++17','-O1','-pthread','-ffp-contract=off','-fno-fast-math','-I'+str(self.root),
                          self.root/(name+'.cxx'),'-o',self.root/name)
            self.run_tool(self.root/name,self.root/'native/input.bin',self.root/(name+'.bin'))
            check_output(self.root/'native/reference.json',self.root/(name+'.bin'))
        self.assertEqual((self.root/'host.bin').read_bytes(),(self.root/'reverse.bin').read_bytes())
        source=self.root/'optimized.cu';self.run_tool(self.lowerer,'schedule-source',recipe,source)
        self.assertEqual(explanation_blocks(source.read_text()),explanation_blocks((self.root/'kernels.cu').read_text()))
        function=re.search(r'void (open64_optimized_[A-Za-z0-9_]+)\(',source.read_text()).group(1)
        header=Path(__file__).resolve().parents[2]/'whirl2c'
        self.run_tool('g++','-std=c++17','-x','c++','-O1','-shared','-fPIC','-ffp-contract=off','-fno-fast-math',
                      '-I'+str(header),source,'-o',self.root/'optimized.so')
        got=execute_source(self.root/'optimized.so',function,json.loads(recipe.read_text()),(self.root/'native/input.bin').read_bytes())
        self.assertEqual(got,(self.root/'host.bin').read_bytes())
        for mode in ('audit-mask','audit-barrier','audit-shared','audit-resource','audit-collective-prototype','audit-comment','audit-comment-missing','audit-attention-layout'):
            out=self.root/(mode+'.json');self.run_tool(self.lowerer,mode,after,recipe,out,success=False);self.assertFalse(out.exists())

    def test_single_token_column_and_batch_tails(self): self.execute(1,rename=True)
    def test_short_fused_attention_unequal_shapes(self): self.execute(65)
    def test_split_fused_attention_uneven_chunks(self): self.execute(129,split=33)
    def test_empty_chunks_with_very_negative_scores(self): self.execute(65,split=33,variant='empty-negative')
    def test_local_normalization_avoids_large_unnormalized_vectors(self): self.execute(65,split=33,variant='large-values')

    def test_permission_topology_and_numerical_guards(self):
        previous=self.fixture()
        out=self.root/'no-permission.json'
        self.run_tool(self.lowerer,'fuse-attention',previous,out,'--deny-reassociate',success=False);self.assertFalse(out.exists())
        original=previous.read_text()
        changes=[('3e93cd3a','7f800000','finite scale'),
                 ('"attr.value_bits" : "00000000"','"attr.value_bits" : "3f800000"','numerical guard mismatch')]
        # The fixture has a different finite scale; replace its exact canonical bits.
        j=json.loads(original);scale=j['operations'][14]['attributes']['attr.value_bits']
        changes[0]=(scale,'7f800000','finite scale')
        for i,(old,new,message) in enumerate(changes):
            bad=self.root/('bad%d.json'%i);bad.write_text(reseal(original.replace(old,new,1)))
            rejected=self.root/('rejected%d.json'%i)
            p=self.run_tool(self.lowerer,'fuse-attention',bad,rejected,'--allow-reassociate',success=False)
            self.assertIn(message,p.stderr);self.assertFalse(rejected.exists())
        # A valid unrelated tensor DAG must not be recognized by a model name.
        simple=self.root/'unrelated.B'
        self.run_tool(sys.executable,Path(baseline.__file__),'--produce','valid',simple)
        base=self.root/'unrelated.json';fused=self.root/'unrelated.fused.json';coop=self.root/'unrelated.coop.json'
        self.run_tool(self.lowerer,'prepare',simple,base,'--verify',3)
        self.run_tool(self.lowerer,'optimize',base,fused,'--fuse');self.run_tool(self.lowerer,'cooperate',fused,coop,'--allow-reassociate')
        self.run_tool(self.lowerer,'fuse-attention',coop,out,'--allow-reassociate',success=False);self.assertFalse(out.exists())

    def test_lower_levels_reject_fusion_before_device_output(self):
        source=self.root/'unrelated.B';self.run_tool(sys.executable,Path(baseline.__file__),'--produce','valid',source)
        for level in (0,1,2,3):
            recipe=self.root/('O%d.json'%level);out=self.root/('O%d.attention.json'%level)
            self.run_tool(self.lowerer,'prepare',source,recipe,'--verify',level)
            self.run_tool(self.lowerer,'fuse-attention',recipe,out,'--allow-reassociate',success=False);self.assertFalse(out.exists())


if __name__=='__main__':
    if len(sys.argv)>1 and sys.argv[1]=='--produce':produce(Path(sys.argv[2]),int(sys.argv[3]),sys.argv[4])
    else:unittest.main()
