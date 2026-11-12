"""Prepare portable Q7 data from an authenticated Q6d receipt, off GPU.

PyTorch and retained host source code are used only here, outside search. The
recipient runner uses Python's standard library and fixed native cubins.
See doc/AI-COMPILER-MLA-Q7-QUALIFICATION-v1.0.md.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import sys
from mla_q7_common import authenticate


def prepare(args):
    import torch
    root, repo, out = args.receipt.resolve(), args.source.resolve(), args.output.resolve()
    # Q6d authenticates its whole unchanged Q6c parent. Never recover live Q3
    # device leases from these saved host provenance strings.
    verified = authenticate(root, args.expected_sha256, allow_internal_symlinks=True)
    prior = root / 'parent'
    if hashlib.sha256((prior/'SHA256SUMS').read_bytes()).hexdigest() != '41298aa3a567af0e96653ecacc7af86390a2e5504c9051e34d2a1bf320dc0e21':
        raise ValueError('wrong Q6c parent')
    out.mkdir(parents=True, exist_ok=False)
    sys.path.insert(0, str(repo/'osprey/be/lno/tests')); sys.path.insert(0, str(repo/'osprey/torch2whirl/python/tests'))
    from run_quantized_high_review import execute, DTYPES
    from models.mla_quantized_paged import make_fixture
    from mla_q5_long_fixture import make_fixture as long_fixture
    from models.mla_quantized_reference import reference
    torch.set_num_threads(1)
    cases = []; source_hashes = {}
    def copy(src, dst):
        dst.parent.mkdir(parents=True, exist_ok=True); shutil.copy2(src, dst)
    for item in json.loads((prior/'cases.json').read_text()):
        label = item['case'] + '--' + ('sync' if item['delivery']==1 else 'async')
        old = prior/item['relative']; folder = out/'cases'/label; folder.mkdir(parents=True)
        stages = [json.loads((old/(s+'.json')).read_text()) for s in ('prepare','attention','finalize')]
        recipe = stages[0]['recipe']; buffers = stages[0]['buffers']
        name = 'dense_s8_decode' if item['case']=='zero-active' else item['case']
        schema = json.loads((prior/item['family']/(name+'.schema.json')).read_text())
        model, inputs, meta = (long_fixture if 'long' in item['family'] else make_fixture)(name)
        inputs = [x.clone() for x in inputs]
        if item['case']=='zero-active': inputs[6].zero_()
        by_input = {i:inputs[v['ordinal']] for i,v in enumerate(recipe['values']) if v['role']==1}
        storage = []
        for ordinal, index in enumerate(buffers):
            value = recipe['values'][index]; size = torch.empty((), dtype=DTYPES[value['dtype']]).element_size()
            for dim in value['shape']: size *= dim
            b = dict(bytes=size, storage=index, dtype=value['dtype'], role=value['role'])
            if index in by_input:
                b['input'] = 'input-%d.bin' % ordinal
                (folder/b['input']).write_bytes(by_input[index].contiguous().numpy().tobytes())
            storage.append(b)
        # Status is initialized by Q6d immediately before launch, not fabricated
        # by the runner. Scratch/output interiors deliberately remain undefined.
        storage.append(dict(bytes=4, dtype='int32', role=0, status=True))
        g = schema['geometry']; rows = []
        for batch in range(g['B']):
            q, length, append = g['Q'], g['L'], g['A']
            phase = {'prefill':1,'decode':2,'speculative-verify':3}[meta['phase']]
            update = 2 if append else 0 if schema['paged'] else 1
            rows.append([phase,q,length-append,length,batch*q,1,0,update,append]+list(range(length-q,length))+list(range(length))+list(range(length-append,length)))
        metadata = torch.tensor(rows, dtype=torch.int64).numpy().tobytes(); (folder/'metadata.bin').write_bytes(metadata)
        source = execute(old.parent.parent/'native.so', json.loads((old.parent.parent/'selected.manifest.json').read_text()), inputs)
        with torch.no_grad(): target = model(*inputs)
        scalar = reference([x.tolist() for x in inputs], meta)
        outputs = []; count = 0
        for ordinal, index in enumerate(buffers):
            value = recipe['values'][index]
            if value['role'] != 2: continue
            ref = source[count]; torch.testing.assert_close(ref, target[count], rtol=2e-5 if count<2 else 0, atol=2e-6 if count<2 else 0, equal_nan=True)
            independent = torch.tensor(scalar[count], dtype=torch.float64 if count<2 else ref.dtype)
            torch.testing.assert_close(ref.double() if count<2 else ref, independent, rtol=2e-5 if count<2 else 0, atol=2e-6 if count<2 else 0, equal_nan=True)
            refs = []
            for title, tensor in (('selected-source',ref),('pytorch',target[count]),('scalar',independent)):
                filename = 'output-%d-%s.bin' % (count,title); (folder/filename).write_bytes(tensor.contiguous().numpy().tobytes())
                refs.append(dict(name=title, file=filename, dtype=str(tensor.dtype).split('.')[-1]))
            outputs.append(dict(ordinal=ordinal, dtype=value['dtype'], shape=value['shape'], references=refs,
                                rtol=2e-5 if count<2 else 0, atol=2e-6 if count<2 else 0)); count += 1
        if count != 6: raise ValueError('wrong reference result count')
        selected = json.loads(json.loads((old.parent/'manifest.json').read_text())['selected_plan'])
        image = (old/'native.cubin').read_bytes()
        code = dict(program=schema['program'], recipe=recipe, source_key=selected['source_key'], mapping_key=stages[0]['mapping_key'],
                    cubin_bytes=len(image), sha256=list(hashlib.sha256(image).digest()), stages=stages)
        (folder/'code.json').write_text(json.dumps(code, indent=2)+'\n')
        for name in ('native.cubin','math.cu'):
            copy(old/name, folder/name)
        for stage in ('prepare','attention','finalize'):
            for suffix in ('cu','ptx','json','low.B','low.T','before.T','high.T','mid.T','mid-low.raw.diff'):
                name = stage+'.'+suffix; copy(old/name, folder/name)
                source_hashes['cases/'+label+'/'+name] = hashlib.sha256((old/name).read_bytes()).hexdigest()
        published_ordinals = [buffers.index(i) for i in schema['returned']]
        if schema['paged']: published_ordinals += [buffers.index(schema[x]) for x in ('table','tokens')]
        publication = [storage[i]['bytes'] for i in published_ordinals]
        publication.append(len(metadata))
        cases.append(dict(id=label, source_case=item['case'], delivery=item['delivery'], paged=schema['paged'],
                          buffers=storage, outputs=outputs, metadata_bytes=len(metadata), publication_bytes=publication, publication_ordinals=published_ordinals,
                          source_geometry=g, query_ordinal=buffers.index(2)))
        print('PREPARED',label,flush=True)
    shutil.copytree(root/'source', out/'source')
    scripts = Path(__file__).parent
    for name in ('mla_q7_common.py','run_mla_q7.py','prepare_mla_q7.py','build_mla_q7.py','test_mla_q7.py',
                 'package_mla_q7_results.py','test_mla_q7_results.py','RUN_COMMANDS.sh',
                 'run_mla_q7_workflow.py','test_mla_q7_workflow.py','validate_mla_q7_results.py',
                 'test_mla_q7_validation.py'):
        copy(scripts/name,out/name); copy(scripts/name,out/'source/osprey/be/lno/tests'/name)
    for name in ('mla_q7_profile.py','profile_mla_q7.py','test_mla_q7_profiling.py','RETURN_RESULTS.sh'):
        copy(scripts/name,out/name); copy(scripts/name,out/'source/osprey/be/lno/tests'/name)
    for name in ('gpu_test_results.py','validate_gpu_results.py'):
        copy(scripts/name,out/name);copy(scripts/name,out/'source/osprey/be/lno/tests'/name)
    for name in ('GPU-TEST-SHARED-RESULTS-v1.0.md','AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.27.md'):
        copy(repo/'doc'/name,out/'source/doc'/name)
    copy(repo/'doc/AI-COMPILER-MLA-Q7-PROFILING-v1.0.md',out/'source/doc/AI-COMPILER-MLA-Q7-PROFILING-v1.0.md')
    copy(repo/'doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.26.md',out/'source/doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.26.md')
    copy(repo/'doc/MLA-Q7-PROFILING-RELEASE-NOTES-v1.0.md',out/'source/doc/MLA-Q7-PROFILING-RELEASE-NOTES-v1.0.md')
    for name in ('osprey/ir_tools/tensor_serving_cuda_review.cxx','osprey/ir_tools/Makefile.gbase',
                 'doc/AI-COMPILER-MLA-Q7-QUALIFICATION-v1.0.md','doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.25.md',
                 'doc/AI-COMPILER-MLA-Q7-RESULTS-RETURN-v1.0.md','doc/AI-COMPILER-MLA-Q7-RESULTS-RETURN-v1.1.md',
                 'doc/AI-COMPILER-MLA-Q7-RESULTS-VALIDATION-v1.0.md',
                 'doc/MLA-Q7-VALIDATOR-README.md','doc/MLA-Q7-VALIDATOR-RELEASE-NOTES-v1.0.md'):
        copy(repo/name,out/'source'/name)
    copy(repo/'osprey/include/open64_mla_staging.cuh',out/'source/osprey/include/open64_mla_staging.cuh')
    copy(args.tools/'ir_tools/libmla_serving_cuda_q7.so',out/'runtime/ir_tools/libmla_serving_cuda_q7.so')
    copy(args.tools/'be/be.so',out/'runtime/be/be.so')
    copy(args.tools/'ir_tools/tensor_attention_search.o',out/'build-input/osprey/targdir/ir_tools/tensor_attention_search.o')
    copy(root/'sdk-build-commands.json',out/'host-build-commands.json')
    for dep in (root/'build-dependencies').glob('*.d'):
        for word in dep.read_text().replace('\\\n',' ').split()[1:]:
            if word.startswith('/build/'):
                relative = Path(word[len('/build/'):]); copy(root/'runtime'/relative,out/'build-input'/relative)
    for name in ('receipt.json','source-file-hashes.json','SHA256SUMS'):
        copy(root/name,out/'upstream-q6d'/name)
    (out/'unchanged-generated-source.json').write_text(json.dumps(source_hashes,indent=2)+'\n')
    packet = dict(schema='open64.mla-q7.packet.v1',status='PREPARED_NOT_GPU_QUALIFIED',q6d_receipt_sha256=args.expected_sha256,
                  authenticated_upstream_files=verified,cases=cases,source_outputs=84,portable_oracle_comparisons=252,
                  architecture='sm_90',ptx_isa='8.6',cuda_toolchain='12.8.1',gpu_execution=False,
                  search_invocations=0,aot_selection='deferred',personnel_roles='TBD')
    packet['profiling_contract']='open64.mla-q7.profiling.v1'
    packet['shared_results_contract']='open64.gpu-test.results.v1'
    (out/'packet.json').write_text(json.dumps(packet,indent=2)+'\n')
    return packet


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--receipt',type=Path,required=True); p.add_argument('--expected-sha256',required=True)
    p.add_argument('--source',type=Path,required=True); p.add_argument('--tools',type=Path,required=True); p.add_argument('--output',type=Path,required=True)
    prepare(p.parse_args())


if __name__=='__main__': main()
