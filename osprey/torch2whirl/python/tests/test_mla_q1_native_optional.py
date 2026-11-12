"""Q1 typed native boundaries and source-inferred cache facts, without a GPU."""
import copy
import importlib.util
import unittest

AVAILABLE = (importlib.util.find_spec("torch") is not None and
             importlib.util.find_spec("open64_dsc._whirl") is not None)
if AVAILABLE:
    import torch
    from open64_dsc import export_to_whirl,WhirlExportOptions,load_builder,_whirl
    from models.mla_quantized_paged import make_fixture
    from test_tensor_expression_native_optional import TensorExpressionNativeTests as NativeBase
else:
    NativeBase = unittest.TestCase


def records(module):
    """Copy a public logical manifest; never inspect native handles/layouts."""
    types={x.name:x for x in module.tensor_types}
    values={x.name:types[x.type_name] for x in module.values}
    result=[]
    def row(name,opcode,version,kids,attrs):
        t=values[name]
        import json
        return dict(name=name,opcode=opcode,version=version,dtype=t.dtype,
                    shape=json.loads(t.logical_shape),operands=list(kids),attrs=dict(attrs))
    result.extend(row(v.name,"formal",0,[],{}) for v in module.values if v.value_kind=="formal")
    result.extend(row(o.metadata["semantic_name"],o.name,int(o.metadata["operator_version"]),o.kids,o.attrs)
                  for o in module.graph_operators)
    return result


@unittest.skipUnless(AVAILABLE,"native extension and CPU torch required")
class Q1NativeContracts(NativeBase):
    # Reuse the established native harness; inherited old-version tests also
    # exercise compatibility against the new registry and preflight predicate.
    def test_quantized_native_positive_schemas(self):
        f,x=self.tensor("float32",(2,4))
        byte,_=self.tensor("int8",(2,4),False)
        u,ux=self.tensor("uint8",(2,4))
        packed,_=self.tensor("uint8",(2,2),False)
        _,idx=self.tensor("int64",(1,))
        gathered,_=self.tensor("uint8",(1,4),False)
        _,updates=self.tensor("uint8",(1,4))
        self.assertGreater(self.create("common.abs",[x],{},f),0)
        self.assertGreater(self.create("common.round_even",[x],{"attr.rounding":"ties_to_even"},f),0)
        clamp={"attr.has_min":"true","attr.min_bits":"c2fe0000","attr.has_max":"true","attr.max_bits":"42fe0000"}
        self.assertGreater(self.create("common.clamp",[x],clamp,f),0)
        self.assertGreater(self.create("common.convert",[x],{"attr.src_dtype":"float32","attr.dst_dtype":"int8","attr.mode":"checked_integral"},byte),0)
        attrs={"attr.axis":"1","attr.nibble_order":"even_low","attr.code_bits":"4"}
        self.assertGreater(self.create("common.pack_u4",[ux],attrs,packed),0)
        gather={"attr.axis":"0","attr.bounds":"guarded_nonnegative","attr.alias_rule":"fresh_result"}
        self.assertGreater(self.create("common.gather",[ux,idx],gather,gathered),0)
        self.assertGreater(self.create("common.scatter",[ux,idx,updates],dict(gather,**{"attr.conflicts":"unique_indices"}),u,2),0)

    def test_quantized_native_negative_schemas_types_shapes(self):
        f,x=self.tensor("float32",(2,4))
        _,u=self.tensor("uint8",(2,4))
        odd,odd_value=self.tensor("uint8",(2,3))
        _,idx=self.tensor("int64",(1,))
        for name,kids,attrs,ty,version in (
            ("common.abs",[u],{},odd,1),
            ("common.round_even",[x],{"attr.rounding":"toward_zero"},f,1),
            ("common.clamp",[x],{"attr.has_min":"true","attr.min_bits":"7fc00000","attr.has_max":"false","attr.max_bits":"00000000"},f,1),
            ("common.convert",[x],{"attr.src_dtype":"float32","attr.dst_dtype":"uint8","attr.mode":"wrap"},odd,1),
            ("common.pack_u4",[odd_value],{"attr.axis":"1","attr.nibble_order":"even_low","attr.code_bits":"4"},odd,1),
            ("common.gather",[u,idx],{"attr.axis":"0","attr.bounds":"unchecked","attr.alias_rule":"fresh_result"},odd,1),
            ("common.scatter",[u,idx,u],{"attr.axis":"0","attr.bounds":"guarded_nonnegative","attr.conflicts":"unique_indices","attr.alias_rule":"fresh_result"},odd,2),
        ):
            with self.subTest(name=name),self.assertRaises(RuntimeError):self.create(name,kids,attrs,ty,version)

    def test_integer_v2_versions_and_literals(self):
        i,x=self.tensor("int64",(2,4))
        attrs={"attr.broadcast_rule":"numpy","attr.integer_domain":"nonnegative_i64_checked"}
        for name in ("common.add","common.mul","common.div","common.rem"):
            a=dict(attrs)
            if name in ("common.div","common.rem"):a["attr.rounding"]="floor"
            self.assertGreater(self.create(name,[x,x],a,i,2),0)
            with self.assertRaises(RuntimeError):self.create(name,[x,x],dict(a,unknown="true"),i,2)
        for text in ("0","-9223372036854775808","9223372036854775807"):
            self.assertGreater(self.create("common.fill_like",[x],{"attr.value_i64":text},i,2),0)
        for text in ("-0","01","9223372036854775808","-9223372036854775809","1.0"):
            with self.subTest(text=text),self.assertRaises(RuntimeError):
                self.create("common.fill_like",[x],{"attr.value_i64":text},i,2)


@unittest.skipUnless(AVAILABLE,"native extension and CPU torch required")
class Q1CacheInferenceTests(unittest.TestCase):
    def source(self,name,native=False):
        m,x,_=make_fixture(name)
        return export_to_whirl(m,x,WhirlExportOptions(backend="native" if native else "mock"))

    def test_undeclared_roles_and_encodings_survive_native_ingestion(self):
        module=self.source("append_u4_verify",True)
        types={t.name:t for t in module.tensor_types}
        for name,role in (("cache","attention.latent_kv_cache.v1"),("rotary","attention.rotary_key_cache.v1")):
            descriptor=types[name+"_type"].descriptor
            self.assertEqual(descriptor["traits"],role)
            self.assertEqual(descriptor["quantization"],"u4-offset8-f32.v1")
        self.assertFalse(any("phase" in k for t in module.tensor_types for k in t.descriptor))

    def test_native_query_depends_on_dataflow_not_names_or_phase(self):
        a=records(self.source("dense_s8_decode"));b=records(self.source("dense_s8_prefill_tail"))
        self.assertEqual(a,b)
        facts=_whirl.infer_quantized_cache_traits(a);self.assertTrue(facts)
        rename={v["name"]:"v"+str(i) for i,v in enumerate(a)}
        for value in a:
            value["name"]=rename[value["name"]];value["operands"]=[rename[n] for n in value["operands"]]
        renamed=_whirl.infer_quantized_cache_traits(a)
        self.assertEqual({rename[k]:dict(v,scale=rename[v["scale"]]) for k,v in facts.items()},renamed)
        self.assertTrue(all(v["encoding"]!="s8-token-rne127-f32.v1" for v in facts.values()))

    def test_missing_decode_proof_and_mismatched_page_scale_gather_are_unknown(self):
        graph=records(self.source("paged_s8_decode"))
        for value in graph:
            if value["opcode"]=="common.gather" and value["dtype"]=="float32":
                value["operands"][1]="tokens"
        self.assertEqual(_whirl.infer_quantized_cache_traits(graph),{})
        graph=records(self.source("dense_s8_decode"))
        for value in graph:
            if value["opcode"]=="common.convert" and value["dtype"]=="float32":
                value.update(opcode="formal",version=0,operands=[],attrs={})
        self.assertEqual(_whirl.infer_quantized_cache_traits(graph),{})

    def test_stale_paired_scale_update_is_refused(self):
        graph=records(self.source("append_s8_decode"))
        for value in graph:
            if value["opcode"]=="common.gather" and value["dtype"]=="float32":
                value["operands"][0]="cs" # incoming scales with successor bytes
        with self.assertRaisesRegex(ValueError,"paired scale"):_whirl.infer_quantized_cache_traits(graph)

    def test_declared_contradiction_and_invalid_query_are_refused(self):
        m,x,_=make_fixture("paged_s8_decode")
        m.__open64_tensor_traits__={"cache":"attention.rotary_key_cache.v1"}
        with self.assertRaisesRegex(ValueError,"contradicts"):
            export_to_whirl(m,x,WhirlExportOptions(backend="native"))
        with self.assertRaises(ValueError):_whirl.infer_quantized_cache_traits([{}])
        graph=records(self.source("dense_s8_decode"));graph[-1]["operands"]=["unknown"]
        with self.assertRaises(ValueError):_whirl.infer_quantized_cache_traits(graph)

    def test_fake_quantization_remains_float_storage(self):
        module=self.source("fake_s8_decode",True)
        facts=_whirl.infer_quantized_cache_traits(records(module))
        self.assertTrue(any(v["encoding"]=="fake-s8-float32.v1" for v in facts.values()))
        self.assertTrue(all(t.dtype not in ("int8","uint8") for t in module.tensor_types))
        self.assertTrue(all("quantization" not in t.descriptor for t in module.tensor_types))

    def test_general_cast_and_escaping_pack_intermediate_refuse(self):
        class Cast(torch.nn.Module):
            def forward(self,x):return x.to(torch.int8)
        class Leak(torch.nn.Module):
            def forward(self,x):
                from models.mla_quantized_paged import quantize
                codes=torch.clamp(torch.round(x),0,15).to(torch.uint8)
                low=codes[...,0::2]
                return torch.bitwise_or(low,torch.bitwise_left_shift(codes[...,1::2],4)),low
        for model in (Cast(),Leak()):
            with self.subTest(model=type(model).__name__),self.assertRaises(NotImplementedError):
                export_to_whirl(model,(torch.randn(2,4),),WhirlExportOptions(backend="native"))


del NativeBase
if __name__=="__main__":unittest.main()
