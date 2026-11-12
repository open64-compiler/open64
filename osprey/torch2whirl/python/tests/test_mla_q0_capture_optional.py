"""Q0 real capture/census/oracle/refusal checks; no optimization or GPU work."""
import copy
import importlib.util
import json
import math
import os
from pathlib import Path
import tempfile
import unittest

TORCH_AVAILABLE=importlib.util.find_spec("torch") is not None
if TORCH_AVAILABLE:
    import torch
    from mla_q0_capture import (capture,census,check_case,probe_ingestion,write_artifacts,
                                replay_artifacts,classify)
    from mla_q0_contract import make_contract,validate_contract,loads
    from models.mla_quantized_paged import CASES,make_fixture,quantize,dequantize
    from models.mla_quantized_reference import encode_row,decode_row

@unittest.skipUnless(TORCH_AVAILABLE,"requires CPU PyTorch")
class MLAQ0CaptureTests(unittest.TestCase):
    def test_source_cases_against_independent_expanded_reference(self):
        for name in CASES:
            with self.subTest(case=name):
                result=check_case(name)
                self.assertEqual(len(result[5]),6)

    def test_real_census_drift(self):
        path=Path(__file__).with_name("golden")/"mla_q0_fx.json"
        self.assertEqual(census(),json.loads(path.read_text()))

    def test_every_source_node_is_classified(self):
        categories=set()
        for name in CASES:
            data=capture(name)[4]
            for row in data["nodes"]:
                self.assertIn(row["classification"]["category"],
                    ("region-contract","constant","common-substrate","domain-expression-component","state-effect-functional"))
                categories.add(row["classification"]["category"])
        self.assertIn("state-effect-functional",categories)
        self.assertIn("domain-expression-component",categories)

    def test_phase_is_not_a_graph_or_dtype_classifier(self):
        a,b=capture("dense_s8_decode"),capture("dense_s8_prefill_tail")
        self.assertEqual(a[4],b[4])
        self.assertNotEqual(a[2]["phase"],b[2]["phase"])
        contract=make_contract(b[2],b[1])
        contract["workload"]["phase"]=["unknown","decode"]
        self.assertEqual(validate_contract(contract)["workload"]["phase"],["unknown","decode"])
        verify=capture("append_u4_verify")
        self.assertGreater(verify[2]["queries"],1)
        self.assertEqual(verify[2]["phase"],"speculative-verify")

    def test_signed_and_nibble_rounding_and_decode(self):
        for kind in ("s8-token","u4-token"):
            # Max=qmax makes the scale exactly one, so half-integer inputs
            # exercise actual rounding ties rather than merely decimal halves.
            limit=127.0 if kind=="s8-token" else 7.0
            rows=[[0.0]*16,[-limit,-7.0,-3.5,-2.5,-1.5,-0.5,0.0,0.5,1.5,2.5,3.5,4,5,6,7,limit]]
            tensor=torch.tensor([rows])
            q,s=quantize(tensor,kind)
            for i,row in enumerate(rows):
                expected,scale=encode_row(row,kind)
                self.assertEqual(q[0,i].tolist(),expected)
                self.assertEqual(s[0,i].tolist(),scale)
                self.assertEqual(dequantize(q,s,kind)[0,i].tolist(),decode_row(expected,scale,kind))
        all_codes=torch.tensor([[[i | ((15-i)<<4) for i in range(16)]]],dtype=torch.uint8)
        scale=torch.ones(1,1,1)
        signed=torch.tensor([[[-128,-127,0,127]]],dtype=torch.int8)
        self.assertEqual(dequantize(signed,scale,"s8-token").tolist(),[[[-128.0,-127.0,0.0,127.0]]])
        self.assertEqual(dequantize(all_codes,scale,"u4-token").tolist(),
                         [[[v for i in range(16) for v in (float(i-8),float(7-i))]]])

    def test_actual_packed_and_fake_storage_are_distinct(self):
        packed=check_case("dense_s8_decode")[5][2]
        fake=check_case("fake_s8_decode")[5][2]
        self.assertEqual(packed.dtype,torch.int8)
        self.assertEqual(fake.dtype,torch.float32)
        self.assertEqual(fake.numel()*fake.element_size(),4*packed.numel()*packed.element_size())

    def test_partial_page_and_unused_padding_are_not_read(self):
        model,inputs,meta=make_fixture("paged_s8_decode")
        baseline=model(*inputs)
        changed=[x.clone() for x in inputs]
        used=set()
        for b,row in enumerate(inputs[6].tolist()):
            used.update(row[t//4]*4+t%4 for t in range(meta["length"]))
        for slot in range(meta["physical_pages"]*4):
            if slot not in used:
                changed[3].reshape(-1)[slot]=float("nan")
                changed[5].reshape(-1)[slot]=float("nan")
        actual=model(*changed)
        for a,b in zip(baseline[:2],actual[:2]):torch.testing.assert_close(a,b,rtol=0,atol=0)
        self.assertNotEqual(meta["length"]%4,0)

    def test_functional_append_retains_untouched_rows_and_inputs(self):
        model,inputs,meta=make_fixture("append_s8_prefill")
        originals=[x.clone() for x in inputs]
        actual=model(*inputs);slots=set(inputs[12].reshape(-1).tolist())
        for source,result in zip(inputs[2:6],actual[2:]):
            self.assertNotEqual(source.data_ptr(),result.data_ptr())
            a,b=source.reshape(-1,source.shape[-1]),result.reshape(-1,result.shape[-1])
            for row in range(a.shape[0]):
                if row not in slots:torch.testing.assert_close(a[row],b[row],rtol=0,atol=0)
        for a,b in zip(inputs,originals):torch.testing.assert_close(a,b,rtol=0,atol=0)

    def test_contract_roundtrip_and_refusals(self):
        _,inputs,meta=make_fixture("append_s8_prefill")
        value=make_contract(meta,inputs)
        self.assertEqual(loads(json.dumps(value)),value)
        mutations=[
            lambda x:x.update(schema="unsupported"),
            lambda x:x.update(extra=1),
            lambda x:x["shape"].update(batch=True),
            lambda x:x["paging"]["table"][0].__setitem__(0,999),
            lambda x:x["paging"]["write_slots"][0].__setitem__(0,0),
            lambda x:x["state"].update(result_version=1),
            lambda x:x["workload"].update(valid_lengths=[99,99]),
            lambda x:x["alignment"].update(global_guarantee_bytes=128),
            lambda x:x["capabilities"].update(tma="enabled"),
            lambda x:x["numerics"].update(compute_dtype="float16"),
        ]
        for change in mutations:
            broken=copy.deepcopy(value);change(broken)
            with self.subTest(value=broken):
                with self.assertRaises(ValueError):validate_contract(broken)
        with self.assertRaises(ValueError):loads(json.dumps(value).replace('"schema":','"schema":"duplicate","schema":',1))
        with self.assertRaises(ValueError):loads(json.dumps(value).replace('0.000244140625','NaN',1))

    def test_write_to_shared_or_reused_page_refused(self):
        _,inputs,meta=make_fixture("append_s8_decode")
        value=make_contract(meta,inputs)
        value["paging"]["table"][1][-1]=value["paging"]["table"][0][-1]
        value["paging"]["write_slots"][1]=value["paging"]["write_slots"][0][:]
        with self.assertRaises(ValueError):validate_contract(value)

    def test_native_mapper_is_not_silently_broadened(self):
        for name in CASES:
            model,inputs,_=make_fixture(name)
            self.assertEqual(probe_ingestion(model,inputs)["status"],"expected-unsupported")

    def test_source_receipt_replay_and_no_replace(self):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary)/"q0"
            report=write_artifacts(root)
            self.assertEqual(len(report["cases"]),len(CASES))
            self.assertFalse(report["native_quantized_whirl"])
            self.assertEqual(replay_artifacts(root)["status"],"PASS")
            with self.assertRaises(FileExistsError):write_artifacts(root)
            path=root/"dense_s8_decode/fx.graph.txt";path.write_text("corrupt")
            with self.assertRaises(ValueError):replay_artifacts(root)

if __name__=="__main__":
    unittest.main()
