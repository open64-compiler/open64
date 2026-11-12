import json
import math
from pathlib import Path
import struct
import sys
import tempfile
import unittest

sys.path.insert(0,str(Path(__file__).resolve().parents[1]/"server"))
import data
import run


class ScaffoldTests(unittest.TestCase):
    def test_bf16_round_to_nearest_even(self):
        self.assertEqual(data.bf16_bits(1.0+1/256),0x3F80)
        self.assertEqual(data.bf16_bits(1.0+3/256),0x3F82)
        self.assertEqual(data.bf16_value(data.bf16_bits(-2.5)),-2.5)

    def test_gemm_oracle_hand_computed(self):
        p=dict(m=2,n=2,k=2,lda=2,ldb=2,ldc=2,trans_a=0,trans_b=0,alpha=2,beta=0.5)
        self.assertEqual(data.gemm_reference(p,[1,2,3,4],[5,6,7,8],[2]*4),[39,45,87,101])

    def test_gemm_transpose_and_padding(self):
        p=dict(m=2,n=2,k=2,lda=3,ldb=3,ldc=3,trans_a=1,trans_b=1,alpha=1,beta=0)
        self.assertEqual(data.gemm_reference(p,[1,3,99,2,4,99],[5,7,99,6,8,99],[0]*6),[19,22,43,50])

    def test_mla_uniform_scores_paged_mean(self):
        p=dict(batch=1,heads=1,latent_dim=2,rope_dim=1,page_size=2,max_pages=2,scale=1)
        # Physical pages reversed; padding token must be ignored.
        kv=[5,6,0,99,99,0,1,2,0,3,4,0]
        self.assertEqual(data.mla_reference(p,[0,0,0],kv,[1,0],[3]),[3,4])

    def test_mla_stable_softmax(self):
        p=dict(batch=1,heads=1,latent_dim=1,rope_dim=0,page_size=2,max_pages=1,scale=1)
        self.assertTrue(math.isfinite(data.mla_reference(p,[1000],[1,2],[0],[2])[0]))

    def test_corpus_is_deterministic_and_tampering_detected(self):
        with tempfile.TemporaryDirectory() as a,tempfile.TemporaryDirectory() as b:
            for kind in ["gemm","mla"]:
                x=data.generate(kind,a)
                y=data.generate(kind,b)
                self.assertEqual((x/"index.json").read_bytes(),(y/"index.json").read_bytes())
                index=data.verify(x)
                for case in index["cases"]:
                    meta=json.loads((x/case["case_id"]/"case.json").read_text())
                    golden=(x/case["case_id"]/"golden.bin").read_bytes()
                    self.assertEqual(len(golden),meta["output_elements"]*8)
                    self.assertTrue(all(math.isfinite(v) for v in struct.unpack("<"+"d"*meta["output_elements"],golden)))
            target=x/index["cases"][0]["case_id"]/"golden.bin"
            target.write_bytes(b"corrupted")
            with self.assertRaisesRegex(ValueError,"integrity"):
                data.verify(x)

    def test_ratio_direction_and_throughput(self):
        meta=dict(case_id="example",useful_flops=1000000000)
        native=dict(status="passed",correctness=dict(baseline_pass=True,candidate_pass=True),
                    baseline=dict(wall_us=100,device_us=80),candidate=dict(wall_us=75,device_us=50))
        result=run.case_result(meta,native)
        self.assertEqual(result["wall_time_ratio"],0.75)
        self.assertEqual(result["baseline"]["effective_tflops"],12.5)
        self.assertEqual(result["candidate"]["effective_tflops"],20)

    def test_failed_case_has_no_ratio(self):
        result=run.case_result(dict(case_id="x",useful_flops=10),dict(status="runtime_failed",reason="timeout"))
        self.assertIsNone(result["wall_time_ratio"])
        self.assertIsNone(result["candidate"])

    def test_nonpositive_timing_rejected(self):
        with self.assertRaisesRegex(ValueError,"timing"):
            run.case_result(dict(case_id="x",useful_flops=10),dict(status="passed",baseline=dict(wall_us=0,device_us=1)))

    def test_submission_contracts(self):
        for kind in ["gemm","mla"]:
            _,manifest,actual,_=run.read_submission(data.ROOT/"submissions"/kind)
            self.assertEqual(actual,kind)
            self.assertEqual(manifest["baseline"],{"source":"baseline.cu"})


if __name__=="__main__":
    unittest.main()
