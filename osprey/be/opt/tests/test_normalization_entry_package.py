"""Synthetic package boundary tests; real driver/assembly evidence is separate.
Design: doc/AI-COMPILER-LAYERNORM-COMPILED-ENTRY-v0.1.md.
"""
import hashlib
import json
from pathlib import Path
import struct
import tempfile
import unittest
from unittest.mock import patch

from package_normalization_entry import package, authenticate
from normalization_lower_trace_report import audit as lower_audit
from test_normalization_request_trace_report import request_fixture
from run_normalization_entry_review import audit_trace
from normalization_guard_trace_report import FIELDS


def source(root, kind=3):
    """Minimal fake byte inputs isolate package checks, not CUDA/body certification."""
    case=root/'case';(case/'logs').mkdir(parents=True)
    text,m,reopened=request_fixture(kind);record=lower_audit(text,reopened)
    old=m['kernel'];m['kernel']=f'open64_ln_ref_11_7_c{kind-1}';text=text.replace(old,m['kernel'])
    m['request']['registers_per_thread']=[32,32,32]
    for name,content in (('driver.t',text),('reopen.t',reopened),('selected.after.B.manifest.json',json.dumps(m))):
        (case/name).write_text(content)
    for name in ('request.json','selected.before.B','selected.before.T','selected.after.B','selected.after.T',
                 'selected.before-after.diff','kernel.cu','kernel.ptx'):(case/name).write_text('synthetic fixture')
    (case/'kernel.cubin').write_bytes(b'\x7fELF\x02\x01'+b'\0'*12+struct.pack('<H',190))
    (case/'logs/cubin.stderr').write_text(f"Compiling entry function '{m['kernel']}' for 'sm_90'\n"
        f"16 bytes stack frame, 0 bytes spill stores, 0 bytes spill loads\nUsed 31 registers, {record['guard']['reserved_shared']} bytes smem\n")
    return case,record


class EntryPackageTests(unittest.TestCase):
    """Only CUDA-structure auditing is replaced by an explicitly synthetic record."""
    def test_all_three_native_descriptors(self):
        """Exact guard bytes, digest, resource reservation and explicit evidence limits."""
        for kind in (1,2,3):
            with tempfile.TemporaryDirectory() as directory:
                root=Path(directory);case,record=source(root,kind)
                with patch('package_normalization_entry.audit',return_value=record):
                    result=package(case,root/'out','synthetic')
                self.assertEqual(result['compiled_registers'],31);self.assertEqual(len(result['guard_words']),83)
                self.assertEqual(result['cubin_sha256'],hashlib.sha256((case/'kernel.cubin').read_bytes()).hexdigest())
                self.assertFalse(result['GPU']);self.assertFalse(result['runtime_authorized'])
                self.assertEqual((root/'out/selected.guard').stat().st_size,664)
                self.assertTrue((root/'out/compiled-entry.log').read_text().startswith('# What it summarizes:'))

    def test_resource_and_symbol_refusals(self):
        """Foreign/ambiguous entries, extra scratch traffic and reservation overflow refuse."""
        for old,new in (('Used 31','Used 33'),('0 bytes spill loads','4 bytes spill loads'),
                        ('16 bytes stack','32 bytes stack'),('sm_90','sm_100'),
                        ('open64_ln_ref_11_7_c2','foreign'),('256 bytes smem','99999 bytes smem')):
            with tempfile.TemporaryDirectory() as directory:
                root=Path(directory);case,record=source(root);log=case/'logs/cubin.stderr'
                text=log.read_text()
                if old=='256 bytes smem':old=str(record['guard']['reserved_shared'])+' bytes smem'
                self.assertIn(old,text)
                log.write_text(text.replace(old,new))
                with patch('package_normalization_entry.audit',return_value=record),self.assertRaises(ValueError):package(case,root/'out','synthetic')
                self.assertFalse((root/'out').exists())

    def test_foreign_cost_or_kernel(self):
        """A complete cost cannot be borrowed for another selected entry or handoff."""
        for key,value in (('cost',0),('kernel','unsafe"name')):
            with tempfile.TemporaryDirectory() as directory:
                root=Path(directory);case,record=source(root);path=case/'selected.after.B.manifest.json'
                m=json.loads(path.read_text());m[key]=value;path.write_text(json.dumps(m))
                with patch('package_normalization_entry.audit',return_value=record),self.assertRaises(ValueError):package(case,root/'out','synthetic')

    def test_cubin_only(self):
        """PTX, fatbinary, wrong-endian/wrong-machine and short blobs cannot hide JIT work."""
        for payload in (b'.version 8.7',b'FATBIN'*4,b'\x7fELF\x02\x02'+b'\0'*14,b'\x7fELF\x02\x01'+b'\0'*14):
            with tempfile.TemporaryDirectory() as directory:
                root=Path(directory);case,record=source(root);(case/'kernel.cubin').write_bytes(payload)
                with patch('package_normalization_entry.audit',return_value=record),self.assertRaises(ValueError):package(case,root/'out','synthetic')

    def test_fresh_destination(self):
        """Repeated packaging never overwrites a previously reviewed descriptor."""
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);case,record=source(root)
            with patch('package_normalization_entry.audit',return_value=record):
                package(case,root/'out','synthetic');before=(root/'out/entry.h').read_bytes()
                with self.assertRaises(FileExistsError):package(case,root/'out','synthetic')
            self.assertEqual((root/'out/entry.h').read_bytes(),before)

    def test_receipt_authentication(self):
        """Drift and escaping checksum paths fail before using producer artifacts."""
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory);receipt=root/'receipt';receipt.mkdir();data=receipt/'evidence-index.json';data.write_text('{}')
            digest=hashlib.sha256(data.read_bytes()).hexdigest()
            (receipt/'receipt.json').write_text(json.dumps(dict(schema='open64-ln4-driver-request-review-v1',
                source_v1_preserved=True,GPU=False,primary_artifact=data.name,primary_sha256=digest)))
            sums=receipt/'SHA256SUMS';sums.write_text(digest+'  '+data.name+'\n')
            self.assertEqual(authenticate(receipt),receipt.resolve());data.write_text('changed')
            with self.assertRaises(ValueError):authenticate(receipt)
            data.write_text('{}');(root/'outside').write_text('{}');sums.write_text(digest+'  ../outside\n')
            with self.assertRaises(ValueError):authenticate(receipt)

    def test_provider_trace_tampering(self):
        """Missing checks/refusals, foreign resource identity and post-summary rows refuse."""
        words=[0]*83
        for key,value in (('threads',128),('ptx',807),('architecture',1)):words[FIELDS.index(key)]=value
        expected=dict(source_line=211,plan_seal=42,tree_seal=99,compiled_registers=31,static_shared=256,guard_words=words,resident_floor=16)
        text=''.join(f'AI_NORM_CUDA_TEST 1 FAULT {i} 211\n' for i in range(1,32))
        events=[('attribute',0,k,v) for k,v in ((4,31),(1,256),(0,1024),(6,90),(5,87),(10,0))]
        events+=[('resources',0,31,256),('resident',0,16,16),('open',0,42,99),('pinned',4,0,0),('perform',0,1,1),('perform',5,11,1),('unload',0,0,0)]
        text+=''.join(f'AI_NORM_CUDA 1 EVENT 1 {i} {action} {status} {a} {b} 211\n' for i,(action,status,a,b) in enumerate(events,1))
        text+='AI_NORM_CUDA_TEST 1 CHECK synthetic 1 211\n'*101+'AI_NORM_CUDA_TEST 1 SUMMARY 101 1 1 1 1 1 host_mock_only 211\n'
        self.assertEqual(audit_trace(text,expected)['checks'],101)
        for old,new in (('FAULT 1 211\n','FAULT 2 211\n'),('resources 0 31','resources 0 32'),
                        ('SUMMARY 101','SUMMARY 102'),('CHECK synthetic 1','CHECK synthetic 0')):
            with self.assertRaises(ValueError):audit_trace(text.replace(old,new),expected)
        with self.assertRaises(ValueError):audit_trace(text+'AI_NORM_CUDA_TEST 1 CHECK extra 1 211\n',expected)


if __name__=='__main__':unittest.main()
