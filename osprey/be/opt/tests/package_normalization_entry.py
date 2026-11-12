"""Package audited LN4 review evidence as an exact compiled-entry descriptor.

Design: doc/AI-COMPILER-LAYERNORM-COMPILED-ENTRY-v0.1.md.
Review/deployment-issuer tool, not an optimizer, code generator or signing service.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import struct

from normalization_request_trace_report import audit
from normalization_guard_trace_report import FIELDS
from normalization_plan_trace_report import require
from normalization_cost_trace_report import row, COST
from normalization_host_trace_report import audit as host_audit


def sha(path):
    """Authenticate exact input bytes before reuse, not a current-link identity."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def authenticate(receipt):
    """Require complete accepted primary evidence; any checksum drift refuses."""
    receipt=receipt.resolve()
    metadata=json.loads((receipt/'receipt.json').read_text())
    require(metadata['schema'] in ('open64-ln4-driver-request-review-v1','open64-ln4-host-dispatch-review-v1') and metadata['source_v1_preserved'] is True and
            metadata['GPU'] is False,'unsupported producer receipt')
    for line in (receipt/'SHA256SUMS').read_text().splitlines():
        digest,name=line.split('  ',1);path=receipt/name
        require(receipt in path.resolve().parents and sha(path)==digest,'receipt checksum mismatch: '+name)
    require(sha(receipt/metadata['primary_artifact'])==metadata['primary_sha256'],'primary receipt identity mismatch')
    return receipt


def package(case, output, parent):
    """Join selected native semantics/body and exact assembly resources before writing.

    Caller authenticates the producer. Fresh destination only; this review tool
    never publishes a runtime authorization or substitutes a kernel template.
    """
    manifest=json.loads((case/'selected.after.B.manifest.json').read_text())
    trace=(case/'driver.t').read_text();cuda=(case/'kernel.cu').read_text()
    record=audit(trace,manifest,(case/'reopen.t').read_text(),cuda)
    g=record['guard'];kind=manifest['selected_kind'];request=manifest['request']
    costs=[row(s,'AI_NORM_COST 1 COST',COST) for s in trace.splitlines() if s.startswith('AI_NORM_COST 1 COST ')]
    selected=costs[kind-1];require(selected['status']==0 and selected['total']==manifest['cost'],'unbound complete selected cost')
    compilation=(case/'logs/cubin.stderr').read_text();arch=request['target']['architecture'];kernel=manifest['kernel']
    require(kernel==f"open64_ln_ref_{g['pu']}_{g['node']}_c{kind-1}",'unexpected selected-entry name')
    entries=re.findall(r"Compiling entry function '([^']+)' for '([^']+)'",compilation)
    registers=re.findall(r'Used (\d+) registers',compilation);shared=re.findall(r'(\d+) bytes smem',compilation)
    require(entries==[(kernel,arch)] and len(registers)==1 and len(shared)<=1,'ambiguous/foreign assembly report')
    registers=int(registers[0]);shared=int(shared[0]) if shared else 0
    upper=request['registers_per_thread'][kind-1]
    require(0<registers<=upper and shared==g['reserved_shared']<=49152,'compiled resources require recost')
    spills=re.findall(r'(\d+) bytes stack frame, (\d+) bytes spill stores, (\d+) bytes spill loads',compilation)
    require(len(spills)==1 and int(spills[0][0])<=16 and spills[0][1:]==('0','0'),'extra stack/spill traffic requires recost')
    cubin=(case/'kernel.cubin').read_bytes()
    require(20<=len(cubin)<=64*1024*1024 and cubin[:6]==b'\x7fELF\x02\x01' and
            struct.unpack_from('<H',cubin,18)[0]==190,'not a little-endian ELF64 CUDA cubin')
    words=[g[k] for k in FIELDS];digest=hashlib.sha256(cubin).hexdigest()
    result=dict(schema='open64.layernorm.compiled-review.v1',parent_receipt=parent,kernel=kernel,
        parent_receipt_sha256=sha(Path(parent)/'receipt.json') if (Path(parent)/'receipt.json').is_file() else None,
        guard_words=words,plan_seal=manifest['plan_seal'],tree_seal=manifest['tree_seal'],registers_upper=upper,
        compiled_registers=registers,static_shared=shared,resident_floor=selected['resident'],cubin_bytes=len(cubin),
        cubin_sha256=digest,source_line=g['source_line'],architecture=arch,profile=request['profile'],
        evidence={name:sha(case/name) for name in ('driver.t','reopen.t','request.json','selected.after.B.manifest.json',
            'selected.before.B','selected.before.T','selected.after.B','selected.after.T','selected.before-after.diff',
            'kernel.cu','kernel.ptx','kernel.cubin','logs/cubin.stderr')},GPU=False,runtime_authorized=False)
    host_files={}
    if 'execution_mode' in manifest:
        mode=manifest['execution_mode'];host=manifest['host_dispatch']
        host_audit(trace,manifest,(case/'host.c').read_text() if mode=='standalone' else None)
        result.update(execution_mode=mode,host_dispatch=host)
        if mode=='standalone':
            host_files={name:(case/name).read_bytes() for name in ('host.c','host.h')}
            require(b'#include "host.h"' in host_files['host.c'],'host source is not a relocatable package')
            host_files['whirl2c.h']=(Path(__file__).resolve().parents[3]/'include/whirl2c.h').read_bytes()
            result['host_evidence']={name:hashlib.sha256(data).hexdigest() for name,data in host_files.items()}
    identity={k:result[k] for k in ('guard_words','plan_seal','tree_seal','registers_upper','compiled_registers',
                                  'static_shared','resident_floor','cubin_bytes','cubin_sha256')}
    symbol='open64_norm_entry_'+hashlib.sha256(json.dumps(identity,sort_keys=True).encode()).hexdigest()[:16]
    lines=['/* Audited LN4 review descriptor; no kernel code is generated here.',
           ' * Design: doc/AI-COMPILER-LAYERNORM-COMPILED-ENTRY-v0.1.md. */',
           '#include "open64_normalization_cuda.h"','#include <string.h>',
           '/* Return an owned native descriptor; runtime must still load/query/admit. */',
           'static OPEN64_NORMALIZATION_COMPILED_ENTRY '+symbol+'()', '{',
           '  OPEN64_NORMALIZATION_COMPILED_ENTRY p={};',
           '  const uint64_t words[]={'+','.join(str(v)+'ULL' for v in words)+'};',
           '  static_assert(sizeof(p.guard)==sizeof(words),"native guard layout changed");',
           '  memcpy(&p.guard,words,sizeof(words)); p.version=1; p.size=sizeof(p);']
    for name in ('plan_seal','tree_seal','registers_upper','compiled_registers','static_shared','resident_floor','cubin_bytes'):
        lines.append('  p.'+name+'='+str(result[name])+'ULL;')
    lines+=['  const unsigned char digest[]={'+','.join(str(v) for v in bytes.fromhex(digest))+'};',
            '  memcpy(p.cubin_sha256,digest,sizeof(digest));',
            '  memcpy(p.kernel,"'+kernel+'",sizeof("'+kernel+'"));','  return p;','}']
    output.mkdir(parents=True,exist_ok=False)
    (output/'entry.h').write_text('\n'.join(lines)+'\n');(output/'kernel.cubin').write_bytes(cubin)
    for name,data in host_files.items():(output/name).write_bytes(data)
    (output/'selected.guard').write_bytes(struct.pack('<83Q',*words))
    result['header_symbol']=symbol
    (output/'entry.json').write_text(json.dumps(result,indent=2,sort_keys=True)+'\n')
    content='# What it summarizes: source-bound selected entry, authenticated evidence, exact cubin identity and assembly RF/shared reservations; not GPU certification.\n'
    content+='ENTRY_COLUMNS kernel kind registers upper static_shared resident_floor source_line\n'
    content+=f"ENTRY {kernel} {kind} {registers} {upper} {shared} {selected['resident']} {g['source_line']}\n"
    content+=f"SEALS {manifest['plan_seal']} {manifest['tree_seal']}\nCOST {selected['total']} REASON {record['selection']['reason']}\n"
    content+='PARENT_SHA256 '+str(result['parent_receipt_sha256'])+'\n'
    content+='CUBIN_SHA256 '+digest+'\nWHIRL_AUTHORITATIVE true SOURCE_V1 preserved GPU not_run RUNTIME_AUTHORIZED false\n'
    (output/'compiled-entry.log').write_text(content)
    return result


def main():
    """Require authenticated retained producer evidence for command-line packaging."""
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--receipt',type=Path,required=True)
    p.add_argument('--case',required=True);p.add_argument('--output',type=Path,required=True);a=p.parse_args()
    receipt=authenticate(a.receipt)
    require(re.fullmatch(r'sm_(?:90|90a|100|100a)-[a-z-]+-[123]',a.case) is not None,'invalid case name')
    metadata=json.loads((receipt/'receipt.json').read_text())
    producer='review' if metadata['schema']=='open64-ln4-host-dispatch-review-v1' else 'native'
    known=json.loads((receipt/producer/'audit.json').read_text())['records']
    require(sum(r['case']==a.case for r in known)==1,'case not in accepted producer census')
    package(receipt/producer/a.case,a.output,str(receipt))


if __name__=='__main__':main()
