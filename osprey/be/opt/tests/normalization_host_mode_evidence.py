"""Independent mode parity and complete mapped boundaries for LN4 closure.
Design: doc/AI-COMPILER-LAYERNORM-HOST-DISPATCH-v0.1.md.
Only the whirl2c timestamp banner is excluded from source parity comparisons.
"""
import difflib
import re
from normalization_plan_trace_report import require


def body(text):
    """Retain raw source; remove precisely its nondeterministic comment banner."""
    lines=text.splitlines(True)
    require(len(lines)>3 and lines[0].strip()=='/*******************************************************' and
        lines[2].strip()=='*******************************************************/' and
        re.fullmatch(r' \* C file translated from WHIRL [A-Za-z]{3} [A-Za-z]{3} [ 0-9]{2} [0-9:]{8} [0-9]{4}\n',lines[1]),
        'unexpected emitter banner')
    return ''.join(lines[:1]+lines[2:])


def complete_modes(build, source, output, records, run, parent=None):
    """Reopen/dump both service boundaries and prove identical emitted devices."""
    for row in records:
        case=output/row['case']
        run(case,'service-reopen',[build/'ir_tools/dsl_normalization_request_test','reopen',case/'service.after.B',
            case/'service.after.B.manifest.json',case/'service.reopen.t'])
        run(case,'dump-service-before',[build/'ir_tools/ir_b2a','-st','-src',case/'service.before.B',case/'service.before.T'])
        before=(case/'service.before.T').read_text();after=(case/'service.after.T').read_text()
        (case/'service.before-after.diff').write_text(''.join(difflib.unified_diff(before.splitlines(True),after.splitlines(True),fromfile='service.before.T',tofile='service.after.T')))
        run(case,'service-cuda',[build/'whirl2c/whirl2c','-cuda','-ft,'+str(case/'service-cuda.t'),
            '-CLIST:dotc_file=service.cu','-fB,'+str(case/'service.after.B'),source/'osprey/be/opt/tests/dsl_normalization_ref_test.cxx'])
        require(body((case/'service.cu').read_text())==body((case/'kernel.cu').read_text()),'mode changes device realization')
        if parent is not None:
            original=parent/'native'/row['case']
            require(body((original/'kernel.cu').read_text())==body((case/'kernel.cu').read_text()),'device source differs from accepted numerical producer')
            for name in ('kernel.ptx','kernel.cubin'):
                require((case/name).read_bytes()==(original/name).read_bytes(),'offline device instructions differ from accepted producer: '+name)
        require('OPR_DSLMEANVARNORM' in before and 'OPR_DSLMEANVARNORM' in after,'service source v1/v2 lost')
        # The original source PU portion is unchanged by either realization.
        for prefix in ('selected','service'):
            a=(case/(prefix+'.before.T')).read_text();b=(case/(prefix+'.after.T')).read_text()
            kernel=row['host'].replace('dispatch','ref')
            # Match the actual device boundary without assuming global ST indices.
            boundary=re.search(r'^FUNC_ENTRY <[^>]*,'+re.escape(kernel)+r'>',a,re.M)
            require(boundary is not None and a[:boundary.start()] in b,'analysis PU changed during mode materialization')
        with (case/'host-dispatch.log').open('a') as f:
            f.write('MODE_PARITY selected_device_source_exact_except_banner SERVICE mapped_before_after_and_native_reopen_verified ANALYSIS_PUS unchanged\n')
            if parent is not None:f.write('PARENT_DEVICE source_exact_except_banner PTX_exact CUBIN_exact NUMERICAL_EVIDENCE inherited_authenticated GPU not_run\n')
