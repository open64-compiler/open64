"""Join LN4 mode/selected-host observations to native body and emitted C evidence.
Design: doc/AI-COMPILER-LAYERNORM-HOST-DISPATCH-v0.1.md.
Native mapped ABI verification is independently required by the review runner.
"""
from normalization_plan_trace_report import require


def audit(text, manifest, host_source=None):
    """Reject ambiguous/mismatched modes, entries, identities and source lines."""
    rows=[s.split() for s in text.splitlines() if s.startswith('AI_NORM_HOST ')]
    mode=manifest['execution_mode'];host=manifest['host_dispatch'];source=manifest['request']['source']
    require(mode in ('service','standalone'),'unsupported execution mode')
    require(len(rows)==1 and rows[0]==list(map(str,('AI_NORM_HOST',1,'DISPATCH',mode,host or 'none',manifest['kernel'],
        manifest['plan_seal'],manifest['tree_seal'],source['source_line']))),'foreign or missing host dispatch record')
    require(bool(host)==(mode=='standalone'),'host PU presence contradicts mode')
    if mode=='standalone':
        expected=f"open64_ln_dispatch_{source['owner_pu']}_{source['node_id']}_c{manifest['selected_kind']-1}"
        require(host==expected,'host PU belongs to another source/choice')
        if host_source is not None:
            require(host_source.count(host+'(')==1 and host_source.count('open64_normalization_execution_run_selected(')==1,
                    'host definition/runtime call missing or duplicated')
            require(str(manifest['plan_seal'])+'ULL' in host_source and str(manifest['tree_seal'])+'ULL' in host_source,
                    'emitted selected constants changed')
            require('return dispatch_status;' in host_source and '__global__' not in host_source,
                    'host status propagation/device boundary changed')
    else:require(host_source is None,'service unexpectedly has generated host source')
    return dict(mode=mode,host=host,plan_seal=manifest['plan_seal'],tree_seal=manifest['tree_seal'],source_line=source['source_line'])
