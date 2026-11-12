"""Join LN4 driver request, actual selection, transformation and publication.

Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md.
Existing LN3/LN4 auditors retain ownership of costs, guards and body structure.
"""
from normalization_plan_trace_report import require
from normalization_lower_trace_report import audit as lower_audit, cuda_structure
from normalization_ref_trace_report import audit as source_audit
from normalization_cost_trace_report import row, COST, SELECT


def audit(text, manifest, reopened, cuda):
    """Reject foreign request/profile/source/choice/image joins, not just missing tags."""
    record=lower_audit(text,reopened)
    rows=[s.split() for s in text.splitlines() if s.startswith('AI_NORM_REQUEST ')]
    require(len(rows)==3 and [r[:3] for r in rows]==[['AI_NORM_REQUEST','1',k] for k in ('BIND','SELECT','PUBLISH')],
            'incomplete or duplicate driver lifecycle')
    request=manifest['request'];source=request['source'];selection=record['selection'];b,s,p=rows
    require(len(b)==9 and b[3:]==list(map(str,(source['owner_pu'],source['node_id'],request['target']['architecture'],
            request['profile']['model_id'],request['profile']['description_revision'],source['source_line']))),'foreign profile binding')
    require(len(s)==9 and list(map(int,s[3:]))==[source['owner_pu'],source['node_id'],manifest['selected_kind'],
            manifest['plan_seal'],manifest['cost'],source['source_line']],'foreign selected handoff')
    require(selection['selected']==manifest['selected_kind'] and selection['source_line']==source['source_line'] and
            record['facts'][3:5]==[manifest['plan_seal'],manifest['tree_seal']],'foreign actual body')
    require(len(p)==9 and p[3:7]==list(map(str,(source['owner_pu'],source['node_id'],manifest['kernel'],manifest['tree_seal']))) and
            int(p[-1])==source['source_line'],'foreign publication')
    require(manifest['schema']=='open64.layernorm.driver-review.v1' and manifest['runtime_authorized'] is False and
            manifest['gpu_verified'] is False,'compiler evidence upgraded to runtime proof')
    record['kernel_name']=manifest['kernel'];cuda_structure(cuda,record)
    return record


def reports(text, manifest, reopened, cuda, output):
    """Conventional compact phase reports keep remaining execution gates explicit."""
    record=audit(text,manifest,reopened,cuda);output.mkdir(parents=True,exist_ok=False)
    facts=source_audit(text)
    (output/'semantic.log').write_text('# What it summarizes: identical native normalization contracts at actual PREOPT entry/exit, including preserved v1 records.\n'+
        ''.join(' '.join(k+'='+str(f[k]) for k in ('pu','node','version','rows','width','epsilon_bits','reference','source_line'))+'\n' for f in facts)+
        'native_numerical_contract=LN-REF-1 v1_migration=none source_replacement=false\n')
    request=manifest['request'];source=request['source']
    request_log='# What it summarizes: explicit driver/JIT request admission, source/target/profile binding, cost gate and mapped-image publication.\n'
    request_log+='BIND_COLUMNS pu node arch model revision source_line\nSELECT_COLUMNS pu node kind plan_seal cost source_line\n'
    request_log+='PUBLISH_COLUMNS pu node kernel tree_seal image source_line\n'
    request_log+='\n'.join(s for s in text.splitlines() if s.startswith('AI_NORM_REQUEST '))+'\n'
    request_log+='PROFILE_FIELDS id provenance lower nominal upper (native scalar-profile units)\n'
    request_log+=''.join('PROFILE '+' '.join(map(str,[f['id'],f['provenance']]+f['bounds']))+'\n' for f in request['profile']['fields'])
    request_log+='v1=preserved reduction_dependency=false caller_measurements=not_authenticated runtime_authorized=false GPU=not_run\n'
    (output/'request.log').write_text(request_log)
    for name,prefixes in (('physical-plan.log',('AI_NORM_MODEL ','AI_NORM_COST ','AI_NORM_SELECT ','AI_NORM_CHOICE_GUARD ')),
                          ('lowering.log',('AI_NORM_LOWER ',))):
        (output/name).write_text('# What it summarizes: '+('complete candidate costs and cost-gated choice' if name.startswith('physical') else
                                'actual selected WHIRL actions and independent mapped identity')+'.\n'+
                                '\n'.join(s for s in text.splitlines() if s.startswith(prefixes))+'\n')
    (output/'optimization.log').write_text('# What it summarizes: full successful request, candidate/selection and WHIRL-transformation phase collection.\n'+
        ''.join((output/n).read_text() for n in ('semantic.log','request.log','physical-plan.log','lowering.log')))
    (output/'optimization-summary.log').write_text('# What it summarizes: driver/profile/selection/materialization coverage and open executable-admission gates.\n'+
        f"pu={source['owner_pu']} node={source['node_id']} kind=C{manifest['selected_kind']-1} cost={manifest['cost']} O3=true n64=true strict_FP=true\n"+
        'request=bound PREOPT=verified cost=complete selected=true WHIRL=materialized mapped_reopen=verified CUDA=emitted runtime_provider=missing GPU=not_run\n')
    return record


def refusal_reports(root, commands):
    """Summarize actual failed be invocations, retaining their raw diagnostics/traces."""
    from pathlib import Path
    summaries=[]
    for command in commands:
        if not command['returncode'] or Path(command['argv'][0]).name!='be':continue
        case=Path(command['cwd']);label=command['label'];trace=case/(label+'.t')
        text=trace.read_text() if trace.exists() else ''
        costs=[row(s,'AI_NORM_COST 1 COST',COST) for s in text.splitlines() if s.startswith('AI_NORM_COST 1 COST ')]
        choices=[row(s,'AI_NORM_SELECT 1 SELECT',SELECT) for s in text.splitlines() if s.startswith('AI_NORM_SELECT 1 SELECT ')]
        require(len(choices)<=1,'failed request contains ambiguous selections')
        selected=choices[0]['selected'] if choices else 0
        materialized='AI_NORM_LOWER 1 MATERIALIZE ' in text
        published='AI_NORM_REQUEST 1 PUBLISH ' in text
        diagnostic=(case/'logs'/(label+'.stderr')).read_text()
        reason=next((s for s in diagnostic.splitlines() if s.startswith(('LN4-REQUEST:','LN4-DRIVER:'))),
                    next((s for s in diagnostic.splitlines() if 'LN4 request requires' in s),'see retained diagnostic'))
        content='# What it summarizes: refused request, reached cost/selection gates, absence of executable lowering and retained failure reason.\n'
        content+=f'case={case.name} request={label} status=refused cost_records={len(costs)} complete_costs={sum(c["status"]==0 for c in costs)} selection_records={len(choices)} materialized={str(materialized).lower()}\n'
        if selected or materialized or published:
            content+=f'selected_kind={selected} published={str(published).lower()} driver_success=false runtime_authorized=false\n'
        content+='reason='+reason+'\nraw_trace='+trace.name+' raw_diagnostic=logs/'+label+'.stderr GPU=not_run\n'
        path=case/(label+'.request.log');require(not path.exists(),'refusal report already exists');path.write_text(content)
        summaries.append(content)
    path=root/'refused-requests.log';require(not path.exists(),'refusal collection already exists')
    path.write_text('# What it summarizes: all refused driver/profile requests and which optimization gates were reached.\n'+''.join(summaries))
    return len(summaries)
