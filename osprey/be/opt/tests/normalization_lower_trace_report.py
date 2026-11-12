"""LN4 raw decision/action and WHIRL-derived CUDA structural review.

Design: doc/AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md.
No reduction imports, source templates, or GPU performance assertions.
"""
import re
from normalization_plan_trace_report import require, integers
from normalization_cost_trace_report import row, COST, SELECT
from normalization_guard_trace_report import FIELDS


def comparison(text):
    """Separate actual LN3 rows from LN4 actions without interpreting actions as costs."""
    lines=text.splitlines();boundary=next((i for i,s in enumerate(lines) if s.startswith('AI_NORM_LOWER ')),len(lines))
    return '\n'.join(lines[:boundary])+'\n'


def prior_comparison(text,experiment,node):
    """Find one exact independently audited comparison in the authenticated LN3 matrix."""
    blocks=re.split(r'(?=^AI_NORM_EXPERIMENT 1 CASE )',text,flags=re.M)
    matches=[b for b in blocks if b.startswith('AI_NORM_EXPERIMENT 1 CASE '+experiment+' ') and
             any(s.startswith('AI_NORM_COST 1 COST ') and int(s.split()[4])==node for s in b.splitlines())]
    require(len(matches)==1,'missing/ambiguous prior source-bound cost comparison')
    return matches[0]


def audit(text,reopened=None):
    """Join selected kind/guard/prepare/materialize/verify and exact action order."""
    lines=text.splitlines();selection=[row(s,'AI_NORM_SELECT 1 SELECT',SELECT) for s in lines if s.startswith('AI_NORM_SELECT 1 SELECT ')]
    require(len(selection)==1,'one selected source member required');s=selection[0]
    costs=[row(a,'AI_NORM_COST 1 COST',COST) for a in lines if a.startswith('AI_NORM_COST 1 COST ')]
    require(len(costs)==3 and [a['kind'] for a in costs]==[1,2,3],'three comparable candidates required')
    lower=[a.split() for a in lines if a.startswith('AI_NORM_LOWER ')]
    if not s['selected']:
        require(not lower and reopened is None,'refused/incomplete source materialized')
        return dict(selection=s,guard=None,materialized=False)
    require(lower and lower[0][:3]==['AI_NORM_LOWER','1','PREPARE'] and len(lower[0])==9,'invalid preparation')
    prep=integers(lower[0][3:]);c=costs[s['selected']-1]
    require(c['status']==0 and prep[:3]==[s['pu'],s['node'],s['selected']] and prep[4:]==[c['total'],s['source_line']],'unbound preparation')
    guards=[a.split() for a in lines if a.startswith('AI_NORM_CHOICE_GUARD 1 EXPORT ')]
    require(len(guards)==1 and guards[0][3]=='83' and len(guards[0])==88,'invalid selected guard')
    guard=dict(zip(FIELDS,integers(guards[0][4:-1])))
    require(guard['kind']==s['selected'] and guard['node']==s['node'] and guard['source_line']==s['source_line'],'foreign selected guard')
    actual=[a for a in lower if a[2] in ('MATERIALIZE','VERIFY')]
    require(len(actual)==2 and [a[2] for a in actual]==['MATERIALIZE','VERIFY'] and actual[0][3:]==actual[1][3:],'missing actual materialization/verification')
    facts=integers(actual[0][3:]);require(len(facts)==12 and facts[:4]==prep[:4] and facts[-3:]==[guard['reserved_shared'],guard['threads'],s['source_line']],'stale device binding')
    kind=s['selected'];require(facts[6:9]==[3 if kind==1 else 4 if kind==2 else 5,13 if kind==1 else 12,0 if kind==1 else 4 if kind==2 else 5],'wrong loop/arithmetic/barrier recipe')
    actions=['FORMALS_SUBRANGE_X_GAMMA_BETA_Y','PRIVATE_CARRIER_3_F32']
    if kind!=1:actions+=['SHARED_ARENA']
    if kind==3:actions+=['X_COPY_TO_SHARED','ROW_VISIBLE_BARRIER']
    actions+=['MEAN_ASCENDING_F32']
    if kind!=1:actions+=['MU_PUBLISH_BARRIER','CENTER_FROM_X' if kind==2 else 'CENTER_X_TO_D_IN_PLACE','CENTER_VISIBLE_BARRIER']
    actions+=['VARIANCE_ASCENDING_F32']
    if kind!=1:actions+=['Q_PUBLISH_BARRIER']
    actions+=['AFFINE_POINTWISE_F32']
    if kind!=1:actions+=['READERS_RELEASE_BARRIER']
    events=[a for a in lower if a[2]=='ACTION']
    require([a[4] for a in events]==actions and len(lower)==3+len(events),'wrong actual action sequence')
    for a in events:
        require(len(a)==7 and integers([a[3],a[5],a[6]])==[kind,guard['reserved_shared'] if a[4]=='SHARED_ARENA' else 0,s['source_line']],'invalid action binding')
    if reopened is not None:require(reopened.splitlines()==[' '.join(actual[1])],'mapped identity drift')
    return dict(selection=s,guard=guard,facts=facts,actions=actions,materialized=True)


def cuda_structure(text,record):
    """Inspect actual emission: uniform barrier depth, exact scalar forms and layout."""
    g=record['guard'];kind=g['kind'];name='ln_ref_c'+str(kind-1)
    match=re.search(r'extern "C" __global__ void '+name+r'\(\s*_IEEE32 \* x,\s*_IEEE32 \* gamma,\s*_IEEE32 \* beta,\s*_IEEE32 \* y\)\s*\{(.*)\}\s*/\* '+name+r' \*/',text,re.S)
    require(match is not None,'missing exact kernel/formal ABI');body=match[1]
    require('__restrict' not in text and 'OPR_DSL' not in text,'invented alias/lowering semantics')
    shared=re.findall(r'__shared__ __align__\(16\) _IEEE32 ln_shared\[(\d+)LL\];',body)
    require(shared==([] if kind==1 else [str(g['reserved_shared']//4)]),'wrong shared reservation')
    require(len(re.findall(r'\bfor\(',body))==record['facts'][6] and body.count('__syncthreads();')==record['facts'][8],'wrong loops/barriers')
    require('_UINT64)(ln_row)' in body and (g['width']==1 or '* '+str(g['width'])+'ULL' in body),'row address not widened')
    asm=re.findall(r'__asm__ volatile \((.*?)\);',body,re.S)
    require(len(asm)==record['facts'][7],'missing scalar carriers')
    for a in asm:
        require(': "memory"' in a and '"l"( ln_out_ptr)' in a and '"l"( ln_left_ptr)' in a,'lost scalar effects/operands')
        ops=re.findall(r'\b(add|sub|mul|div|sqrt)\.rn\.f32\b',a);require(len(ops)==1,'wrong arithmetic recipe')
        require('ftz' not in a and (ops[0]=='sqrt')==('ln_right_ptr' not in a),'unsafe scalar carrier')
    require(sum('sqrt.rn.f32' in a for a in asm)==1 and sum('div.rn.f32' in a for a in asm)==3,'lost finalize rounding')
    # Ignore quoted PTX braces while checking that no barrier is divergent.
    clean=re.sub(r'"(?:\\.|[^"\\])*"','""',body);depth=0
    for token in re.findall(r'__syncthreads\(\);|[{}]',clean):
        if token=='{':depth+=1
        elif token=='}':depth-=1
        else:require(depth==0,'barrier nested in divergent control')
    require(depth==0,'unbalanced emitted body')
    return body


def write_reports(text,reopened,cuda,output):
    """Conventional human logs distinguish actual lowering from remaining gates."""
    r=audit(text,reopened);output.mkdir(parents=True,exist_ok=False)
    content='# What it summarizes: actual selected-device WHIRL materialization, exact action order, mapped identity and CUDA structure.\n'
    content+='PREPARE_COLUMNS pu node kind plan_seal modeled_total source_line\n'
    content+='DEVICE_COLUMNS pu node kind plan_seal tree_seal nodes loops scalar_operations barriers reserved_shared threads source_line\n'
    content+='ACTION_COLUMNS kind action reserved_shared_if_allocation source_line\n'
    content+='\n'.join(s for s in text.splitlines() if s.startswith('AI_NORM_LOWER '))+'\n'
    if r['materialized']:cuda_structure(cuda,r)
    content+=f"source_v1=preserved materialized={str(r['materialized']).lower()} source_replacement=false GPU=not_run production_adapter=missing\n"
    (output/'lowering.log').write_text(content)
    (output/'optimization.log').write_text('# What it summarizes: full successful selected-device lowering report collection.\n'+content)
    (output/'optimization-summary.log').write_text('# What it summarizes: selected/refused source, actual materialization and remaining executable integration gates.\n'+
        f"selected={r['selection']['selected']} materialized={str(r['materialized']).lower()} v1=preserved reduction_dependency=false\n"+
        'WHIRL_authoritative=true default_driver_dispatch=not_connected actual_RF_admission=requires_compile_audit runtime_provider=missing GPU=not_run\n')
    return r
