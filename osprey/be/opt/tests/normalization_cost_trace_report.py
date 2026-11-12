"""Strict LN3 native/plan/model/schedule/choice joins and conventional reports.

Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md.
No reduction imports; profile assumptions never become measured GPU evidence.
"""
from normalization_plan_trace_report import audit as plan_audit, require, integers
from normalization_guard_trace_report import FIELDS as GUARD_FIELDS, decode
from normalization_cost_oracle import FIELDS, MAX, profile_status, residents, recipe, evaluate, choose, ceildiv

RESOURCE='arch revision sms blocks warps threads threads_cta registers max_regs reg_quantum reg_warps shared_cap shared_budget shared_quantum shared_reserved source_line'.split()
MODEL='version arch revision model regs0 regs1 regs2 max_jobs margin source_line'.split()
COST=('pu node result kind plan_status status resident jobs global_logical global_sectors carrier_sectors shared scalar_lanes scalar_insts control barriers '
      'lower total upper compute memory_unhidden communication_unhidden sync launch guard candidate_id plan_id source_line').split()
COST_HEADER='AI_NORM_COST 1 HEADER '+ ' '.join('cost_status' if c=='status' else c for c in COST)
SELECT='pu node result nominal selected fallback reason common_plan analysis_only source_line'.split()
SCENARIOS=('builtin balanced global_slow shared_slow uncertain missing wrong_revision budget rf_unknown rf_exhausted overflow margin tie rf_pressure').split()


def row(line,prefix,columns):
    """Reject noncanonical numbers, narrowed fields and positional drift."""
    words=line.split();require(words[:3]==prefix.split() and len(words)==3+len(columns),'invalid '+prefix+' columns')
    return dict(zip(columns,integers(words[3:])))


def expected_cost(p,hw,model,values,provenance):
    """Independent legality/resource/work/event/corner reconstruction."""
    output=dict.fromkeys(COST,0)
    for k in ('pu','node','result','kind','source_line'):output[k]=int(p[k])
    output['plan_status']=int(p['status'])
    if output['plan_status']:output['status']=1;return output
    status=profile_status(values,provenance,tuple(model[k] for k in ('version','arch','revision','model')),hw)
    output['status']=status
    if status:return output
    regs=model['regs'+str(output['kind']-1)]
    resident=residents(hw,int(p['threads']),int(p['warp']),int(p['reserved']),regs)
    output['resident']=resident
    if not resident:output['status']=6 if regs else 2;return output
    rows=p['ranges'][0]['bytes']//int(p['row_bytes']);width=int(p['row_bytes'])//4
    budget=model['max_jobs']
    if not budget or budget>1000000 or rows>budget or width>budget//rows:
        output['status']=4;return output
    shape=(rows,width,output['kind'],int(p['threads']),int(p['warp']),hw['sms'],resident,values[16][1],int(p['stride']),
           tuple(a['offset'] for a in p['ranges']),tuple(a['alignment'] for a in p['ranges']))
    jobs,counts=recipe(*shape)
    if len(jobs)>budget:output['status']=4;return output
    output.update(counts,jobs=len(jobs));result=evaluate(shape,tuple(tuple(v) for v in values),int(p['conflict']))
    if result['overflow']:output['status']=5;return output
    output.update(lower=result['lower'],total=result['total'],upper=result['upper'])
    for name,value in zip(('compute','memory_unhidden','communication_unhidden','sync','launch','guard'),result['terms']):output[name]=value
    return output


def audit(text,plan_text,source_text,guard_images,scenarios=SCENARIOS,source_keys=None):
    """Authenticate source joins and every cost/decision, including no-choice cases."""
    require(scenarios and len(set(scenarios))==len(scenarios) and set(scenarios)<=set(SCENARIOS),'invalid explicit experiment scope')
    plans=plan_audit(plan_text,source_text);baseline={}
    for p in plans:
        if p['kind'] in ('1','2','3'):baseline.setdefault(tuple(int(p[k]) for k in ('pu','node','result','kind')),p)
    guards={}
    for path in guard_images:
        g=decode(path.read_bytes());guards[(g['pu'],g['node'],g['result'],g['kind'])]=g
    lines=text.splitlines();index=0;results=[];seen=set();hardware=None
    while index<len(lines):
        require(len(lines)-index>=11+len(FIELDS),'truncated comparison')
        case=lines[index].split();require(len(case)==5 and case[:3]==['AI_NORM_EXPERIMENT','1','CASE'] and case[3] in SCENARIOS,'unknown experiment schema')
        name=case[3];source_line=integers(case[4:])[0];index+=1
        hw=row(lines[index],'AI_NORM_RESOURCE 1 BIND',RESOURCE);index+=1
        origin=lines[index].split();index+=1
        names={1:'sm90_estimated_v2',2:'sm90a_estimated_v2',3:'sm100_estimated_v2',4:'sm100a_estimated_v2'}
        require(origin==['AI_NORM_RESOURCE','1','ORIGIN',names.get(hw['arch'],''),'reference_cycles','estimated',str(source_line)],'unbound resource provenance')
        identity={k:v for k,v in hw.items() if k!='source_line'}
        if hardware is None:hardware=identity
        require(identity==hardware and hw['source_line']==source_line and all(hw[k]>0 for k in RESOURCE if k not in ('shared_reserved','source_line')),'inconsistent hardware facts')
        require(lines[index]=='AI_NORM_MODEL 1 HEADER '+' '.join(MODEL),'invalid model header');index+=1
        model=row(lines[index],'AI_NORM_MODEL 1 BIND',MODEL);index+=1
        require(model['source_line']==source_line and all(model[k]<=2**32-1 for k in ('version','arch','revision','model','regs0','regs1','regs2')),'invalid model identity')
        values=[];provenance=[]
        for field in range(len(FIELDS)):
            a=row(lines[index],'AI_NORM_MODEL 1 FIELD','field provenance low nominal high source_line'.split());index+=1
            require(a['field']==field and a['source_line']==source_line,'invalid model field order/source')
            values.append([a[k] for k in ('low','nominal','high')]);provenance.append(a['provenance'])
        require(lines[index]==COST_HEADER,'invalid cost header');index+=1
        costs=[]
        for kind in (1,2,3):
            c=row(lines[index],'AI_NORM_COST 1 COST',COST);index+=1
            key=tuple(c[k] for k in ('pu','node','result','kind'))
            require(c['kind']==kind and key in baseline and c['source_line']==source_line,'foreign cost identity')
            p=baseline[key]
            require(hw['arch']=={'sm_90':1,'sm_90a':2,'sm_100':3,'sm_100a':4}[plan_text.splitlines()[0].split()[3]] and
                    hw['threads_cta']==int(plan_text.splitlines()[0].split()[-2]),'foreign hardware binding')
            expected=expected_cost(p,hw,model,values,provenance)
            require(all(c[k]==expected[k] for k in COST if k not in ('candidate_id','plan_id')),'invented cost/resource/bound: '+name+' C'+str(kind-1))
            costs.append(c)
        key=tuple(costs[0][k] for k in ('pu','node','result'))+(name,)
        require(key not in seen,'duplicate comparison');seen.add(key)
        require(all(tuple(c[k] for k in ('pu','node','result'))==key[:3] for c in costs),'cross-node candidates')
        require(lines[index]=='AI_NORM_SELECT 1 HEADER '+' '.join(SELECT),'invalid selection header');index+=1
        selected=row(lines[index],'AI_NORM_SELECT 1 SELECT',SELECT);index+=1
        nominal,choice,fallback,reason=choose(costs,model['margin'])
        require(tuple(selected[k] for k in ('nominal','selected','fallback','reason'))==(nominal,choice,fallback,reason) and
                selected['analysis_only']==1 and selected['source_line']==source_line and tuple(selected[k] for k in ('pu','node','result'))==key[:3],
                'unsupported selection or promotion')
        next_id=0
        for c in costs:
            if choice and c['status']==0:next_id+=1;expected_id=next_id
            else:expected_id=0
            require(c['candidate_id']==expected_id==c['plan_id'],'invalid common-record identity')
        require(selected['common_plan']==(costs[choice-1]['plan_id'] if choice else 0),'common selection mismatch')
        if choice:
            require(index<len(lines),'missing selected guard')
            words=lines[index].split();index+=1
            require(words[:4]==['AI_NORM_CHOICE_GUARD','1','EXPORT','83'] and len(words)==88 and words[-1]==str(source_line),'invalid selected guard')
            g=dict(zip(GUARD_FIELDS,integers(words[4:-1])));guard_key=key[:3]+(choice,)
            require(guard_key in guards and g==guards[guard_key],'selected guard not freshly source/plan bound')
        results.append(dict(experiment=name,model=model,hardware=hw,values=values,provenance=provenance,costs=costs,selection=selected))
    keys={(pu,node,result) for pu,node,result,_ in baseline}
    if source_keys is not None:
        require(source_keys and set(source_keys)<=keys,'invalid explicit source scope');keys=set(source_keys)
    expected={(pu,node,result,scenario) for pu,node,result in keys for scenario in scenarios}
    require(seen==expected,'missing comparison census')
    return results


def write_reports(trace,plan,source,guards,output,scenarios=SCENARIOS,source_keys=None):
    """Expose all components, nominal/gated choices and open executable gates."""
    rows=audit(trace.read_text(),plan.read_text(),source.read_text(),list(guards.glob('*.guard')),scenarios=scenarios,source_keys=source_keys)
    output.mkdir(parents=True,exist_ok=False)
    cost='# What it summarizes: complete C0/C1/C2 declared-model costs, resources, bounds, nominal ranking and uncertainty-gated analytical selection.\n'
    cost+='COST_COLUMNS '+' '.join(COST)+'\n'
    cost+='COST_STATUS 0=complete 1=plan_refused 2=unknown 3=invalid_profile 4=budget 5=overflow 6=resource_rejected\n'
    cost+='PROFILE_FIELDS '+' '.join(FIELDS)+' each=(provenance,low,nominal,high); PREFIX=(version,arch,revision,model,regs0,regs1,regs2,max_jobs,margin); source_line=last\n'
    cost+='RESOURCE_COLUMNS '+' '.join(RESOURCE)+' origin=estimated_envelope\n'
    cost+='RESOURCE '+' '.join(str(rows[0]['hardware'][k]) for k in RESOURCE)+'\n'
    cost+='FORMULA total=compute+memory_unhidden+communication_unhidden+sync+launch+guard; bounds=optimistic_dependency,fully_serial_slow; units=assumed_reference_cycles\n'
    cost+='REASONS 0=no_baseline 1=incomplete_cost 2=C0_minimum_or_tie 3=uncertain_benefit_retain_C0 4=upper_plus_margin_below_C0_lower\n'
    bubbles='# What it summarizes: scheduled critical-path exposure, issued external/private-carrier traffic, barriers and finite resident CTA planning; not GPU waiting-cycle measurements.\n'
    roofline='# What it summarizes: candidate-specific all-miss scalar-operation intensity and shared hardware-profile service lower bounds, not measured HBM throughput or Tensor Core FLOPs.\n'
    for record in rows:
        name=record['experiment'];s=record['selection']
        model=record['model'];profile_words=[str(model[k]) for k in MODEL if k!='source_line']
        for f,bounds in enumerate(record['values']):profile_words += [str(record['provenance'][f])]+list(map(str,bounds))
        cost+='PROFILE '+name+' '+' '.join(profile_words)+' '+str(model['source_line'])+'\n'
        for c in record['costs']:
            cost+='COST '+name+' '+' '.join(str(c[k]) for k in COST)+'\n'
            if c['status']==0:
                bubbles+=f"EXPOSURE {name} {c['node']} C{c['kind']-1} memory={c['memory_unhidden']} compute={c['compute']} control_sync={c['sync']} carrier_sectors={c['carrier_sectors']} resident={c['resident']} {c['source_line']}\n"
                values=record['values'];hw=record['hardware'];bytes_=c['global_sectors']+c['carrier_sectors']
                memory_bound=ceildiv(bytes_,values[0][1]);scalar_bound=ceildiv(c['scalar_lanes'],hw['sms']*values[2][1])
                kernel=c['total']-sum(values[f][1] for f in (12,13,14,15))
                require(max(memory_bound,scalar_bound)<=kernel,'schedule below engine service lower bound')
                roofline+=f"ROOFLINE {name} {c['node']} C{c['kind']-1} intensity={c['scalar_lanes']}/{bytes_} memory_bound={memory_bound} scalar_bound={scalar_bound} modeled_kernel={kernel} div_sqrt=separately_scheduled {c['source_line']}\n"
        cost+='SELECT '+name+' '+' '.join(str(s[k]) for k in SELECT)+'\n'
    (output/'physical-plan.log').write_text(cost);(output/'bubble.log').write_text(bubbles);(output/'roofline.log').write_text(roofline)
    (output/'optimization.log').write_text('# What it summarizes: full successful LN3 physical-plan, analytical exposure and Roofline report collection.\n'+cost+bubbles+roofline)
    summary=(f"comparisons={len(rows)} complete_costs={sum(c['status']==0 for r in rows for c in r['costs'])} analytical_selections={sum(bool(r['selection']['selected']) for r in rows)}\n")
    (output/'optimization-summary.log').write_text('# What it summarizes: complete/unknown/refused comparisons and analytical selections, preserved source v1 and remaining lowering/provider/GPU gates.\n'+summary+
        'selection_scope=analysis_only timing=explicit_profile_assumptions resources=estimated_envelope builtin_timing=incomplete fallback=C0_conditional_not_executable\n'+
        'TRANSFORM none executable_WHIRL=not_lowered production_provider=missing GPU=not_run reduction_dependency=false\n')
    return rows
