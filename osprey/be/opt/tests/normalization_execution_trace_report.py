"""Audit LN4 consumer movement, shared leases and retryable ownership traces.
Design: doc/AI-COMPILER-LAYERNORM-EXECUTION-CONSUMER-v0.1.md.
No numerical/GPU inference from mock sentinel writes.
"""
from normalization_plan_trace_report import require
from normalization_guard_trace_report import FIELDS


def audit(text, expected, scenarios=22):
    """Reconstruct actual host-event order and ownership from raw rows, not CHECK names."""
    g=dict(zip(FIELDS,expected['guard_words']));owners={};tickets={};checks=0;summary=None;scenario_rows=[];copies=[]
    for line in text.splitlines():
        w=line.split()
        if w[:2] in (['AI_NORM_EXEC','1'],['AI_NORM_EXEC_TEST','1']):require(summary is None,'execution records after summary')
        if w[:3]==['AI_NORM_RUN','1','BIND']:
            require(len(w)==89 and w[4]=='83' and list(map(int,w[5:-1]))==expected['guard_words'] and
                    int(w[-1])==g['source_line'],'foreign execution guard')
            ticket=int(w[3]);require(ticket and ticket not in tickets,'duplicate ticket');tickets[ticket]=dict(done=set(),busy=None,launches=0,closed=False,publish_started=False,aborted=False)
        elif w[:3]==['AI_NORM_RUN','1','EVENT']:
            require(len(w)==11,'invalid runtime event extent')
            ticket=int(w[4]);action=w[5];status=int(w[6])
            if ticket not in tickets:continue
            t=tickets[ticket]
            if action.startswith('begin_'):
                phase=action[6:];require(status==0 and t['busy'] is None,'overlapping runtime work')
                predecessor={'ready':None,'launch':'ready','drain':'launch','publish':'drain'}[phase]
                # Ambiguous failed launch is still an attempted launch, needing drain.
                require(predecessor is None or predecessor in t['done'] or phase=='drain' and t['launches']==1,'work before predecessor')
                t['busy']=phase
                if phase=='launch':t['launches']+=1;require(t['launches']==1,'kernel reissued')
                if phase=='publish':t['publish_started']=True
            elif action in ('ready','launch','drain','publish') and status in (0,5):
                # Idempotent successful calls may occur without a fresh begin.
                require(t['busy'] in (None,action),'wrong callback completion');t['busy']=None
                if status==0:t['done'].add(action)
            elif action=='abort' and status==0:
                require(not t['busy'] and (not t['launches'] or 'drain' in t['done']) and
                        (not t['publish_started'] or 'publish' in t['done']),'abort before work joins')
                t['aborted']=True
            elif action=='close' and status==0:
                require(not t['busy'] and int(w[8])==0 and ('publish' in t['done'] or t['aborted']),'runtime close before release/abort')
                t['closed']=True
        elif w[:3]==['AI_NORM_EXEC','1','EVENT']:
            require(len(w)==10 and int(w[-1])==g['source_line'],'invalid execution columns')
            owner,sequence=int(w[3]),int(w[4]);action=w[5];status,a,b=map(int,w[6:9])
            require(action in {'open','begin','preflight','import','allocate','allocation_quarantined','register','owned_root',
                'borrowed_root','admit','binding','input_join','h2d','d2h','consumer_join','quiesce','advance','close','retire',
                'release','idle','resume','cancel','abort','destroy','selected'},'unknown execution action')
            p=owners.setdefault(owner,dict(seq=0,active=False,ticket=0,owned=set(),registered=set(),quiesce=False,destroyed=False,ready=False))
            require(owner>0 and sequence==p['seq']+1 and not p['destroyed'],'execution sequence/lifetime drift');p['seq']=sequence
            if action=='open':p['ready']=status==0
            if action=='selected':
                matched=p['ready'] and a==expected['plan_seal'] and b==expected['tree_seal']
                require(status==(0 if matched else 1),'selected identity gate disagrees with compiled entry')
            if action=='allocate':
                require(a in range(4) and b==g[f'range{a}_capacity'],'allocation extent changed')
                p['active']=True;p['owned'].add(a)
            if action=='owned_root':
                require(a in p['owned'],'registered root lacks allocation ownership')
                p['active']=True;p['registered'].add(a)
            if action=='borrowed_root':p['active']=True
            if action=='admit' and status==0:
                require(a in tickets and not tickets[a]['closed'],'admission lacks exact native guard');p['ticket']=a
            if action in ('h2d','d2h'):
                require(p['ticket'] in tickets and not p['quiesce'],'copy before admission/quiescence')
                t=tickets[p['ticket']]
                require(a in (0,1,2) if action=='h2d' else a==3,'copy role changed')
                require(b==g[f'range{a}_bytes'],'copy extent changed')
                require(t['busy']==('ready' if action=='h2d' else 'publish'),'copy outside leased phase')
                if action=='d2h':require('drain' in t['done'],'copyback before device drain')
                copies.append((action,status,a,b))
            if action=='advance' and status:p['quiesce']=True
            if action=='allocation_quarantined':
                require(b in (0,1),'invalid quarantine ownership');p['quiesce']=True
                if not b:p['owned'].discard(a)
            if action=='release' and status:p['quiesce']=True
            if action=='quiesce':p['quiesce']=status!=0
            if action=='close' and status==0:
                require(a==p['ticket'] and tickets[a]['closed'],'wrapper close lacks runtime close');p['ticket']=0
            if action=='retire' and status==0:
                require(not p['ticket'] and a in p['registered'],'retirement before close or foreign slot');p['registered'].remove(a)
            if action=='release' and status==0:
                require(not p['ticket'] and a not in p['registered'] and not p['quiesce'],'release before completion/retirement/quiescence')
                p['owned'].discard(a)
            if action=='idle':
                require(not p['ticket'] and not p['registered'] and not p['owned'],'idle hides live ownership');p['active']=False;p['quiesce']=False
            if action=='destroy' and status==0:
                require(not p['active'] and not p['ticket'],'destroy while invocation active');p['destroyed']=True
        elif w[:3]==['AI_NORM_EXEC_TEST','1','CHECK']:
            require(len(w)==6 and w[4]=='1' and int(w[-1])==g['source_line'],'failed execution assertion');checks+=1
        elif w[:3]==['AI_NORM_EXEC_TEST','1','SCENARIO']:
            require(len(w)==5 and int(w[-1])==g['source_line'],'invalid scenario');scenario_rows.append(int(w[3]))
        elif w[:3]==['AI_NORM_EXEC_TEST','1','SUMMARY']:
            require(len(w)==8 and w[-2]=='host_mock_sentinel_only' and int(w[-1])==g['source_line'],'invalid execution summary')
            summary=list(map(int,w[3:6]))
    require(summary and summary[0]==checks and summary[1]==scenarios and scenario_rows==list(range(1,scenarios+1)),
            'incomplete execution coverage')
    require(owners and all(p['destroyed'] for p in owners.values()) and all(t['closed'] for t in tickets.values()),'unfinished consumer/ticket')
    require(any(c[0]=='h2d' and c[1]==0 for c in copies) and any(c[0]=='d2h' and c[1]==0 for c in copies),'missing movement coverage')
    return dict(checks=checks,scenarios=scenarios,API_checks=summary[2],consumers=len(owners),tickets=len(tickets),
                h2d_attempts=sum(c[0]=='h2d' for c in copies),d2h_attempts=sum(c[0]=='d2h' for c in copies),GPU=False)


def report(text, expected, output):
    """Produce execution and provider logs only after reconstructing raw evidence."""
    result=audit(text,expected)
    content='# What it summarizes: host/imported root ownership, leased input/copyback order, retry/quiescence and retirement/release in the real execution consumer; CUDA API/sentinel mocks only.\n'
    content+='EVENT_COLUMNS consumer seq action status a b source_line\n'
    content+=''.join(s+'\n' for s in text.splitlines() if s.startswith('AI_NORM_EXEC '))
    content+='GPU not_run NUMERICAL_KERNEL not_executed V1 preserved REDUCTION_DEPENDENCY false\n'
    output.write_text(content)
    provider=[s for s in text.splitlines() if s.startswith('AI_NORM_CUDA ')]
    require(provider,'missing consumer provider observations')
    for line in provider:
        w=line.split();require(len(w)==10 and w[:3]==['AI_NORM_CUDA','1','EVENT'] and
            int(w[-1])==expected['source_line'],'foreign provider source/columns')
        if w[5]=='open' and w[6]=='0':require([int(w[7]),int(w[8])]==[expected['plan_seal'],expected['tree_seal']],
                                            'foreign consumer compiled entry')
    output.with_name('provider.log').write_text('# What it summarizes: selected-entry module/resource observations and lifetime pins while the host/service consumer dispatches; CUDA API mocks, not GPU execution.\n'+
        'EVENT_COLUMNS provider seq action status a b source_line\n'+'\n'.join(provider)+'\n')
    return result
