"""LN2-d exact guard/native-plan joins and host-mock protocol audit.

Design: doc/AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md.
Declarations, exports and observed host events remain separate evidence classes.
"""
from pathlib import Path
import struct

from normalization_plan_trace_report import audit as plan_audit, integers, require, COLS, RANGES
from normalization_ref_trace_report import audit as source_audit

FIELDS = ("abi_version size source_version source_flags pu node x gamma beta result "
          "x_type gamma_type beta_type result_type rows width epsilon_bits source_line "
          "architecture route toolkit ptx environment fp_options kind layout contract context ownership threads ctas tail "
          "stride shared reserved_shared shared_limit shared_quantum participants barriers banks bank_word warp bank_conflict obligations").split()
FIELDS += [f"range{i}_{key}" for i in range(4) for key in ("role", "capacity", "offset", "alignment", "lease", "bytes")]
FIELDS += [f"before{i}" for i in range(15)]
require(len(FIELDS)==83,"guard schema drift")
HOST = {"ready":0,"launch":1,"drain":11,"publish":12,"input_release":13,"output_release":14}


def decode(data):
    """Fixture bytes are native little-endian uint64 on retained test hosts."""
    require(len(data)==83*8,"invalid guard byte extent")
    return dict(zip(FIELDS,struct.unpack("<83Q",data)))


def audit_exports(text, plan_text, source_text, images):
    """Join all exported fields to independently audited source/plan evidence.

    Native TY identities are opaque compiler facts: only their nonzero extent
    and expected X/Y, gamma/beta equality are asserted by this auditor.
    """
    plans=plan_audit(plan_text,source_text)
    baseline={}
    for p in plans:
        if p["kind"] in ("1","2","3"):
            baseline.setdefault(tuple(p[k] for k in ("pu","node","result","kind")),p)
    facts={tuple(f[k] for k in ("pu","node","result")):f for f in source_audit(source_text)}
    lines=text.splitlines(); exports=[];refused=0;offset=0;seen=set()
    while offset<len(lines):
        chunk=lines[offset:offset+22];require(len(chunk)==22,"incomplete export block")
        require(chunk[0]=="AI_NORM_PLAN 1 HEADER "+" ".join(COLS),"invalid plan header")
        fields=chunk[1].split();require(fields[:3]==["AI_NORM_PLAN","1","BIND"] and len(fields)==3+len(COLS),"invalid plan row")
        row=dict(zip(COLS,fields[3:]));key=tuple(row[k] for k in ("pu","node","result","kind"))
        require(key in baseline and key not in seen,"foreign/duplicate guard plan");seen.add(key)
        p=baseline[key];require(all(row[k]==p[k] for k in COLS),"guard/source plan drift")
        for i in range(4):
            require(chunk[2+i].split()==["AI_NORM_RANGE","1","RANGE"]+[str(p["ranges"][i][k]) for k in RANGES],"export range drift")
        for i in range(15):
            require(chunk[6+i].split()==["AI_NORM_EVENT","1","EDGE",str(i),str(p["edges"][i]),p["source_line"]],"export event drift")
        words=chunk[21].split()
        if p["status"]!="0":
            require(words==["AI_NORM_GUARD","1","REFUSE",p["status"],p["source_line"]],"refused plan exported")
            refused+=1;offset+=22;continue
        require(words[:4]==["AI_NORM_GUARD","1","EXPORT","83"] and len(words)==88,"invalid guard schema")
        values=integers(words[4:-1]);g=dict(zip(FIELDS,values))
        require(words[-1]==p["source_line"] and g["source_line"]==int(words[-1]),"guard line mismatch")
        fact=facts[key[:3]]
        fixed=dict(abi_version=1,size=664,source_version=2,source_flags=29687,route=1,toolkit=12080,ptx=807,environment=1,fp_options=0)
        require(all(g[k]==v for k,v in fixed.items()),"invalid exact source/target contract")
        for k in ("pu","node","x","gamma","beta","result","rows","width","source_line"):
            require(g[k]==int(fact[k]),"guard/source identity drift: "+k)
        require(g["epsilon_bits"]==int(fact["epsilon_bits"],16),"epsilon drift")
        require(all(0<g[k]<=2**32-1 for k in ("x_type","gamma_type","beta_type","result_type")) and
                g["x_type"]==g["result_type"] and g["gamma_type"]==g["beta_type"],"invalid opaque TY identity")
        require(g["architecture"]=={"sm_90":1,"sm_90a":2,"sm_100":3,"sm_100a":4}[plan_text.splitlines()[0].split()[3]],"architecture drift")
        mapping=dict(contract="runtime",reserved_shared="reserved",shared_limit="limit",shared_quantum="quantum",bank_conflict="conflict")
        for k in ("kind","layout","contract","context","ownership","threads","ctas","tail","stride","shared","reserved_shared",
                  "shared_limit","shared_quantum","participants","barriers","banks","bank_word","warp","bank_conflict","obligations"):
            require(g[k]==int(p[mapping.get(k,k)]),"guard/plan field drift: "+k)
        for i in range(4):
            for k in ("role","capacity","offset","alignment","lease","bytes"):
                require(g[f"range{i}_{k}"]==p["ranges"][i]["allocation" if k=="role" else k],"guard/range field drift")
        require([g[f"before{i}"] for i in range(15)]==p["edges"],"guard/DAG drift")
        exports.append(g);offset+=22
    decoded=[decode(p.read_bytes()) for p in images]
    require(sorted(tuple(g[k] for k in FIELDS) for g in exports)==sorted(tuple(g[k] for k in FIELDS) for g in decoded),"raw export/binary census drift")
    require(len(exports)+refused==len(baseline),"missing source alternatives")
    return exports,refused


def audit_runtime(text,g):
    """Reconstruct successful/failed host transitions and lease lifecycle.
    Internal completion bits must NEVER appear, including at successful drain.
    """
    lines=text.splitlines();require(lines and lines[0]=="AI_NORM_RUN 1 HEADER seq ticket action status host_done leases host_test source_line","runtime header missing")
    tickets={};checks=0;seq=0;begun={};observed=set();refusals=0;summary=None;failed=set();attempted=set()
    bindings={};limits={};ranges={};exact_exports=0;host_variants=0;admissions=set()
    for line in lines[1:]:
        require(summary is None,"records after terminal summary")
        words=line.split()
        if words[:3]==["AI_NORM_TEST","1","CHECK"]:
            require(len(words)==6 and words[4]=="1" and int(words[5])==g["source_line"],"host test failure")
            checks+=1;continue
        if words[:3]==["AI_NORM_TEST","1","SUMMARY"]:
            require(summary is None and len(words)==6 and words[4]=="host_mock_only" and int(words[5])==g["source_line"],"invalid test summary")
            summary=int(words[3]);continue
        if words[:3]==["AI_NORM_RUN","1","BIND"]:
            require(len(words)==89 and words[4]=="83","invalid runtime guard columns")
            id=integers([words[3]])[0];values=integers(words[5:-1]);bound=dict(zip(FIELDS,values))
            require(id and id not in bindings and id not in tickets and words[-1]==str(g["source_line"]),"duplicate/foreign runtime binding")
            require(all(bound[k]==g[k] for k in FIELDS if not k.startswith("range")),"runtime/source/plan identity drift")
            # Extra host-test admissions intentionally vary ONLY backing bindings.
            require(all(bound[f"range{i}_{k}"]==g[f"range{i}_{k}"] for i in range(4) for k in ("alignment","lease","bytes")),"host variant changed typed range contract")
            bindings[id]=bound;ranges[id]=[]
            if bound==g:exact_exports+=1
            else:host_variants+=1
            continue
        if words[:3]==["AI_NORM_RUN","1","LIMIT"]:
            require(len(words)==9,"invalid runtime limit columns")
            id,ctas,threads,shared,flags,lineno=integers(words[3:])
            require(id in bindings and id not in limits and flags==1 and lineno==g["source_line"] and
                    ctas>=g["ctas"] and threads>=g["threads"] and shared>=g["reserved_shared"],"unenforced actual limits")
            limits[id]=(ctas,threads,shared);continue
        if words[:3]==["AI_NORM_RUN","1","RANGE"]:
            require(len(words)==14,"invalid runtime root columns")
            id,i,role,root,backing,capacity,offset,alignment,bytes_,lease,lineno=integers(words[3:])
            require(id in bindings and i==len(ranges[id]) and i<4 and root and backing and lineno==g["source_line"],"invalid live root binding")
            bound=bindings[id]
            require([role,capacity,offset,alignment,lease,bytes_]==[bound[f"range{i}_{k}"] for k in ("role","capacity","offset","alignment","lease","bytes")],"root/guard range drift")
            require(offset<=capacity and bytes_<=capacity-offset,"root footprint overflow")
            for j,other in enumerate(ranges[id]):
                other_role,other_root,other_backing,other_capacity,other_offset,other_bytes=other
                require((role==other_role)==(root==other_root)==(backing==other_backing),"canonical root alias mismatch")
                if root==other_root:
                    require(capacity==other_capacity and (i!=3 or max(offset,other_offset)>=min(offset+bytes_,other_offset+other_bytes)),"root overlap/metadata contradiction")
            ranges[id].append((role,root,backing,capacity,offset,bytes_));continue
        require(words[:3]==["AI_NORM_RUN","1","EVENT"] and len(words)==11,"invalid host event schema")
        n,id=integers(words[3:5]);action=words[5];status,done,leases,host,lineno=integers(words[6:])
        require(n==seq+1 and status<=7 and host==1 and not done&~sum(1<<e for e in HOST.values()) and leases<=15,"invented host completion")
        seq=n;observed.add(action)
        if id and action not in ("register",): require(lineno in (0,g["source_line"]),"foreign host line")
        prior=tickets.get(id,(0,0))
        if action=="admit" and status==0:
            require(id and id not in tickets and id in bindings and id in limits and len(ranges[id])==4 and done==0 and leases==15,"invalid admission grant")
            tickets[id]=(0,15)
            admissions.add(id)
        elif action.startswith("begin_"):
            name=action[6:];require(name in ("ready","launch","drain","publish") and status==0 and (done,leases)==prior and id not in begun,"invalid phase begin")
            event=HOST[name];need=0 if event==0 else 1 if event==1 else (2 if done&2 else 0) if event==11 else 1<<11
            require(done&need==need,"work began before predecessor completion")
            require(event!=11 or id in attempted,"drain without launch attempt")
            require(id not in failed or event==11,"failed launch permitted work other than drain")
            if event==1:attempted.add(id)
            begun[id]=name
        elif action in HOST:
            if status==0:
                event=HOST[action]
                require(id in tickets,"event without admission")
                if not prior[0]&(1<<event):
                    if event<13: require(begun.pop(id,None)==action,"completion without matching work")
                    if event>=13:
                        require(prior[0]&(1<<(event-1)),"early lease release")
                    expected_leases=prior[1] & (8 if event==13 else 7) if event>=13 else prior[1]
                    require(done==prior[0]|(1<<event) and leases==expected_leases,"invalid completion/lease change")
                else: require((done,leases)==prior,"retry mutated event")
                require(id not in failed or action=="drain","publication after failed launch")
                tickets[id]=(done,leases)
            elif status==5:
                require(begun.pop(id,None)==action and (done,leases)==prior,"failed work released storage")
                if action=="launch":failed.add(id)
            else:
                require((done,leases)==prior or id not in tickets,"refused transition changed state")
        elif action=="abort" and status==0:
            require(id in tickets and done==prior[0] and leases==0 and id not in begun,"invalid abort")
            require(id not in attempted or done&(1<<11),"abort before launched work drained")
            tickets[id]=(done,0)
        elif action=="close" and status==0:
            require(id in tickets and prior[1]==0 and (done,leases)==prior,"closing live ticket")
            del tickets[id]
        elif action not in ("register","retire","admit","abort","close"):
            require(False,"unknown runtime action")
        if status: refusals+=1
    require(summary==checks and checks>=200 and not tickets and not begun and exact_exports>=1 and
            set(bindings)==admissions,"incomplete host enforcement evidence")
    require(all(k in observed for k in HOST) and refusals>=100,"missing runtime evidence classes")
    return dict(checks=checks,events=seq,refusals=refusals,exact_export_admissions=exact_exports,
                host_mutated_binding_admissions=host_variants,host_mock=True,internal_GPU_events_observed=False)
