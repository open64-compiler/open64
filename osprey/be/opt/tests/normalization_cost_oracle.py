"""Independent LN3 literal recipe and frontier-scan scheduling oracle.

Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md.
No reduction imports or native implementation calls. Integer arithmetic has no
overflow; explicit checked-u64 boundaries reproduce refusal, not wraparound.
"""
from functools import lru_cache

MAX=2**64-1
FIELDS=('global_rate shared_rate compute_rate global_latency carrier_latency shared_latency add_latency mul_latency '
        'div_latency sqrt_latency control barrier guard launch workspace publication sector div_issue sqrt_issue').split()


def ceildiv(n,d):
    """Exact positive-denominator service/reservation division."""
    return n//d+(n%d!=0)


def profile_status(values,provenance,identity,hardware):
    """Unknown zero payloads differ from explicitly known zero durations."""
    version,arch,revision,model=identity
    if version!=1 or arch!=hardware['arch'] or revision!=hardware['revision'] or not model:return 3
    unknown=False
    for f,(bounds,origin) in enumerate(zip(values,provenance)):
        if origin>3 or not 0<=bounds[0]<=bounds[1]<=bounds[2]<=MAX:return 3
        if not origin:
            if bounds[2]:return 3
            unknown=True
        elif (f<3 or f==16) and not bounds[0]:return 3
    if provenance[16]:
        sector=values[16][0]
        if sector<4 or sector>256 or sector&(sector-1) or sector!=values[16][2]:return 3
    return 2 if unknown else 0


def residents(hw,threads,warp,shared,registers):
    """Recompute rounded fixed RF/SMEM/CTA occupancy without native helpers."""
    if not registers or registers>hw['max_regs']:return 0
    warps=ceildiv(threads,warp)
    allocation=ceildiv(registers*warp,hw['reg_quantum'])*hw['reg_quantum']*ceildiv(warps,hw['reg_warps'])*hw['reg_warps']
    count=min(hw['blocks'],hw['threads']//threads,hw['warps']//warps,hw['registers']//allocation)
    allocation=ceildiv(shared+hw['shared_reserved'],hw['shared_quantum'])*hw['shared_quantum']
    if allocation:count=min(count,hw['shared_budget']//allocation)
    return count


@lru_cache(maxsize=96)
def recipe(rows,width,kind,threads,warp,sms,resident,sector,stride,offsets,alignments):
    """Construct independent per-warp programs and explicit joins/row waves.
    Nodes are (engine, resource-class, timing-field, lanes, bytes, predecessors).
    """
    jobs=[(None,'join',0,0,0,())]
    counts=dict(global_logical=0,global_sectors=0,carrier_sectors=0,shared=0,scalar_lanes=0,scalar_insts=0,control=0,barriers=0)
    sm=0

    def add(resource,field=0,lanes=0,bytes_=0,preds=()):
        """Append one DAG node; program order and thread joins stay explicit."""
        engine=0 if resource=='global' else None if resource=='join' else 1+3*sm+{'shared':1,'barrier':2}.get(resource,0)
        jobs.append((engine,resource,field,lanes,bytes_,tuple(preds)));return len(jobs)-1

    def control(pred):
        """Predicate/index/loop service is not silently free."""
        counts['control']+=1;return add('control',10,1,preds=(pred,))

    def memory(pred,space,offset,lanes,operand=0):
        """Project guaranteed-alignment sectors; shared broadcasts use one word."""
        size=lanes*4
        if space=='shared':traffic=size;field=5;counts['shared']+=traffic
        else:
            alignment=sector if space=='carrier' else alignments[operand]
            traffic=max(((base+offset%sector+size-1)//sector-(base+offset%sector)//sector+1)*sector
                        for base in range(0,sector,min(sector,alignment)))
            field=4 if space=='carrier' else 3
            counts['carrier_sectors' if space=='carrier' else 'global_sectors']+=traffic
            if space!='carrier':counts['global_logical']+=size
        return add('shared' if space=='shared' else 'global',field,lanes,traffic,(control(pred),))

    def scalar(pred,field,lanes):
        """Literal store/load pointer packaging, operation, store/unpack sequence."""
        for _ in range(2 if field==9 else 4):pred=memory(pred,'carrier',0,lanes)
        counts['scalar_lanes']+=lanes;counts['scalar_insts']+=1
        pred=add('scalar',field,lanes,preds=(pred,))
        for _ in range(2):pred=memory(pred,'carrier',0,lanes)
        return pred

    def barrier(preds):
        """Every warp participates, including wholly inactive masked groups."""
        counts['barriers']+=1;return add('barrier',11,preds=preds)

    def fold(seed,row,variance):
        """Exact ascending ordered statistics; finalization contains real div/sqrt."""
        shared=kind!=1 if variance else kind==3
        for k in range(width):
            seed=memory(control(seed),'shared' if shared else 'global',k*4 if shared else offsets[0]+(row*width+k)*4,1)
            if variance:
                if kind==1:seed=scalar(seed,6,1)
                seed=scalar(seed,7,1)
            seed=scalar(seed,6,1)
        seed=scalar(seed,8,1)
        if variance:
            for field in (6,9,8):seed=scalar(seed,field,1)
        return seed

    def cooperative(seed,row,mode):
        """Warp programs retain independence until the all-participant barrier."""
        tails=[]
        for w in range(threads//warp):
            first=w*warp;cursor=seed
            if mode and first<width:cursor=memory(cursor,'shared',stride+(4 if mode==2 else 0),1)
            for base in range(0,width,threads):
                cursor=control(cursor);k=base+first
                if k>=width:continue
                active=min(warp,width-k);shared=mode==2 or mode==1 and kind==3
                cursor=memory(cursor,'shared' if shared else 'global',k*4 if shared else offsets[0]+(row*width+k)*4,active)
                if mode==1:cursor=scalar(cursor,6,active)
                if mode==2:
                    cursor=scalar(cursor,7,active)
                    cursor=memory(cursor,'global',offsets[1]+k*4,active,1);cursor=scalar(cursor,7,active)
                    cursor=memory(cursor,'global',offsets[2]+k*4,active,2);cursor=scalar(cursor,6,active)
                    cursor=memory(cursor,'global',offsets[3]+(row*width+k)*4,active,3)
                else:cursor=memory(cursor,'shared',k*4,active)
            tails.append(cursor)
        return barrier(tails)

    slots=min(rows,sms*resident);previous=[0]*slots;outputs=[]
    for row in range(rows):
        sm=(row%slots)%sms;cursor=previous[row%slots]
        if kind==3:cursor=cooperative(cursor,row,0)
        cursor=fold(cursor,row,False)
        if kind!=1:
            cursor=memory(cursor,'shared',stride,1);cursor=barrier((cursor,));cursor=cooperative(cursor,row,1)
        cursor=fold(cursor,row,True)
        if kind==1:
            for k in range(width):
                cursor=memory(control(cursor),'global',offsets[0]+(row*width+k)*4,1)
                cursor=scalar(cursor,6,1);cursor=scalar(cursor,7,1)
                cursor=memory(cursor,'global',offsets[1]+k*4,1,1);cursor=scalar(cursor,7,1)
                cursor=memory(cursor,'global',offsets[2]+k*4,1,2);cursor=scalar(cursor,6,1)
                cursor=memory(cursor,'global',offsets[3]+(row*width+k)*4,1,3)
        else:
            cursor=memory(cursor,'shared',stride+4,1);cursor=barrier((cursor,));cursor=cooperative(cursor,row,2)
        previous[row%slots]=cursor;outputs.append(cursor)
    add('join',preds=outputs)
    return jobs,counts


def duration(job,values,corner,conflict):
    """Service ends before startup completion; scalar issue is already in latency."""
    _,kind,field,lanes,bytes_,_=job
    if kind=='join':return 0,0
    latency=values[field][corner]
    if kind in ('global','shared'):
        service=ceildiv(bytes_,values[0 if kind=='global' else 1][2-corner])*(conflict if kind=='shared' else 1)
        return service,service+latency
    if kind=='scalar':
        service=ceildiv(lanes,values[2][2-corner])
        if field in (8,9):service=max(service,values[17 if field==8 else 18][corner])
        return service,max(service,latency)
    return latency,latency


@lru_cache(maxsize=192)
def evaluate(shape,values,conflict):
    """Frontier scan is independent of native heap/requeue scheduling.
    Attribution propagates full component vectors rather than native backpointers.
    """
    jobs,counts=recipe(*shape)
    setup=[sum(values[f][corner] for f in (12,13,14)) for corner in range(3)]
    low=[setup[0]];upper=setup[2];engine_service={}
    for job in jobs[1:]:
        fast_service,fast_duration=duration(job,values,0,conflict)
        low.append(max(low[p] for p in job[5])+fast_duration)
        if job[0] is not None:engine_service[job[0]]=engine_service.get(job[0],0)+fast_service
        upper+=duration(job,values,2,conflict)[1]
        if low[-1]>MAX or upper>MAX:return dict(counts,jobs=len(jobs),overflow=True)
    lower=max(low[-1],setup[0]+max(engine_service.values(),default=0))+values[15][0];upper+=values[15][2]
    if lower>MAX or upper>MAX:return dict(counts,jobs=len(jobs),overflow=True)
    # Event tuples match tie identities, but carry independent component vectors.
    events=[(0,(0,)*6)]

    def event(parent,delta,term):
        """Propagate one exclusive critical-path category at a time."""
        time,components=events[parent];v=list(components);v[term]+=delta
        events.append((time+delta,tuple(v)));return len(events)-1

    root=event(0,values[12][1],5);root=event(root,values[13][1],4);root=event(root,values[14][1],4)
    finish={0:root};engines={};pending=[len(j[5]) for j in jobs];successors=[[] for _ in jobs]
    for i,j in enumerate(jobs[1:],1):
        for p in j[5]:successors[p].append(i)
    ready_parent=[0]*len(jobs);frontier=set()
    for next_ in successors[0]:
        ready_parent[next_]=root;pending[next_]-=1
        if not pending[next_]:frontier.add(next_)
    while frontier:
        def start(id):
            """Candidate start uses data readiness and byte/issue engine service."""
            engine=jobs[id][0];parent=ready_parent[id];busy=engines.get(engine,0)
            return max(events[parent][0],events[busy][0]) if engine is not None else events[parent][0]
        id=min(frontier,key=lambda n:(start(n),n));frontier.remove(id)
        engine,kind,_,_,_,_=jobs[id];parent=ready_parent[id]
        if engine is not None and events[engines.get(engine,0)][0]>events[parent][0]:parent=engines[engine]
        service,elapsed=duration(jobs[id],values,1,conflict)
        term=1 if kind in ('global','shared') else 0 if kind=='scalar' else 3
        service_end=event(parent,service,term);finish[id]=event(service_end,elapsed-service,term)
        if engine is not None:engines[engine]=service_end
        for next_ in successors[id]:
            if events[finish[id]][0]>events[ready_parent[next_]][0]:ready_parent[next_]=finish[id]
            pending[next_]-=1
            if not pending[next_]:frontier.add(next_)
    last=event(finish[len(jobs)-1],values[15][1],3)
    total,components=events[last]
    if total>MAX:return dict(counts,jobs=len(jobs),overflow=True)
    return dict(counts,jobs=len(jobs),lower=lower,total=total,upper=upper,terms=components,overflow=False)


def choose(costs,margin):
    """Strict improvement/ties and uncertainty gates are separate from ranking."""
    if costs[0]['plan_status'] or costs[0]['status']==6:return (0,0,0,0)
    if any(c['status'] not in (0,6) for c in costs if not c['plan_status']):return (0,0,1,1)
    best=min((i for i,c in enumerate(costs) if not c['status']),key=lambda i:(costs[i]['total'],i))
    if best==0:return (1,1,1,2)
    admissible=[i for i in (1,2) if not costs[i]['status'] and costs[i]['upper']+margin<=MAX and costs[i]['upper']+margin<costs[0]['lower']]
    if not admissible:return (best+1,1,1,3)
    selected=min(admissible,key=lambda i:(costs[i]['total'],i))
    return (best+1,selected+1,1,4)
