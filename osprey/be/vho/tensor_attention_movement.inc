/* Copyright (C) 2026 Open64 Project */
/* Included by the detached estimator to reuse its counts, checked arithmetic,
 * schedule admission, occupancy and portfolio. No native services.
 * Synchronous copy -> consume -> release; no speculative overlap discount. */
const char *VHO_mla_movement_catalog() {return "mla-movement-space-v1-36";}
const char *VHO_mla_movement_capabilities() {return "mla-materializers-v1:G1=verified-v8;G2,G4,G8=analysis-only";}
BOOL VHO_mla_movement_profile_valid(const VHO_MLA_MOVEMENT_PROFILE &p,FILE *f) {
    bool spelling=true;
    for(size_t j=0;j<p.id.size();++j) spelling=spelling && (std::isalnum((unsigned char)p.id[j]) || p.id[j]=='_' || p.id[j]=='-' || p.id[j]=='.');
    bool ok=spelling && !p.id.empty() && p.id.size()<=96 && (p.architecture=="sm_90" || p.architecture=="sm_90a") &&
        (p.provenance=="synthetic" || p.provenance=="estimated" || p.provenance=="measured-input") &&
        (p.objective=="worst" || p.objective=="weighted") && p.hbm_rate && p.hbm_rate<=1000000000 &&
        p.l2_rate && p.l2_rate<=1000000000 && (p.sector_bytes==32 || p.sector_bytes==64 || p.sector_bytes==128) &&
        p.request_amplification_ppm>=1000000 && p.request_amplification_ppm<=8000000 &&
        p.shared_conflict_ppm>=1000000 && p.shared_conflict_ppm<=8000000 &&
        !p.scenarios.empty() && p.scenarios.size()<=4;
    std::set<std::string> ids;
    for(size_t i=0;i<p.scenarios.size();++i) {
        const VHO_MLA_CACHE_SCENARIO &s=p.scenarios[i];
        for(size_t j=0;j<s.id.size();++j) ok=ok && (std::isalnum((unsigned char)s.id[j]) || s.id[j]=='_' || s.id[j]=='-' || s.id[j]=='.');
        ok=ok && !s.id.empty() && s.id.size()<=96 && ids.insert(s.id).second &&
            s.weight && s.weight<=1000000 && s.cache_hit_ppm<=1000000 && s.other_hit_ppm<=1000000;
    }
    if(!ok && f) fprintf(f,"MLA movement: incomplete/unsupported hierarchy profile\n");
    return ok;
}
/* Length-framed full values, never ID-only or an architecture-implied hit rate. */
std::string VHO_mla_movement_profile_key(const VHO_MLA_MOVEMENT_PROFILE &p) {
    std::ostringstream s;s<<"mla-hierarchy-v1;"<<p.id.size()<<':'<<p.id<<';'<<p.architecture<<';'<<p.provenance<<';'
        <<p.objective<<';'<<p.hbm_rate<<';'<<p.l2_rate<<';'<<p.sector_bytes<<';'
        <<p.request_amplification_ppm<<';'<<p.shared_conflict_ppm<<';'<<p.scenarios.size()<<';';
    for(size_t i=0;i<p.scenarios.size();++i) {
        const VHO_MLA_CACHE_SCENARIO &c=p.scenarios[i];
        s<<c.id.size()<<':'<<c.id<<';'<<c.weight<<';'<<c.cache_hit_ppm<<';'<<c.other_hit_ppm<<';';
    }
    return s.str();
}
namespace {
/* Every descriptor is a hypothesis. There is no mutable capability switch. */
void Movement_catalog(VHO_MLA_SEARCH_RESULT &r,const VHO_TENSOR_ATTENTION &a) {
    VHO_search_catalog(r);
    for(size_t i=0;i<r.candidates.size();++i) r.candidates[i].descriptor.column=a.rank;
    const UINT32 widths[]={32,64,128,256},groups[]={2,4,8};
    for(unsigned g=0;g<3;++g) for(unsigned w=0;w<4;++w) for(unsigned retain=0;retain<2;++retain) {
        VHO_MLA_SEARCH_CANDIDATE c;c.width=widths[w];c.descriptor.group=groups[g];
        c.descriptor.retained=retain;c.descriptor.column=retain?a.rank:std::min(UINT64(16),a.rank);
        r.candidates.push_back(c);
    }
}
std::string Movement_descriptor(const VHO_MLA_SEARCH_CANDIDATE &c) {
    std::ostringstream s;s<<"mla-movement-descriptor-v1/G="<<c.descriptor.group<<"/W="<<c.width<<"/policy="<<c.policy
        <<"/column="<<c.descriptor.column<<"/sweeps="<<(c.descriptor.retained?1:2)
        <<"/layout="<<(c.descriptor.group==1?"existing":"row-major-smem")
        <<"/ownership="<<(c.descriptor.group==1?"existing":"warp-per-head")
        <<"/pipeline=synchronous/tuple=normalized-f32-v1";
    return s.str();
}
/* Scenario transaction counts are an explicit amplification + sector envelope,
 * not an affine coalescing proof. Aggregate rounding is per CTA and operand.
 * L1 is conservatively ignored; all modeled transactions reach L2. Stores are
 * charged write-through. Hits are fixed inputs shared by every candidate. */
void Movement_traffic(VHO_MLA_STAGE_ESTIMATE &s,const VHO_MLA_MOVEMENT_PROFILE &p,
    const VHO_MLA_CACHE_SCENARIO &scenario,const char *operand,const char *direction,
    UINT64 per_cta,UINT64 shared,bool cache) {
    VHO_MLA_TRAFFIC_ESTIMATE t;t.operand=operand;t.direction=direction;t.scenario=scenario.id;
    t.requested=mul(per_cta,s.ctas);t.shared=mul(shared,s.ctas);
    UINT64 envelope=ceilq(mul(per_cta,p.request_amplification_ppm),1000000);
    UINT64 sectors=mul(ceilq(envelope,p.sector_bytes),p.sector_bytes);
    t.transactions=t.l2=mul(sectors,s.ctas);
    UINT64 hit=cache?scenario.cache_hit_ppm:scenario.other_hit_ppm;
    t.hbm=std::string(direction)!="read"?t.transactions:mul(ceilq(mul(sectors,1000000-hit),1000000),s.ctas);
    s.movement.traffic.push_back(t);
}
/* Reuse the readiness max-plus composition for the explicitly serialized
 * delivery/consume/release graph. There is no hidden async/TMA edge. */
UINT64 Movement_sequence(UINT64 delivery,UINT64 consume,UINT64 release) {
    DSL_GEMM_READINESS_MATRIX a,b,c;
    memset(&a,0xff,sizeof(a));memset(&b,0xff,sizeof(b));
    a.edge[0][0]=delivery;b.edge[0][0]=consume;
    if(!DSL_readiness_compose(a,b,&c)) throw std::runtime_error("movement_overflow");
    b.edge[0][0]=release;
    if(!DSL_readiness_compose(c,b,&a)) throw std::runtime_error("movement_overflow");
    return a.edge[0][0];
}
/* Sum only valid element requests. Reserve all static/padded resources. Class
 * grids (full/tail token x full/tail head) are serialized as a conservative
 * envelope, not a prediction of the hardware CTA issue order. */
void Movement_cost_stage(VHO_MLA_STAGE_ESTIMATE &s,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &p,const VHO_MLA_MOVEMENT_PROFILE &h,UINT64 registers) {
    DSL_GEMM_OCCUPANCY_RESULT o;
    if(!DSL_gemm_occupancy(g.resources,32,g.sms,s.threads,mul(4,registers),s.shared,s.ctas,&o) || !o.resident)
        throw std::runtime_error("movement_resource_fit");
    s.resident=o.resident;s.movement.registers=registers;
    s.movement.rounded_registers=o.registers;s.movement.rounded_shared=o.shared;s.movement.unused_slots=o.unused;
    UINT64 capacity=mul(g.sms,s.resident),l2=0,hbm=0;
    for(size_t i=0;i<s.movement.traffic.size();++i) {
        l2=add(l2,s.movement.traffic[i].l2);hbm=add(hbm,s.movement.traffic[i].hbm);
    }
    const UINT64 *v=p.values;
    for(unsigned tail=0;tail<2;++tail) {
        UINT64 active=tail?s.ctas%capacity:capacity,rounds=tail?(active!=0):s.ctas/capacity;
        if(!rounds) continue;
        UINT64 q=ceilq(active,g.sms),warps=mul(q,s.threads/32);
        UINT64 compute=std::max(mul(q,ceilq(s.scalar_ops,v[VHO_MLA_SCALAR_RATE])),mul(s.dependency,v[VHO_MLA_DEPENDENCY]));
        /* Divide the class's homogeneous per-CTA traffic once by device-wide
         * bandwidth. Never charge full-device bandwidth independently per SM. */
        UINT64 memory=add(ceilq(mul(active,l2/s.ctas),h.l2_rate),ceilq(mul(active,hbm/s.ctas),h.hbm_rate));
        memory=add(memory,ceilq(v[VHO_MLA_LOAD_LATENCY],std::min(warps,v[VHO_MLA_HIDING_WARPS])));
        UINT64 special=mul(q,add(add(mul(s.shuffles,v[VHO_MLA_SHUFFLE]),mul(s.exps,v[VHO_MLA_EXP])),mul(s.divs,v[VHO_MLA_DIV])));
        UINT64 local=mul(q,ceilq(ceilq(mul(s.shared_bytes,h.shared_conflict_ppm),1000000),v[VHO_MLA_SHARED_RATE]));
        UINT64 sync=mul(q,add(mul(s.barriers,v[VHO_MLA_CTA_BARRIER]),mul(s.fences,v[VHO_MLA_WARP_FENCE])));
        s.compute=add(s.compute,mul(rounds,compute));s.memory=add(s.memory,mul(rounds,memory));
        s.special=add(s.special,mul(rounds,special));s.local=add(s.local,mul(rounds,local));s.sync=add(s.sync,mul(rounds,sync));
        s.rounds=add(s.rounds,rounds);
        s.total=add(s.total,mul(rounds,Movement_sequence(memory,add(add(compute,special),local),sync)));
    }
}
/* A capability-qualified G1 seed supplies all unaffected projection/output
 * stages. Grouping changes only the proposed attention CTA and its cache
 * staging; normalized per-head split tuples and the existing merge stay.
 * H1c also checks grouped reservations against owned imported CTA limits. */
void Movement_evaluate(VHO_MLA_SEARCH_CANDIDATE &c,const VHO_TENSOR_PROGRAM &source,
    const VHO_TENSOR_GPU_PROFILE &g,const VHO_MLA_COST_PROFILE &model,const VHO_MLA_MOVEMENT_CONTEXT &ctx) {
    c.evaluated=true;c.movement.descriptor_key=Movement_descriptor(c);
    VHO_MLA_SEARCH_CANDIDATE seed;seed.policy=c.policy;seed.width=c.width;
    Evaluate(seed,source,g,model,true);
    if(!seed.eligible) {c.reason=seed.reason;return;}
    VHO_TENSOR_PROGRAM p=source;p.split_width=c.width;p.chunk_policy=c.policy;p.register_latent=true;
    p.auto_chunks=p.search_selected=false;
    VHO_TENSOR_SCHEDULE plan;
    if(!VHO_tensor_schedule(p,&plan,NULL)) throw std::runtime_error("movement_seed_schedule");
    const VHO_TENSOR_ATTENTION &a=plan.attention;
    if(c.width && mul(ceilq(a.length,c.width),a.rank)>a.length) {
        c.reason="movement_private_tuple_capacity";return;
    }
    UINT64 payload=mul(mul(mul(4,a.batch),a.length),add(a.rank,a.rope));
    if(ctx.latent_payload!=mul(mul(mul(4,a.batch),a.length),a.rank) || ctx.rotary_payload!=mul(mul(mul(4,a.batch),a.length),a.rope) || ctx.proof_key.empty())
        throw std::runtime_error("movement_reuse_proof_mismatch");
    c.scratch=seed.scratch;c.movement.cache_payload=payload;c.movement.partial_allocation=c.scratch;
    c.movement.head_slots=mul(ceilq(a.heads,c.descriptor.group),c.descriptor.group);
    const VHO_MLA_MOVEMENT_PROFILE &hp=ctx.profile;UINT64 weight=0,weighted=0,worst=0;
    for(size_t scenario_id=0;scenario_id<hp.scenarios.size();++scenario_id) {
        const VHO_MLA_CACHE_SCENARIO &scenario=hp.scenarios[scenario_id];
        UINT64 total=0,cache_requests=0,partial_requests=0;
        for(size_t j=0;j<plan.kernels.size();++j) {
            const VHO_TENSOR_KERNEL &k=plan.kernels[j];
            if(k.mapping==3) {
                UINT64 width=k.split_width,group=c.descriptor.group,headgroups=ceilq(a.heads,group);
                UINT64 allocated_shared=VHO_tensor_shared_bytes(p,k),registers=g.registers_per_thread;
                if(group>1) {
                    allocated_shared=mul(4,add(add(mul(width,c.descriptor.column),mul(width,a.rope)),mul(group,add(a.rank,2))));
                    registers=add(registers,add(ceilq(a.rank,32),add(mul(2,ceilq(width,32)),4)));
                }
                if(allocated_shared>49152) throw std::runtime_error("movement_static_smem_48KiB");
                UINT64 chunks=ceilq(a.length,width);bool launched=false;
                for(unsigned ht=0;ht<2;++ht) for(unsigned tt=0;tt<2;++tt) {
                    UINT64 h=ht?a.heads%group:group,nh=ht?(h!=0):a.heads/group;
                    UINT64 w=tt?a.length%width:width,nw=tt?(w!=0):a.length/width;
                    if(!nh || !nw) continue;
                    VHO_MLA_STAGE_ESTIMATE s;s.operation=k.id;s.ctas=mul(mul(a.batch,nh),nw);
                    s.threads=group>1?mul(32,group):k.threads;s.shared=allocated_shared;
                    if(!g.device.Fits_CTA(s.threads,registers,s.shared)) throw std::runtime_error("movement_imported_CTA_resources");
                    s.movement.valid_tokens=w;s.movement.valid_heads=h;
                    Attention_counts(s,a,k);
                    s.scalar_ops=mul(h,add(mul(w,add(mul(2,add(a.rank,a.rope)),2)),mul(mul(2,w),a.rank)));
                    s.exps=add(mul(h,ceilq(w,32)),group==1 && k.warp_owned_partials?4:0);s.divs=s.exps;
                    UINT64 latent=mul(mul(4,w),mul(a.rank,group==1?h*2:(c.descriptor.retained?1:2)));
                    UINT64 rotary=mul(mul(4,w),mul(a.rope,group==1?h:1));
                    UINT64 shared_write=group>1?add(latent,rotary):0;
                    UINT64 shared_read=group>1?mul(mul(mul(4,w),h),add(mul(2,a.rank),a.rope)):0;
                    if(group>1) {
                        UINT64 tiles=ceilq(a.rank,c.descriptor.column),passes=c.descriptor.retained?1:2;
                        s.barriers=mul(2,mul(tiles,passes));s.fences=0;
                        s.shuffles=mul(group,add(12,mul(20,ceilq(a.rank,4))));
                        s.dependency=mul(add(a.rank,a.rope),ceilq(w,32));
                        s.scalar_ops=add(s.scalar_ops,shared_write/4);
                        s.shared_bytes=add(shared_write,shared_read);
                    }
                    Movement_traffic(s,hp,scenario,"latent-cache","read",latent,0,true);
                    Movement_traffic(s,hp,scenario,"rotary-cache","read",rotary,0,true);
                    Movement_traffic(s,hp,scenario,"query","read",mul(mul(mul(4,w),h),add(a.rank,a.rope)),0,false);
                    Movement_traffic(s,hp,scenario,"mask","read",mul(w,h),0,false);
                    Movement_traffic(s,hp,scenario,chunks>1?"partial-tuple":"attention-output","write",
                        mul(mul(4,h),add(a.rank,chunks>1?2:1)),0,false);
                    /* Staging transfers have no additional global request. */
                    if(group>1) Movement_traffic(s,hp,scenario,"cache-staging","shared",0,add(shared_write,shared_read),false);
                    cache_requests=add(cache_requests,mul(add(latent,rotary),s.ctas));
                    if(chunks>1) partial_requests=add(partial_requests,mul(mul(mul(4,h),add(a.rank,2)),s.ctas));
                    Movement_cost_stage(s,g,model,hp,registers);
                    if(!launched) {s.launch=model.values[VHO_MLA_LAUNCH];s.total=add(s.total,s.launch);launched=true;}
                    total=add(total,s.total);
                    c.stages.push_back(s);
                }
                (void)headgroups;
            } else {
                VHO_MLA_STAGE_ESTIMATE s;
                s.operation=k.id;s.ctas=VHO_tensor_grid(p,k);s.threads=k.threads;s.shared=VHO_tensor_shared_bytes(p,k);
                /* The legacy non-cache schedule envelope remains visible. */
                if(k.mapping==4) {
                    Attention_counts(s,a,k);
                    UINT64 reads=mul(mul(4,k.chunks),add(a.rank,4));
                    Movement_traffic(s,hp,scenario,"partial-tuple","read",reads,0,false);
                    Movement_traffic(s,hp,scenario,"attention-output","write",mul(4,add(a.rank,1)),0,false);
                    partial_requests=add(partial_requests,mul(reads,s.ctas));
                } else {
                    /* Preserve the existing instruction/shared envelope.
                     * Dense contraction/copy operand counts include CTA tail
                     * padding; these are requests, not an affine sector proof. */
                    const VHO_MLA_STAGE_ESTIMATE &old=seed.stages[j];
                    s.scalar_ops=old.scalar_ops;s.dependency=old.dependency;s.shared_bytes=old.shared_bytes;
                    s.barriers=old.barriers;s.shuffles=old.shuffles;
                    const VHO_TENSOR_OPERATION &op=p.operations[k.root];
                    UINT64 count=ceilq(k.elements,s.ctas),width=op.name=="common.matmul"?p.values[op.inputs[0]].shape.back():1;
                    for(size_t operand=0;operand<op.inputs.size();++operand)
                        Movement_traffic(s,hp,scenario,operand?"projection-right":"projection-left","read",mul(mul(4,count),width),0,false);
                    Movement_traffic(s,hp,scenario,"projection-or-copy-output","write",mul(4,count),0,false);
                }
                Movement_cost_stage(s,g,model,hp,g.registers_per_thread);s.launch=model.values[VHO_MLA_LAUNCH];
                s.total=add(s.total,s.launch);total=add(total,s.total);
                c.stages.push_back(s);
            }
        }
        if(!scenario_id) {c.movement.cache_requests=cache_requests;c.movement.partial_requests=partial_requests;}
        c.movement.scenario_cycles.push_back(total);worst=std::max(worst,total);
        weighted=add(weighted,mul(total,scenario.weight));weight=add(weight,scenario.weight);
    }
    c.total=hp.objective=="worst"?worst:ceilq(weighted,weight);c.movement.modeled=true;
    c.eligible=c.descriptor.group==1;
    c.reason=c.eligible?"movement_eligible_G1":"analysis_only_unimplemented_grouped_materializer";
}
}


std::string VHO_mla_movement_descriptor_key(const VHO_MLA_SEARCH_CANDIDATE &c) {return Movement_descriptor(c);}
