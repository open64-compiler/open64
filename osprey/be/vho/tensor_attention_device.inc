/* Copyright (C) 2026 Open64 Project */
/* Native WHIRL member implementation for fused absorbed attention. Included
 * inside VHO_TENSOR_DEVICE to reuse its symbols, ARRAY addressing, typed
 * collectives and return-preg protocol. No CUDA source template is used.
 * Design: doc/AI-COMPILER-FUSED-MLA-v0.1.md. */
    ST *_att_token,*_att_group,*_att_outbase,*_att_dot,*_att_rope,*_att_score;
    ST *_att_m,*_att_d,*_att_weight,*_att_dc,*_att_state;
    ST *_att_sums[4];
    UINT64 _att_capacity;
    /* The owning driver already selected finite static loop extents. */
    WN *AttLoop(ST *iv,WN *begin,WN *end,UINT64 step,WN *body) {
        return WN_CreateDO(WN_CreateIdname(0,iv),store(iv,begin),WN_LT(MTYPE_U8,load(iv),end),
            store(iv,WN_Add(MTYPE_U8,load(iv),integer(step))),body,NULL);
    }
    /* Standard scalar intrinsics preserve the existing expf/logf spelling. */
    WN *AttMath(bool logarithm,WN *x) {
        WN *arg=WN_CreateParm(MTYPE_F4,x,MTYPE_To_TY(MTYPE_F4),WN_PARM_BY_VALUE);
        return WN_Create_Intrinsic(OPR_INTRINSIC_OP,MTYPE_F4,MTYPE_V,logarithm?INTRN_F4LOG:INTRN_F4EXP,1,&arg);
    }
    /* Preserve the source's finite maximum fallback, including all-masked rows. */
    WN *AttSafeMax(WN *x) {return WN_Select(MTYPE_F4,WN_LE(MTYPE_F4,WN_Abs(MTYPE_F4,WN_COPY_Tree(x)),floating(0x7f7fffff)),x,floating(0));}
    /* A zero denominator represents an empty row; it must never divide by zero. */
    WN *AttSafeDenom() {return WN_Select(MTYPE_F4,WN_GT(MTYPE_F4,load(_att_d),floating(0)),load(_att_d),floating(0x3f800000));}
    /* The admitted cache is shared across heads but distinct across batches. */
    WN *AttCacheIndex(WN *token,WN *column,UINT64 dimension) {
        return WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,
            WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,WN_Div(MTYPE_U8,load(_flat),integer(_attention.heads)),integer(_attention.length)),token),integer(dimension)),column);
    }
    /* Chunk-major tuple offsets keep every CTA's writes disjoint. */
    WN *AttPartialIndex(WN *chunk,UINT64 component) {
        return WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,
            WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,chunk,integer(_attention.batch*_attention.heads)),load(_flat)),integer(_attention.rank+2)),integer(component));
    }
    /* Only the actual selected allocation is addressed, never a score buffer. */
    void AttOutput(WN *body,WN *component,WN *value) {
        WN_INSERT_BlockLast(body,WN_Istore(MTYPE_F4,0,ST_type(_formals.back()),
            Address(_kernel.inputs.size(),WN_Add(MTYPE_U8,load(_att_outbase),component)),value));
    }
    /* Maxima use the established NaN-propagating rule; sums retain F32 steps. */
    void AttCombine(WN *body,WN *value,bool maximum) {
        WN_INSERT_BlockLast(body,store(_next,value));
        WN_INSERT_BlockLast(body,store(_sum,maximum?WN_Select(MTYPE_F4,
            WN_LIOR(WN_NE(MTYPE_F4,load(_next),load(_next)),WN_GT(MTYPE_F4,load(_next),load(_sum))),
            load(_next),load(_sum)):WN_Add(MTYPE_F4,load(_sum),load(_next))));
    }
    /* One full warp publishes directly through a warp fence. Larger blocks
     * publish one partial per warp, then thread zero folds those partials.
     * Barriers are unconditional; state[4,5,6] survives the next fold. */
    void AttCTAReduce(WN *body,bool maximum) {
        WarpTree(body,maximum);
        if(_kernel.threads==32) {
            WN *finish=WN_CreateBlock();
            SharedStore(finish,_att_state,8,integer(maximum?4:6),load(_sum));
            if(maximum) SharedStore(finish,_att_state,8,integer(5),AttSafeMax(load(_sum)));
            WN_INSERT_BlockLast(body,WN_CreateIf(WN_EQ(MTYPE_U8,load(_lane),integer(0)),finish,WN_CreateBlock()));
            WarpBarrier(body);return;
        }
        WN *publish=WN_CreateBlock();
        SharedStore(publish,_att_state,8,load(_warp),load(_sum));
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_EQ(MTYPE_U8,load(_lane),integer(0)),publish,WN_CreateBlock()));
        Barrier(body);WN *finish=WN_CreateBlock();
        WN_INSERT_BlockLast(finish,store(_sum,floating(maximum?0xff800000:0)));
        for(UINT32 i=0;i<_kernel.threads/32;++i) AttCombine(finish,SharedRead(_att_state,8,integer(i)),maximum);
        SharedStore(finish,_att_state,8,integer(maximum?4:6),load(_sum));
        if(maximum) SharedStore(finish,_att_state,8,integer(5),AttSafeMax(load(_sum)));
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_EQ(MTYPE_U8,load(_tid),integer(0)),finish,WN_CreateBlock()));
        Barrier(body);
    }
    /* Source-equivalent LSE guard: fully masked rows write negative infinity. */
    WN *AttLSE() {
        return WN_Select(MTYPE_F4,WN_GT(MTYPE_F4,load(_att_d),floating(0)),
            WN_Add(MTYPE_F4,load(_att_m),AttMath(true,AttSafeDenom())),floating(0xff800000));
    }
    /* Compute each allowed score once into CTA-local storage; no global score
     * or probability allocation exists. Dot products retain ascending K order. */
    void AttScores(WN *body) {
        WN_INSERT_BlockLast(body,store(_sum,floating(0xff800000)));WN *tokens=WN_CreateBlock();
        WN_INSERT_BlockLast(tokens,store(_att_score,floating(0xff800000)));
        WN *valid=WN_CreateBlock(),*allowed=WN_CreateBlock();
        WN_INSERT_BlockLast(allowed,store(_att_dot,floating(0)));WN_INSERT_BlockLast(allowed,store(_att_rope,floating(0)));
        WN *dot=WN_CreateBlock();
        WN_INSERT_BlockLast(dot,store(_att_dot,WN_Add(MTYPE_F4,load(_att_dot),WN_Mpy(MTYPE_F4,
            Evaluate(_attention.q,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_flat),integer(_attention.rank)),load(_reduction))),
            Evaluate(_attention.cache,AttCacheIndex(WN_Add(MTYPE_U8,load(_begin),load(_att_token)),load(_reduction),_attention.rank))))));
        WN_INSERT_BlockLast(allowed,AttLoop(_reduction,integer(0),integer(_attention.rank),1,dot));
        WN *rope=WN_CreateBlock();
        WN_INSERT_BlockLast(rope,store(_att_rope,WN_Add(MTYPE_F4,load(_att_rope),WN_Mpy(MTYPE_F4,
            Evaluate(_attention.qr,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_flat),integer(_attention.rope)),load(_reduction))),
            Evaluate(_attention.rope_cache,AttCacheIndex(WN_Add(MTYPE_U8,load(_begin),load(_att_token)),load(_reduction),_attention.rope))))));
        WN_INSERT_BlockLast(allowed,AttLoop(_reduction,integer(0),integer(_attention.rope),1,rope));
        WN_INSERT_BlockLast(allowed,store(_att_score,WN_Mpy(MTYPE_F4,WN_Add(MTYPE_F4,load(_att_dot),load(_att_rope)),floating(_attention.scale_bits))));
        WN *mask=Evaluate(_attention.mask,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_flat),integer(_attention.length)),
            WN_Add(MTYPE_U8,load(_begin),load(_att_token))));
        WN_INSERT_BlockLast(valid,WN_CreateIf(WN_NE(MTYPE_U4,mask,WN_Intconst(MTYPE_U4,0)),allowed,WN_CreateBlock()));
        WN_INSERT_BlockLast(tokens,WN_CreateIf(WN_LT(MTYPE_U8,WN_Add(MTYPE_U8,load(_begin),load(_att_token)),load(_end)),valid,WN_CreateBlock()));
        SharedStore(tokens,_shared_a,_att_capacity,load(_att_token),load(_att_score));AttCombine(tokens,load(_att_score),true);
        WN_INSERT_BlockLast(body,AttLoop(_att_token,load(_tid),integer(_att_capacity),_kernel.threads,tokens));
        AttCTAReduce(body,true);WN_INSERT_BlockLast(body,store(_att_m,SharedRead(_att_state,8,integer(5))));
        WN_INSERT_BlockLast(body,store(_sum,floating(0)));WN *weights=WN_CreateBlock();
        WN_INSERT_BlockLast(weights,store(_att_weight,AttMath(false,WN_Sub(MTYPE_F4,SharedRead(_shared_a,_att_capacity,load(_att_token)),load(_att_m)))));
        SharedStore(weights,_shared_a,_att_capacity,load(_att_token),load(_att_weight));AttCombine(weights,load(_att_weight),false);
        WN_INSERT_BlockLast(body,AttLoop(_att_token,load(_tid),integer(_att_capacity),_kernel.threads,weights));
        AttCTAReduce(body,false);
    }
    /* Merge statistics without reading/recomputing any token score. Empty
     * chunks branch before exp, preventing 0*exp(0-m) from producing NaN. */
    void AttMergeWeights(WN *body) {
        WN_INSERT_BlockLast(body,store(_sum,floating(0xff800000)));WN *chunks=WN_CreateBlock();
        AttCombine(chunks,Evaluate(_attention.partial,AttPartialIndex(load(_att_token),0)),true);
        WN_INSERT_BlockLast(body,AttLoop(_att_token,load(_tid),integer(_kernel.chunks),128,chunks));
        AttCTAReduce(body,true);WN_INSERT_BlockLast(body,store(_att_m,SharedRead(_att_state,8,integer(5))));
        WN_INSERT_BlockLast(body,store(_sum,floating(0)));WN *weights=WN_CreateBlock();
        WN_INSERT_BlockLast(weights,store(_att_dc,Evaluate(_attention.partial,AttPartialIndex(load(_att_token),1))));
        WN *scaled=WN_Mpy(MTYPE_F4,load(_att_dc),AttMath(false,WN_Sub(MTYPE_F4,
            AttSafeMax(Evaluate(_attention.partial,AttPartialIndex(load(_att_token),0))),load(_att_m))));
        WN_INSERT_BlockLast(weights,store(_att_weight,WN_Select(MTYPE_F4,WN_EQ(MTYPE_F4,load(_att_dc),floating(0)),floating(0),scaled)));
        SharedStore(weights,_shared_a,_att_capacity,load(_att_token),load(_att_weight));AttCombine(weights,load(_att_weight),false);
        WN_INSERT_BlockLast(body,AttLoop(_att_token,load(_tid),integer(_kernel.chunks),_kernel.threads,weights));
        AttCTAReduce(body,false);
    }
    /* Normalize once in shared memory, then all latent columns reuse exactly
     * those F32 probabilities (or the merge's normalized chunk weights). */
    void AttNormalize(WN *body) {
        WN_INSERT_BlockLast(body,store(_att_d,SharedRead(_att_state,8,integer(6))));
        WN *normal=WN_CreateBlock();SharedStore(normal,_shared_a,_att_capacity,load(_att_token),
            WN_Div(MTYPE_F4,SharedRead(_shared_a,_att_capacity,load(_att_token)),AttSafeDenom()));
        WN_INSERT_BlockLast(body,AttLoop(_att_token,load(_tid),integer(_kernel.mapping==4?_kernel.chunks:_att_capacity),_kernel.threads,normal));
        if(_kernel.threads==32) WarpBarrier(body);else Barrier(body);
        WN *meta=WN_CreateBlock();
        if(_kernel.stage==1) {
            AttOutput(meta,integer(0),SharedRead(_att_state,8,integer(4)));AttOutput(meta,integer(1),load(_att_d));
        } else AttOutput(meta,integer(_attention.rank),AttLSE());
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_EQ(MTYPE_U8,load(_tid),integer(0)),meta,WN_CreateBlock()));
    }
    /* Local chunks stage four cache columns for each 32-token tile. A complete
     * warp owns each latent column; all warps retain uniform collective paths. */
    void AttLatent(WN *body) {
        WN *groups=WN_CreateBlock();WN_INSERT_BlockLast(groups,store(_column,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_att_group),integer(4)),load(_warp))));
        WN_INSERT_BlockLast(groups,store(_sum,floating(0)));WN *tile=WN_CreateBlock();
        if(_kernel.mapping==3) {
            WN *valid=WN_CreateBlock(),*zero=WN_CreateBlock();
            WN *col=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_att_group),integer(4)),WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(4)));
            WN *token=WN_Add(MTYPE_U8,load(_begin),WN_Add(MTYPE_U8,load(_tile),WN_Div(MTYPE_U8,load(_tid),integer(4))));
            SharedStore(valid,_shared_b,128,SharedBIndex(WN_Div(MTYPE_U8,load(_tid),integer(4)),WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(4))),
                Evaluate(_attention.cache,AttCacheIndex(WN_COPY_Tree(token),WN_COPY_Tree(col),_attention.rank)));
            SharedStore(zero,_shared_b,128,SharedBIndex(WN_Div(MTYPE_U8,load(_tid),integer(4)),WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(4))),floating(0));
            WN_INSERT_BlockLast(tile,WN_CreateIf(WN_LAND(WN_LT(MTYPE_U8,col,integer(_attention.rank)),WN_LT(MTYPE_U8,token,load(_end))),valid,zero));
            Barrier(tile);WN *compute=WN_CreateBlock();
            AttCombine(compute,WN_Mpy(MTYPE_F4,SharedRead(_shared_a,_att_capacity,WN_Add(MTYPE_U8,load(_tile),load(_lane))),
                SharedRead(_shared_b,128,SharedBIndex(load(_lane),load(_warp)))),false);
            WN_INSERT_BlockLast(tile,WN_CreateIf(WN_LAND(WN_LT(MTYPE_U8,load(_column),integer(_attention.rank)),
                WN_LT(MTYPE_U8,WN_Add(MTYPE_U8,load(_begin),WN_Add(MTYPE_U8,load(_tile),load(_lane))),load(_end))),compute,WN_CreateBlock()));
            Barrier(tile);
            WN_INSERT_BlockLast(groups,AttLoop(_tile,integer(0),WN_Sub(MTYPE_U8,load(_end),load(_begin)),32,tile));
        } else {
            WN *valid=WN_CreateBlock();WN *index=WN_Add(MTYPE_U8,AttPartialIndex(load(_att_token),2),load(_column));
            AttCombine(valid,WN_Mpy(MTYPE_F4,SharedRead(_shared_a,_att_capacity,load(_att_token)),Evaluate(_attention.partial,index)),false);
            WN_INSERT_BlockLast(tile,WN_CreateIf(WN_LT(MTYPE_U8,load(_column),integer(_attention.rank)),valid,WN_CreateBlock()));
            WN_INSERT_BlockLast(groups,AttLoop(_att_token,load(_lane),integer(_kernel.chunks),32,tile));
        }
        WarpTree(groups,false);WN *write=WN_CreateBlock();
        AttOutput(write,WN_Add(MTYPE_U8,load(_column),integer(_kernel.stage==1?2:0)),load(_sum));
        WN_INSERT_BlockLast(groups,WN_CreateIf(WN_LAND(WN_LT(MTYPE_U8,load(_column),integer(_attention.rank)),WN_EQ(MTYPE_U8,load(_lane),integer(0))),write,WN_CreateBlock()));
        WN_INSERT_BlockLast(body,AttLoop(_att_group,integer(0),integer((_attention.rank+3)/4),1,groups));
    }
    /* Shared weights are immutable after AttNormalize's publication barrier.
     * Each lane owns four private sums; no other lane reads them until the
     * existing full-warp shuffle trees. No shared cache tile or tile barrier. */
    void AttRegisterLatent(WN *body) {
        WN *groups=WN_CreateBlock();
        WN_INSERT_BlockLast(groups,store(_column,WN_Add(MTYPE_U8,
            WN_Mpy(MTYPE_U8,load(_att_group),integer(_kernel.threads/8)),WN_Mpy(MTYPE_U8,load(_warp),integer(4)))));
        for(UINT32 j=0;j<4;++j) WN_INSERT_BlockLast(groups,store(_att_sums[j],floating(0)));
        WN *tokens=WN_CreateBlock();
        WN_INSERT_BlockLast(tokens,store(_att_weight,SharedRead(_shared_a,_att_capacity,load(_att_token))));
        for(UINT32 j=0;j<4;++j) {
            WN *col=WN_Add(MTYPE_U8,load(_column),integer(j)),*valid=WN_CreateBlock();
            WN *value=_kernel.mapping==3?
                Evaluate(_attention.cache,AttCacheIndex(WN_Add(MTYPE_U8,load(_begin),load(_att_token)),WN_COPY_Tree(col),_attention.rank)):
                Evaluate(_attention.partial,WN_Add(MTYPE_U8,AttPartialIndex(load(_att_token),2),WN_COPY_Tree(col)));
            WN_INSERT_BlockLast(valid,store(_att_sums[j],WN_Add(MTYPE_F4,load(_att_sums[j]),WN_Mpy(MTYPE_F4,load(_att_weight),value))));
            WN_INSERT_BlockLast(tokens,WN_CreateIf(WN_LT(MTYPE_U8,col,integer(_attention.rank)),valid,WN_CreateBlock()));
        }
        WN *count=_kernel.mapping==3?WN_Sub(MTYPE_U8,load(_end),load(_begin)):integer(_kernel.chunks);
        WN_INSERT_BlockLast(groups,AttLoop(_att_token,load(_lane),count,32,tokens));
        for(UINT32 j=0;j<4;++j) {
            WN_INSERT_BlockLast(groups,store(_sum,load(_att_sums[j])));WarpTree(groups,false);
            WN *write=WN_CreateBlock(),*col=WN_Add(MTYPE_U8,load(_column),integer(j));
            AttOutput(write,WN_Add(MTYPE_U8,WN_COPY_Tree(col),integer(_kernel.stage==1?2:0)),load(_sum));
            WN_INSERT_BlockLast(groups,WN_CreateIf(WN_LAND(WN_LT(MTYPE_U8,col,integer(_attention.rank)),
                WN_EQ(MTYPE_U8,load(_lane),integer(0))),write,WN_CreateBlock()));
        }
        WN_INSERT_BlockLast(body,AttLoop(_att_group,integer(0),integer((_attention.rank+_kernel.threads/8-1)/(_kernel.threads/8)),1,groups));
    }
    /* One CTA per head/chunk (or head for merge/direct); all intermediates of
     * score, softmax and weighted latent stay in this CTA until final stores. */
    WN *AttentionBody() {
        WN *body=WN_CreateBlock();UINT64 rows=_attention.batch*_attention.heads;
        UINT64 quantum=_kernel.mapping==3?_kernel.threads:128;
        _att_capacity=(((_kernel.mapping==3?_kernel.split_width:_kernel.chunks)+quantum-1)/quantum)*quantum;
        _att_token=local("attention_token",MTYPE_U8,_source);_att_group=local("attention_column_group",MTYPE_U8,_source);
        _att_outbase=local("attention_output_base",MTYPE_U8,_source);_att_dot=local("attention_latent_score",MTYPE_F4,_source);
        _att_rope=local("attention_rope_score",MTYPE_F4,_source);_att_score=local("attention_masked_score",MTYPE_F4,_source);
        _att_m=local("attention_maximum",MTYPE_F4,_source);_att_d=local("attention_denominator",MTYPE_F4,_source);
        _att_weight=local("attention_weight",MTYPE_F4,_source);_att_dc=local("attention_chunk_denominator",MTYPE_F4,_source);
        _shared_a=Shared("attention_shared_scores_weights",_att_capacity);_att_state=Shared("attention_shared_statistics",8);
        if(_program.register_latent) for(UINT32 j=0;j<4;++j) _att_sums[j]=local((std::string("attention_register_sum_")+decimal(j)).c_str(),MTYPE_F4,_source);
        if(_kernel.mapping==3 && !_program.register_latent) _shared_b=Shared("attention_shared_cache_tile",128);
        WN_INSERT_BlockLast(body,store(_tid,Coord(Coordinate("threadIdx"))));
        WN_INSERT_BlockLast(body,store(_lane,WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(32))));
        WN_INSERT_BlockLast(body,store(_warp,WN_Div(MTYPE_U8,load(_tid),integer(32))));
        WN_INSERT_BlockLast(body,store(_flat,WN_Binary(OPR_REM,MTYPE_U8,Coord(Coordinate("blockIdx")),integer(rows))));
        WN_INSERT_BlockLast(body,store(_part,_kernel.mapping==3?WN_Div(MTYPE_U8,Coord(Coordinate("blockIdx")),integer(rows)):integer(0)));
        WN_INSERT_BlockLast(body,store(_begin,WN_Mpy(MTYPE_U8,load(_part),integer(_kernel.split_width))));
        WN *limit=WN_Add(MTYPE_U8,load(_begin),integer(_kernel.split_width));
        WN_INSERT_BlockLast(body,store(_end,WN_Select(MTYPE_U8,WN_LT(MTYPE_U8,WN_COPY_Tree(limit),integer(_attention.length)),limit,integer(_attention.length))));
        WN_INSERT_BlockLast(body,store(_att_outbase,_kernel.stage==1?AttPartialIndex(load(_part),0):WN_Mpy(MTYPE_U8,load(_flat),integer(_attention.rank+1))));
        if(_kernel.mapping==3) AttScores(body);else AttMergeWeights(body);
        AttNormalize(body);if(_program.register_latent) AttRegisterLatent(body);else AttLatent(body);return body;
    }
