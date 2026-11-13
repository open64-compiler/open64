/* Copyright (C) 2026 Open64 Project */
/* Selected H5 methods inside VHO_TENSOR_DEVICE. One warp owns one head; all
 * warps reuse a bounded 32-token x 16-column panel. Publication and release are
 * explicit native CTA barriers. No scheduling decision or CUDA template lives
 * here. Design: doc/AI-COMPILER-H5-MLA-HIERARCHY-v1.0.md. */
    ST *_h5_panel, *_h5_scores, *_h5_copy, *_h5_batch, *_h5_head, *_h5_sums[16];
    /* Every CTA's batch is independent of a padded head's flattened row. */
    WN *H5_cache_index(UINT64 dimension, WN *token, WN *column) {
        return WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,WN_Add(MTYPE_U8,
            WN_Mpy(MTYPE_U8,load(_h5_batch),integer(_attention.length)),token),integer(dimension)),column);
    }
    /* Each warp has disjoint score/weight storage for its entire chunk. */
    WN *H5_score_index(WN *token) {
        return WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_warp),integer(_att_capacity)),token);
    }
    /* Padded heads/tokens cannot read caller operands or publish outputs. */
    WN *H5_valid() {
        return WN_LAND(WN_LT(MTYPE_U8,load(_h5_head),integer(_attention.heads)),
            WN_LT(MTYPE_U8,WN_Add(MTYPE_U8,load(_begin),WN_Add(MTYPE_U8,load(_tile),load(_lane))),load(_end)));
    }
    /* Cooperative bounded panel copy: every address is guarded before load;
     * inactive token/column slots receive zero. All participants then publish. */
    void H5_copy_panel(WN *body, UINT32 cache, UINT64 dimension) {
        WN *copy=WN_CreateBlock(), *valid=WN_CreateBlock(), *zero=WN_CreateBlock();
        WN *token=WN_Add(MTYPE_U8,load(_begin),WN_Add(MTYPE_U8,load(_tile),WN_Div(MTYPE_U8,load(_h5_copy),integer(16))));
        WN *column=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_att_group),integer(16)),WN_Binary(OPR_REM,MTYPE_U8,load(_h5_copy),integer(16)));
        SharedStore(valid,_h5_panel,512,load(_h5_copy),Evaluate(cache,H5_cache_index(dimension,WN_COPY_Tree(token),WN_COPY_Tree(column))));
        SharedStore(zero,_h5_panel,512,load(_h5_copy),floating(0));
        WN_INSERT_BlockLast(copy,WN_CreateIf(WN_LAND(WN_LT(MTYPE_U8,token,load(_end)),WN_LT(MTYPE_U8,column,integer(dimension))),valid,zero));
        WN_INSERT_BlockLast(body,AttLoop(_h5_copy,load(_tid),integer(512),_kernel.threads,copy));
        Barrier(body);
    }
    /* Fold panel columns in original ascending order into each lane's private
     * score dot. Readers finish before any warp can refill the shared panel. */
    void H5_score_panels(WN *body, UINT32 query, UINT32 cache, UINT64 dimension, ST *sum) {
        WN *panels=WN_CreateBlock();H5_copy_panel(panels,cache,dimension);
        WN *consume=WN_CreateBlock();
        for(UINT32 j=0;j<16;++j) {
            WN *column=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_att_group),integer(16)),integer(j));
            WN *valid=WN_CreateBlock();
            WN_INSERT_BlockLast(valid,store(sum,WN_Add(MTYPE_F4,load(sum),WN_Mpy(MTYPE_F4,
                Evaluate(query,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_flat),integer(dimension)),WN_COPY_Tree(column))),
                SharedRead(_h5_panel,512,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_lane),integer(16)),integer(j)))))));
            WN_INSERT_BlockLast(consume,WN_CreateIf(WN_LT(MTYPE_U8,column,integer(dimension)),valid,WN_CreateBlock()));
        }
        WN_INSERT_BlockLast(panels,WN_CreateIf(load(_valid),consume,WN_CreateBlock()));
        Barrier(panels);
        WN_INSERT_BlockLast(body,AttLoop(_att_group,integer(0),integer((dimension+15)/16),1,panels));
    }
    /* Keep complete per-head chunk statistics; warp trees never mix heads.
     * Scores/weights are local shared storage, never global materializations. */
    void H5_scores(WN *body) {
        WN_INSERT_BlockLast(body,store(_sum,floating(0xff800000)));
        WN *slab=WN_CreateBlock();
        WN_INSERT_BlockLast(slab,store(_valid,WN_Intconst(MTYPE_I4,0)));
        WN *mask_read=WN_CreateBlock();
        WN *mask=Evaluate(_attention.mask,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_flat),integer(_attention.length)),
            WN_Add(MTYPE_U8,load(_begin),WN_Add(MTYPE_U8,load(_tile),load(_lane)))));
        WN_INSERT_BlockLast(mask_read,store(_valid,WN_NE(MTYPE_U4,mask,WN_Intconst(MTYPE_U4,0))));
        WN_INSERT_BlockLast(slab,WN_CreateIf(H5_valid(),mask_read,WN_CreateBlock()));
        WN_INSERT_BlockLast(slab,store(_att_dot,floating(0)));WN_INSERT_BlockLast(slab,store(_att_rope,floating(0)));
        H5_score_panels(slab,_attention.q,_attention.cache,_attention.rank,_att_dot);
        H5_score_panels(slab,_attention.qr,_attention.rope_cache,_attention.rope,_att_rope);
        WN_INSERT_BlockLast(slab,store(_att_score,floating(0xff800000)));
        WN *allowed=WN_CreateBlock();
        WN_INSERT_BlockLast(allowed,store(_att_score,WN_Mpy(MTYPE_F4,WN_Add(MTYPE_F4,load(_att_dot),load(_att_rope)),floating(_attention.scale_bits))));
        WN_INSERT_BlockLast(slab,WN_CreateIf(load(_valid),allowed,WN_CreateBlock()));
        SharedStore(slab,_h5_scores,_kernel.head_group*_att_capacity,H5_score_index(WN_Add(MTYPE_U8,load(_tile),load(_lane))),load(_att_score));
        AttCombine(slab,load(_att_score),true);
        WN_INSERT_BlockLast(body,AttLoop(_tile,integer(0),integer(_att_capacity),32,slab));
        WarpTree(body,true);WarpBroadcast(body,_att_raw_m);
        WN_INSERT_BlockLast(body,store(_att_m,AttSafeMax(load(_att_raw_m))));
        WN_INSERT_BlockLast(body,store(_sum,floating(0)));
        WN *weights=WN_CreateBlock();
        WN_INSERT_BlockLast(weights,store(_att_weight,AttMath(false,WN_Sub(MTYPE_F4,
            SharedRead(_h5_scores,_kernel.head_group*_att_capacity,H5_score_index(load(_att_token))),load(_att_m)))));
        SharedStore(weights,_h5_scores,_kernel.head_group*_att_capacity,H5_score_index(load(_att_token)),load(_att_weight));
        AttCombine(weights,load(_att_weight),false);
        WN_INSERT_BlockLast(body,AttLoop(_att_token,load(_lane),integer(_att_capacity),32,weights));
        WarpTree(body,false);WarpBroadcast(body,_att_d);
        WN *normalize=WN_CreateBlock();
        SharedStore(normalize,_h5_scores,_kernel.head_group*_att_capacity,H5_score_index(load(_att_token)),
            WN_Div(MTYPE_F4,SharedRead(_h5_scores,_kernel.head_group*_att_capacity,H5_score_index(load(_att_token))),AttSafeDenom()));
        WN_INSERT_BlockLast(body,AttLoop(_att_token,load(_lane),integer(_att_capacity),32,normalize));
        WarpBarrier(body); /* Every warp reads only its own published weights. */
    }
    /* Second latent sweep shares each panel across all heads. Sixteen scalar
     * accumulators survive token slabs; normalized weights preserve the G1
     * multiply/add grouping. Release precedes refill, including padded heads. */
    void H5_latent(WN *body) {
        WN *panels=WN_CreateBlock();
        for(UINT32 j=0;j<16;++j) WN_INSERT_BlockLast(panels,store(_h5_sums[j],floating(0)));
        WN *slab=WN_CreateBlock();H5_copy_panel(slab,_attention.cache,_attention.rank);
        WN *consume=WN_CreateBlock();
        WN_INSERT_BlockLast(consume,store(_att_weight,SharedRead(_h5_scores,_kernel.head_group*_att_capacity,
            H5_score_index(WN_Add(MTYPE_U8,load(_tile),load(_lane))))));
        for(UINT32 j=0;j<16;++j) {
            WN *valid=WN_CreateBlock(), *column=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_att_group),integer(16)),integer(j));
            WN_INSERT_BlockLast(valid,store(_h5_sums[j],WN_Add(MTYPE_F4,load(_h5_sums[j]),WN_Mpy(MTYPE_F4,load(_att_weight),
                SharedRead(_h5_panel,512,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_lane),integer(16)),integer(j)))))));
            WN_INSERT_BlockLast(consume,WN_CreateIf(WN_LT(MTYPE_U8,column,integer(_attention.rank)),valid,WN_CreateBlock()));
        }
        WN_INSERT_BlockLast(slab,WN_CreateIf(H5_valid(),consume,WN_CreateBlock()));Barrier(slab);
        WN_INSERT_BlockLast(panels,AttLoop(_tile,integer(0),integer(_att_capacity),32,slab));
        for(UINT32 j=0;j<16;++j) {
            WN *column=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_att_group),integer(16)),integer(j));
            WN *reduce=WN_CreateBlock();WN_INSERT_BlockLast(reduce,store(_sum,load(_h5_sums[j])));WarpTree(reduce,false);
            WN *write=WN_CreateBlock();AttOutput(write,WN_Add(MTYPE_U8,WN_COPY_Tree(column),integer(_kernel.stage==1?2:0)),load(_sum));
            WN_INSERT_BlockLast(reduce,WN_CreateIf(WN_LAND(WN_LT(MTYPE_U8,load(_h5_head),integer(_attention.heads)),
                WN_EQ(MTYPE_U8,load(_lane),integer(0))),write,WN_CreateBlock()));
            WN_INSERT_BlockLast(panels,WN_CreateIf(WN_LT(MTYPE_U8,column,integer(_attention.rank)),reduce,WN_CreateBlock()));
        }
        WN_INSERT_BlockLast(body,AttLoop(_att_group,integer(0),integer((_attention.rank+15)/16),1,panels));
    }
    /* Materialize only the sealed H5 recipe in the driver's fresh device PU.
     * Padded warps remain in every CTA barrier; no cross-batch/head data alias.
     * Partial tuple/output indexing and later stable merge use the old ABI. */
    WN *H5_body() {
        need(_kernel.mapping==3 && (_kernel.head_group==2 || _kernel.head_group==4) && _kernel.split_width<=1024,"H5 fixed recipe");
        Is_True(_kernel.head_group==_program.head_group,("H5 selected worker/program group mismatch"));
        WN *body=WN_CreateBlock();_att_capacity=((_kernel.split_width+31)/32)*32;
        _h5_panel=Shared("h5_cache_panel",512);_h5_scores=Shared("h5_head_scores_weights",_kernel.head_group*_att_capacity);
        _h5_copy=local("h5_copy",MTYPE_U8,_source);_h5_batch=local("h5_batch",MTYPE_U8,_source);_h5_head=local("h5_head",MTYPE_U8,_source);
        _att_token=local("h5_token",MTYPE_U8,_source);_att_group=local("h5_panel",MTYPE_U8,_source);
        _att_outbase=local("h5_output",MTYPE_U8,_source);_att_dot=local("h5_dot",MTYPE_F4,_source);_att_rope=local("h5_rope",MTYPE_F4,_source);
        _att_score=local("h5_score",MTYPE_F4,_source);_att_raw_m=local("h5_raw_max",MTYPE_F4,_source);_att_m=local("h5_safe_max",MTYPE_F4,_source);
        _att_d=local("h5_denominator",MTYPE_F4,_source);_att_weight=local("h5_weight",MTYPE_F4,_source);
        for(UINT32 j=0;j<16;++j) _h5_sums[j]=local((std::string("h5_sum_")+decimal(j)).c_str(),MTYPE_F4,_source);
        UINT64 groups=(_attention.heads+_kernel.head_group-1)/_kernel.head_group,ctas=_attention.batch*groups;
        WN_INSERT_BlockLast(body,store(_tid,Coord(Coordinate("threadIdx"))));
        WN_INSERT_BlockLast(body,store(_lane,WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(32))));
        WN_INSERT_BlockLast(body,store(_warp,WN_Div(MTYPE_U8,load(_tid),integer(32))));
        WN_INSERT_BlockLast(body,store(_physical,WN_Binary(OPR_REM,MTYPE_U8,Coord(Coordinate("blockIdx")),integer(ctas))));
        WN_INSERT_BlockLast(body,store(_h5_batch,WN_Div(MTYPE_U8,load(_physical),integer(groups))));
        WN_INSERT_BlockLast(body,store(_h5_head,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,
            WN_Binary(OPR_REM,MTYPE_U8,load(_physical),integer(groups)),integer(_kernel.head_group)),load(_warp))));
        WN_INSERT_BlockLast(body,store(_flat,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_h5_batch),integer(_attention.heads)),load(_h5_head))));
        WN_INSERT_BlockLast(body,store(_part,WN_Div(MTYPE_U8,Coord(Coordinate("blockIdx")),integer(ctas))));
        WN_INSERT_BlockLast(body,store(_begin,WN_Mpy(MTYPE_U8,load(_part),integer(_kernel.split_width))));
        WN *limit=WN_Add(MTYPE_U8,load(_begin),integer(_kernel.split_width));
        WN_INSERT_BlockLast(body,store(_end,WN_Select(MTYPE_U8,WN_LT(MTYPE_U8,WN_COPY_Tree(limit),integer(_attention.length)),limit,integer(_attention.length))));
        WN_INSERT_BlockLast(body,store(_att_outbase,_kernel.stage==1?AttPartialIndex(load(_part),0):WN_Mpy(MTYPE_U8,load(_flat),integer(_attention.rank+1))));
        H5_scores(body);
        WN *meta=WN_CreateBlock();
        if(_kernel.stage==1) {AttOutput(meta,integer(0),load(_att_raw_m));AttOutput(meta,integer(1),load(_att_d));}
        else AttOutput(meta,integer(_attention.rank),AttLSE());
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_LAND(WN_LT(MTYPE_U8,load(_h5_head),integer(_attention.heads)),
            WN_EQ(MTYPE_U8,load(_lane),integer(0))),meta,WN_CreateBlock()));
        H5_latent(body);return body;
    }
