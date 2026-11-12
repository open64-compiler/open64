/* Copyright (C) 2026 Open64 Project */
/* Selected algorithm methods inside VHO_TENSOR_HIGH. Reuse its affine ARRAY,
 * ordered projection and signed-loop construction. Physical warp/SMEM mapping
 * remains fixed detached plan data for M5, never a second search.
 * Design: doc/AI-COMPILER-SEARCH-M4-ATOMIC-HIGH-v1.0.md. */
    ST *_raw, *_safe, *_den, *_dot, *_rope, *_score, *_weight;
    VHO_TENSOR_ATTENTION _attention;
    UINT64 _chunks, _width;
    UINT32 _partial_n, _partial_m, _partial_d;
    /* Return a fresh affine row coordinate for disjoint batch/head ownership. */
    WN *Row() { return WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, VHO_high_load(_indices[0]),
        VHO_high_integer(_attention.heads)), VHO_high_load(_indices[1])); }
    /* Chunk-major within a row; every chunk writes disjoint tuple slots. */
    WN *Tuple() { return WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, Row(), VHO_high_integer(_chunks)),
        VHO_high_load(_indices[2])); }
    /* Signed token offset is affine, including the guarded final partial tile. */
    WN *Token() { return WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, VHO_high_load(_indices[2]),
        VHO_high_integer(_width)), VHO_high_load(_indices[3])); }
    /* Cache is shared among heads, but never among batch elements. */
    WN *Cache_index(UINT64 dimension) { return WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8,
        WN_Add(MTYPE_I8, WN_Mpy(MTYPE_I8, VHO_high_load(_indices[0]),
            VHO_high_integer(_attention.length)), Token()), VHO_high_integer(dimension)),
        VHO_high_load(_reduction)); }
    /* Scalar source intrinsics remain ordinary High expressions. */
    WN *Math(bool logarithm, WN *value) {
        WN *arg=WN_CreateParm(MTYPE_F4,value,MTYPE_To_TY(MTYPE_F4),WN_PARM_BY_VALUE);
        return WN_Create_Intrinsic(OPR_INTRINSIC_OP,MTYPE_F4,MTYPE_V,
            logarithm?INTRN_F4LOG:INTRN_F4EXP,1,&arg);
    }
    /* Match the existing source's finite-maximum guard, including NaN/Inf. */
    WN *Safe_max(WN *value) { return WN_Select(MTYPE_F4,
        WN_LE(MTYPE_F4,WN_Abs(MTYPE_F4,WN_COPY_Tree(value)),Float(0x7f7fffff)),
        value,Float(0)); }
    /* Empty or unordered denominators use the source's exact one fallback. */
    WN *Safe_den() { return WN_Select(MTYPE_F4,WN_GT(MTYPE_F4,VHO_high_load(_den),
        Float(0)),VHO_high_load(_den),Float(0x3f800000)); }
    /* Store only to the existing disjoint caller-owned scratch/output ABI. */
    void Write(WN *body, UINT32 value, WN *index, WN *expression) {
        WN_INSERT_BlockLast(body,WN_Istore(Scalar_type(_program.values[value]),0,ST_type(_buffers[_program.values[value].storage]),
            Address(value,index),expression));
    }
    /* Preserve established NaN-propagating max, with no child sharing. */
    void Maximum(WN *body, WN *value) {
        WN_INSERT_BlockLast(body,VHO_high_store(_next,value));
        WN_INSERT_BlockLast(body,VHO_high_store(_raw,WN_Select(MTYPE_F4,
            WN_LIOR(WN_NE(MTYPE_F4,VHO_high_load(_next),VHO_high_load(_next)),
                WN_GT(MTYPE_F4,VHO_high_load(_next),VHO_high_load(_raw))),
            VHO_high_load(_next),VHO_high_load(_raw))));
    }
    /* Compute one allowed score with ascending dot-product folds. This scalar
     * High realization recomputes scores across the two passes; M5 may retain
     * them in registers/SMEM according to the already selected physical plan. */
    void Score(WN *body) {
        WN_INSERT_BlockLast(body,VHO_high_store(_score,Float(0xff800000)));
        WN *allowed=WN_CreateBlock();
        WN_INSERT_BlockLast(allowed,VHO_high_store(_dot,Float(0)));
        WN_INSERT_BlockLast(allowed,VHO_high_store(_rope,Float(0)));
        const UINT32 values[]={_attention.q,_attention.qr};
        const UINT32 caches[]={_attention.cache,_attention.rope_cache};
        const UINT64 dimensions[]={_attention.rank,_attention.rope};
        ST *accumulators[]={_dot,_rope};
        for(unsigned k=0;k<2;++k) {
            WN *fold=WN_CreateBlock();
            WN *query_index=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Row(),VHO_high_integer(dimensions[k])),
                VHO_high_load(_reduction));
            WN_INSERT_BlockLast(fold,VHO_high_store(accumulators[k],WN_Add(MTYPE_F4,
                VHO_high_load(accumulators[k]),WN_Mpy(MTYPE_F4,Read(values[k],query_index),
                    Read(caches[k],Cache_index(dimensions[k]))))));
            WN_INSERT_BlockLast(allowed,Loop(_reduction,dimensions[k],fold));
        }
        WN_INSERT_BlockLast(allowed,VHO_high_store(_score,WN_Mpy(MTYPE_F4,
            WN_Add(MTYPE_F4,VHO_high_load(_dot),VHO_high_load(_rope)),Float(_attention.scale_bits))));
        WN *mask=Read(_attention.mask,WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Row(),
            VHO_high_integer(_attention.length)),Token()));
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_NE(MTYPE_U4,mask,WN_Intconst(MTYPE_U4,0)),
            allowed,WN_CreateBlock()));
    }
    /* Every token expression is guarded before memory access on the last tile. */
    void Tokens(WN *body, WN *work) {
        WN *guard=WN_CreateBlock();
        WN_INSERT_BlockLast(guard,WN_CreateIf(WN_LT(MTYPE_I8,Token(),VHO_high_integer(_attention.length)),
            work,WN_CreateBlock()));
        WN_INSERT_BlockLast(body,Loop(_indices[3],_width,guard));
    }
    /* Emit the existing ordered query/value projection or final ABI copy. */
    void Operation(WN *body, unsigned ordinal) {
        const VHO_TENSOR_OPERATION &op=_program.operations[ordinal];
        const VHO_TENSOR_VALUE &output=_program.values[op.output];
        WN *work=WN_CreateBlock();
        WN *expression=Expression(op,work);
        Write(work,op.output,Output_index(output),expression);
        for(INT axis=(INT)output.shape.size()-1;axis>=0;--axis) {
            WN *outer=WN_CreateBlock();WN_INSERT_BlockLast(outer,Loop(_indices[axis],output.shape[axis],work));work=outer;
        }
        WN_INSERT_BlockLast(body,work);
    }
    /* Address a chunk's rank-sized normalized latent tuple. */
    WN *Numerator() { return WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Tuple(),VHO_high_integer(_attention.rank)),
        VHO_high_load(_indices[4])); }
    /* Produce [raw max, denominator, normalized latent] per disjoint chunk.
     * No score/probability vector is materialized. Fully masked/tail rows retain
     * zero mass and source-equivalent finite guards. Physical reductions are
     * intentionally deferred; these are serial semantic High folds. */
    WN *Chunks() {
        WN *chunk=WN_CreateBlock();
        WN_INSERT_BlockLast(chunk,VHO_high_store(_raw,Float(0xff800000)));
        WN *maximum=WN_CreateBlock();Score(maximum);Maximum(maximum,VHO_high_load(_score));Tokens(chunk,maximum);
        WN_INSERT_BlockLast(chunk,VHO_high_store(_safe,Safe_max(VHO_high_load(_raw))));
        WN_INSERT_BlockLast(chunk,VHO_high_store(_den,Float(0)));
        WN *zero=WN_CreateBlock();Write(zero,_partial_n,Numerator(),Float(0));
        WN_INSERT_BlockLast(chunk,Loop(_indices[4],_attention.rank,zero));
        WN *weights=WN_CreateBlock();Score(weights);
        WN_INSERT_BlockLast(weights,VHO_high_store(_weight,Math(false,WN_Sub(MTYPE_F4,
            VHO_high_load(_score),VHO_high_load(_safe)))));
        WN_INSERT_BlockLast(weights,VHO_high_store(_den,WN_Add(MTYPE_F4,
            VHO_high_load(_den),VHO_high_load(_weight))));
        WN *column=WN_CreateBlock();
        WN *cache_index=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,WN_Add(MTYPE_I8,
            WN_Mpy(MTYPE_I8,VHO_high_load(_indices[0]),VHO_high_integer(_attention.length)),Token()),
            VHO_high_integer(_attention.rank)),VHO_high_load(_indices[4]));
        Write(column,_partial_n,Numerator(),WN_Add(MTYPE_F4,Read(_partial_n,Numerator()),
            WN_Mpy(MTYPE_F4,VHO_high_load(_weight),Read(_attention.cache,cache_index))));
        WN_INSERT_BlockLast(weights,Loop(_indices[4],_attention.rank,column));Tokens(chunk,weights);
        WN *normalize=WN_CreateBlock();
        Write(normalize,_partial_n,Numerator(),WN_Div(MTYPE_F4,Read(_partial_n,Numerator()),Safe_den()));
        WN_INSERT_BlockLast(chunk,Loop(_indices[4],_attention.rank,normalize));
        if(_chunks>1) {
            Write(chunk,_partial_m,Tuple(),VHO_high_load(_raw));
            Write(chunk,_partial_d,Tuple(),VHO_high_load(_den));
        } else LSE(chunk);
        WN *body=WN_CreateBlock();WN_INSERT_BlockLast(body,Loop(_indices[2],_chunks,chunk));return body;
    }
    /* Source-equivalent output statistic for empty/NaN/positive mass rows. */
    void LSE(WN *body) {
        Write(body,_attention.lse,Row(),WN_Select(MTYPE_F4,WN_GT(MTYPE_F4,
            VHO_high_load(_den),Float(0)),WN_Add(MTYPE_F4,VHO_high_load(_safe),
                Math(true,Safe_den())),Float(0xff800000)));
    }
    /* Stable chunk merge: choose global safe max; compute guarded masses
     * d_c*exp(safe(m_c)-M), normalize once, then combine each latent component.
     * Empty chunks branch before exp; no 0*overflow can manufacture a mass. */
    void Merge(WN *body) {
        WN_INSERT_BlockLast(body,VHO_high_store(_raw,Float(0xff800000)));
        WN *maxima=WN_CreateBlock();Maximum(maxima,Read(_partial_m,Tuple()));
        WN_INSERT_BlockLast(body,Loop(_indices[2],_chunks,maxima));
        WN_INSERT_BlockLast(body,VHO_high_store(_safe,Safe_max(VHO_high_load(_raw))));
        WN_INSERT_BlockLast(body,VHO_high_store(_den,Float(0)));
        WN *masses=WN_CreateBlock(),*nonempty=WN_CreateBlock();
        WN_INSERT_BlockLast(masses,VHO_high_store(_weight,Float(0)));
        WN_INSERT_BlockLast(nonempty,VHO_high_store(_weight,WN_Mpy(MTYPE_F4,Read(_partial_d,Tuple()),
            Math(false,WN_Sub(MTYPE_F4,Safe_max(Read(_partial_m,Tuple())),VHO_high_load(_safe))))));
        WN_INSERT_BlockLast(masses,WN_CreateIf(WN_NE(MTYPE_F4,Read(_partial_d,Tuple()),Float(0)),
            nonempty,WN_CreateBlock()));
        Write(masses,_partial_d,Tuple(),VHO_high_load(_weight));
        WN_INSERT_BlockLast(masses,VHO_high_store(_den,WN_Add(MTYPE_F4,VHO_high_load(_den),VHO_high_load(_weight))));
        WN_INSERT_BlockLast(body,Loop(_indices[2],_chunks,masses));
        WN *column=WN_CreateBlock(),*fold=WN_CreateBlock();
        WN_INSERT_BlockLast(column,VHO_high_store(_sum,Float(0)));
        WN_INSERT_BlockLast(fold,VHO_high_store(_sum,WN_Add(MTYPE_F4,VHO_high_load(_sum),
            WN_Mpy(MTYPE_F4,Read(_partial_n,Numerator()),WN_Div(MTYPE_F4,Read(_partial_d,Tuple()),Safe_den())))));
        WN_INSERT_BlockLast(column,Loop(_indices[2],_chunks,fold));
        Write(column,_attention.latent,WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Row(),VHO_high_integer(_attention.rank)),
            VHO_high_load(_indices[4])),VHO_high_load(_sum));
        WN_INSERT_BlockLast(body,Loop(_indices[4],_attention.rank,column));LSE(body);
    }
    /* Bind a selected, preflighted program to existing storage ABI; stage one
     * body only. The LNO transaction owns tables, failure, commit and rollback. */
    WN *Selected_body(WN *scope) {
        Constants(scope);_selected=true;
        _attention=VHO_tensor_attention(_program);
        _width=_program.split_width?_program.split_width:_attention.length;
        _chunks=(_attention.length+_width-1)/_width;
        unsigned formal=0;
        for(unsigned i=0;i<_program.values.size();++i)
            if(_program.values[i].storage==i) _buffers[i]=WN_st(WN_formal(scope,formal++));
        for(unsigned i=0;i<16;++i) _indices.push_back(VHO_high_local("selected_axis",MTYPE_I8,_source));
        _reduction=VHO_high_local("selected_contribution",MTYPE_I8,_source);
        _sum=VHO_high_local("selected_sum",MTYPE_F4,_source);_next=VHO_high_local("selected_next",MTYPE_F4,_source);
        _raw=VHO_high_local("selected_raw_max",MTYPE_F4,_source);_safe=VHO_high_local("selected_safe_max",MTYPE_F4,_source);
        _den=VHO_high_local("selected_denominator",MTYPE_F4,_source);_dot=VHO_high_local("selected_dot",MTYPE_F4,_source);
        _rope=VHO_high_local("selected_rope",MTYPE_F4,_source);_score=VHO_high_local("selected_score",MTYPE_F4,_source);
        _weight=VHO_high_local("selected_weight",MTYPE_F4,_source);
        _partial_n=_chunks==1?_attention.latent:_program.operations[13].output;
        _partial_m=_program.operations[14].output;_partial_d=_program.operations[15].output;
        WN *body=WN_CreateBlock();
        std::ostringstream comment;
        comment<<"Selected High MLA: width="<<_program.split_width<<", policy="<<_program.chunk_policy
            <<", chunks="<<_chunks<<". Query/value projections remain materialized; score, softmax and weighted latent "
            <<"share one chunk-local algorithm boundary. Scalar High loops recompute scores for max and weight passes, "
            <<"retain F32 folds, finite-max and zero-denominator guards. No global score/probability vectors. "
            <<"Long contexts publish disjoint [raw maximum, denominator, normalized latent] tuples and merge with "
            <<"guarded rescaled masses. Existing private scratch buffers hold tuples within proven capacities. "
            <<"Warp ownership/SMEM/barriers are fixed plan obligations for later GPU mapping, not executed by this host view.";
        WN_INSERT_BlockLast(body,WN_CreateComment(comment.str().c_str()));
        Operation(body,1);
        WN *row=Chunks();if(_chunks>1) Merge(row);
        WN *head=WN_CreateBlock();WN_INSERT_BlockLast(head,Loop(_indices[1],_attention.heads,row));
        WN_INSERT_BlockLast(body,Loop(_indices[0],_attention.batch,head));
        Operation(body,37);Operation(body,46);Operation(body,47);
        WN_INSERT_BlockLast(body,WN_Create(OPR_RETURN,MTYPE_V,MTYPE_V,0));Position(body);return body;
    }
